#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]
#'

### Set evnironment
library(envimaR)
if (Sys.info()[["nodename"]] == "PC19616") {
  root_folder <- "D:/plygrnd/Sen2LUI/Sen2LUI"
} else {
  root_folder <- "~/plygrnd/Sen2LUI"
}
source(file.path(root_folder, "src/functions/000_setup.R"))



### Define settings
compute <- TRUE
meta <- createMeta("Sen2LUI")
meta$explos <- c("Alb", "Hai", "Sch")
meta$years <- c("2017", "2018", "2019")
meta$jd_range <- c(90, 300)
meta$predictors <- c("NDVI", "REIP", "DSWI", "MCARI", "NDII", "SATVI", "B12")
meta$met_predictors <- c("Ta_200", "precipitation_radolan")
meta$use_met_predictory <- TRUE
meta$model_dataset <- c(
  "2017_Alb", "2018_Alb", "2019_Alb",
  "2017_Hai", "2018_Hai", "2019_Hai",
  "2017_Sch", "2018_Sch", "2019_Sch"
)
meta$method <- "cubist"
space_var <- c("Explo_Year", "Year", "Explo")



### Compile dataset
# Sentinel 2
if (compute) {
  sen2_plots <- compileDataset(root_folder = root_folder, compile_sd = meta$predictors)
  enviSave(sen2_plots, file.path(root_folder, "data/compiled_data/", "sen2_plots.rds"), meta = meta)
} else {
  sen2_plots <- enviLoad(file.path(root_folder, "data/compiled_data/", "sen2_plots.rds"))$dat
}

# Climate stations
if (compute) {
  met_plots <- compileMetDataset(root_folder, met_pars = meta$met_predictors, jd_range = c(90, 300))
  enviSave(met_plots, file.path(root_folder, "data/compiled_data/", "met_plots.rds"), meta = meta)
} else {
  met_plots <- enviLoad(file.path(root_folder, "data/compiled_data/", "met_plots.rds"))$dat
}



### Compile predictors
# Compile predictor dataset containing actual variables and additional information
cols_meta <- c(seq(1, grep("JD", names(sen2_plots[[1]][[1]]))[1] - 1))
meta$cols_meta <- names(sen2_plots[[1]][[1]])[cols_meta]
meta$pid <- apply(expand.grid(meta$years, meta$explos, meta$predictors, c("mean", "sd")), 1, paste, collapse = "_")
if (compute) {
  psets <- lapply(meta$pid, function(d) {
    print(paste0("Compiling predicor set: ", d))
    act <- compilePredictors(
      data = sen2_plots[[grep(substr(d, 1, 4), names(sen2_plots))]][[d]],
      info_year = substr(d, 1, 4), jd_start = meta$jd_range[1], jd_end = meta$jd_range[2], root_folder = root_folder,
      png_prefix = d
    )
    if (!is.null(act$tp_info)) {
      names(act$tp_info)[-cols_meta] <- paste(substr(
        d, (str_locate_all(pattern = "_", d)[[1]][2, 1] + 1),
        nchar(d)
      ), names(act$tp_info)[-cols_meta], sep = "_")
    }
    return(act)
  })
  names(psets) <- meta$pid
  enviSave(psets, file.path(root_folder, "data/compiled_data/", "psets.rds"), meta = meta)

  meta$met_pid <- apply(expand.grid(meta$years, meta$explos, meta$met_predictors), 1, paste, collapse = "_")
  msets <- lapply(meta$met_pid, function(d) {
    print(paste0("Compiling predicor set: ", d))
    act <- compilePredictors(
      data = met_plots[[grep(substr(d, 1, 4), names(met_plots))]][[d]],
      info_year = substr(d, 1, 4), jd_start = meta$jd_range[1], jd_end = meta$jd_range[2], root_folder = root_folder,
      png_prefix = d
    )
    if (!is.null(act$tp_info)) {
      names(act$tp_info)[-cols_meta] <- paste(substr(
        d, (str_locate_all(pattern = "_", d)[[1]][2, 1] + 1),
        nchar(d)
      ), names(act$tp_info)[-cols_meta], sep = "_")
    }
    return(act)
  })
  names(msets) <- meta$met_pid
  enviSave(msets, file.path(root_folder, "data/compiled_data/", "msets.rds"), meta = meta)
} else {
  psets <- enviLoad(file.path(root_folder, "data/compiled_data/", "psets.rds"))$dat
  msets <- enviLoad(file.path(root_folder, "data/compiled_data/", "msets.rds"))$dat
}

# Collect some meta information
meta$smoothing <- collectMetaSmoothing(data = psets)
meta$met_smoothing <- collectMetaSmoothing(data = msets)



# Extract actual predictor variables from the overall predictor dataset.
if (compute) {
  df <- lapply(psets, "[[", 1)
  names(df) <- names(psets)

  df_met <- lapply(msets, "[[", 1)
  names(df_met) <- names(msets)

  meta$pvid <- apply(expand.grid(meta$years, meta$explos), 1, paste, collapse = "_")
  df_cmb <- lapply(meta$pvid, function(e) {
    tmp <- Reduce(function(x, y) merge(x, y, all = TRUE), df[grep(e, names(df))])
    tmp_met <- Reduce(function(x, y) merge(x, y, all = TRUE), df_met[grep(e, names(df_met))])
    return(merge(tmp, tmp_met))
  })
  names(df_cmb) <- meta$pvid
  enviSave(df_cmb, file.path(root_folder, "data/compiled_data/", "df_cmb.rds"), meta = meta)
} else {
  df_cmb <- enviLoad(file.path(root_folder, "data/compiled_data/", "df_cmb.rds"))$dat
}

### Compile model dataset
model_data <- Reduce(function(x, y) rbind(x, y), df_cmb[meta$model_dataset])
model_data <- model_data[complete.cases(model_data), ]
# model_data <- model_data[, -grep("jd", names(model_data))]
meta$model_rows <- nrow(model_data)
# meta$correlated_predictors <- findCorrelation(model_data[, -which(names(model_data) %in% meta$cols_meta)],
#   cutoff = 0.99, names = TRUE, exact = FALSE
# )
# meta$predictor_group_final <- colnames(model_data)[!colnames(model_data) %in%
  # c(meta$cols_meta, meta$correlated_predictors)]
if(meta$use_met_predictory == FALSE){
  meta$cols_meta <- c(meta$cols_meta, meta$met_predictors)
}
meta$predictor_group_final <- colnames(model_data)[!colnames(model_data) %in% meta$cols_meta]



### Save metadata and free memory
rm(sen2_plots, psets, df, df_cmb)
gc()



### Split data frame by exploratories
model_data_explo <- lapply(unique(model_data$Explo), function(e){
  model_data[model_data$Explo == e, ]
})
names(model_data_explo) <- unique(model_data$Explo)
model_data_explo <- c(list(model_data), model_data_explo)
names(model_data_explo)[1] <- "ALL"



### Train model(s)
cl <- makeCluster(39)
registerDoParallel(cl)

for(i in seq(length(model_data_explo))){
  m = model_data_explo[[i]]
  meta$model_run = names(model_data_explo)[i]
  for (sv in space_var) {
    meta$model_run =
    meta$space_var <- sv
    if(length(unique(m[, meta$space_var])) > 1){
      print(meta$space_var)
      set.seed(11081974)
      folds <- CreateSpacetimeFolds(m, spacevar = meta$space_var, k = 10, seed = 11081974)
      meta$spacefolds <- unlist(lapply(folds$indexOut, function(f) {
        unique(m[f, meta$space_var])
      }))

      set.seed(11081974)
      # ffs_model <- ffs(m[, meta$predictor_group_final],
      #                  m$LUI,
      #                  method = meta$method,
      #                  metric = "RMSE",
      #                  seed = 11081974,
      #                  withinSE = FALSE,
      #                  trControl = trainControl(method = "cv", index = folds$index)
      # )
      #
      # meta$model <- paste0(
      #   "model_", format(Sys.time(), "%Y%m%d_%H%M%S_"),
      #   paste(meta$model_dataset, collapse = "_"), "_", meta$method, ".rds"
      # )
      # enviSave(ffs_model, file = file.path(root_folder, "data/results/models/", meta$model), meta)
    }
  }
}

stopCluster(cl)


# model_files <- list.files(file.path(root_folder, "data/results/models/"), pattern = glob2rx("model_202104*.rds"),
#                           full.names = TRUE)
# models <- lapply(model_files, function(m){
#   enviLoad(m)
#   })
#
#
#
#
#
#
#
# for(m in models){
#   print(data.frame(m$meta$use_met_predictory))
#     print(data.frame(m$meta$use_met_predictory))
#     print(m$dat$selectedvars)
#     print(data.frame(m$dat$resample[order(m$dat$resample$Resample),"Rsquared"], m$meta$spacefolds))
#
# }
#
#
# for (sv in space_var) {
#   meta$space_var <- sv
#   meta$space_var <- space_var[[1]]
#   if(length(unique(model_data_explo[[1]][, meta$space_var])) > 1){
#     set.seed(11081974)
#     folds <- CreateSpacetimeFolds(model_data_explo[[1]], spacevar = meta$space_var, k = 10, seed = 11081974)
#     meta$spacefolds <- unlist(lapply(folds$indexOut, function(f) {
#       unique(model_data_explo[[1]][f, meta$space_var])
#     }))
#
#   }
#
# }

