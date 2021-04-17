#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]
#'

### Set evnironment
library(envimaR)
if (Sys.info()[["nodename"]] == "MyComputer") {
  root_folder <- "D:/plygrnd/Sen2LUI/Sen2LUI"
} else {
  root_folder <- "/mnt/sd19006/tnauss/Sen2LUI"
}
source(file.path(root_folder, "src/functions/000_setup.R"))



### Define settings
compute <- FALSE
meta <- createMeta("Sen2LUI")
meta$explos <- c("Alb", "Hai", "Sch")
meta$years <- c("2017", "2018", "2019")
meta$jd_range <- c(90, 300)
meta$predictors <- c("NDVI", "REIP", "DSWI", "MCARI", "NDII", "SATVI", "B12")
meta$model_dataset <- c(
  "2017_Alb", "2018_Alb", "2019_Alb",
  "2017_Hai", "2018_Hai", "2019_Hai",
  "2017_Sch", "2018_Sch", "2019_Sch"
)
meta$method <- "cubist"
space_var <- c("Explo_Year", "Year", "Explo")



### Compile dataset
if (compute) {
  sen2_plots <- compileDataset(compile_sd = meta$predictors)
  enviSave(sen2_plots, file.path("data/compiled_data/", "sen2_plots.rds"), meta = meta)
} else {
  sen2_plots <- enviLoad(file.path("data/compiled_data/", "sen2_plots.rds"))
}



### Compile predictors
# Compile predictor dataset containing actual variables and additional information
meta$cols_meta <- c(seq(1, grep("JD", names(sen2_plots[[1]][[1]]))[1] - 1))
meta$pid <- apply(expand.grid(meta$years, meta$explos, meta$predictors, c("mean", "sd")), 1, paste, collapse = "_")
if (compute) {
  psets <- lapply(meta$pid, function(d) {
    print(paste0("Compiling predicor set: ", d))
    act <- compilePredictors(
      data = sen2_plots[[grep(substr(d, 1, 4), names(sen2_plots))]][[d]],
      info_year = substr(d, 1, 4), jd_start = meta$jd_range[1], jd_end = meta$jd_range[2], png_prefix = d
    )
    if (!is.null(act$tp_info)) {
      names(act$tp_info)[-meta$cols_meta] <- paste(substr(d, (str_locate_all(pattern = "_", d)[[1]][2, 1] + 1),
                                                          nchar(d)), names(act$tp_info)[-meta$cols_meta], sep = "_"
      )
    }
    return(act)
  })
  names(psets) <- meta$pid
  enviSave(psets, file.path("data/compiled_data/", "psets.rds"), meta = meta)
} else {
  psets <- enviLoad(file.path("data/compiled_data/", "psets.rds"))
}

# Extract actual predictor variables from the overall predictor dataset.
if (compute) {
  df <- lapply(psets, "[[", 1)
  names(df) <- names(psets)

  meta$pvid <- apply(expand.grid(meta$years, meta$explos), 1, paste, collapse = "_")
  df_cmb <- lapply(meta$pvid, function(e) {
    Reduce(function(x, y) merge(x, y, all = TRUE), df[grep(e, names(df))])
  })
  names(df_cmb) <- meta$pvid
  enviSave(df_cmb, file.path("data/compiled_data/", "df_cmb.rds"), meta = meta)
} else {
  df_cmb <- enviLoad(file.path("data/compiled_data/", "df_cmb.rds"))
}



### Compile model dataset
model_data <- Reduce(function(x, y) rbind(x, y), df_cmb[meta$model_dataset])
model_data <- model_data[complete.cases(model_data), ]
# model_data <- model_data[, -grep("jd", names(model_data))]
meta$model_rows <- nrow(model_data)
meta$correlated_predictors <- findCorrelation(model_data[, -meta$cols_meta], cutoff = 0.99, names = TRUE, exact = FALSE)
meta$predictor_group_final <- colnames(model_data)[!colnames(model_data) %in% meta$correlated_predictors]


### Save metadata and free memory
rm(sen2_plots, psets, df, df_cmb)
gc()



### Train model(s)
cl <- makeCluster(39)
registerDoParallel(cl)

for (sv in space_var) {
  meta$space_var <- sv
  set.seed(11081974)
  folds <- CreateSpacetimeFolds(model_data, spacevar = meta$space_var, k = 10)
  meta$spacefolds <- unlist(lapply(folds$indexOut, function(f) {
    unique(model_data[f, meta$space_var])
  }))

  set.seed(11081974)
  ffs_model <- ffs(model_data[, meta$predictor_group_final],
    model_data$LUI,
    method = meta$method,
    metric = "RMSE",
    trControl = trainControl(method = "cv", index = folds$index)
  )

  meta$model <- paste0(
    "model_", format(Sys.time(), "%Y%m%d_%H%M%S_"),
    paste(meta$model_dataset, collapse = "_"), "_", meta$method, ".rds"
  )
  enviSave(ffs_model, file = file.path("data/results/models/", meta$model), meta)
}

stopCluster(cl)
