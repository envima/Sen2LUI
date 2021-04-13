#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]
#'

library(envimaR)
if (Sys.info()[["nodename"]] == "MyComputer") {
  root_folder <- "D:/plygrnd/Sen2LUI/Sen2LUI"
} else {
  root_folder <- "D:/plygrnd/Sen2LUI/Sen2LUI"
}
source(file.path(root_folder, "src/functions/000_setup.R"))

# Note: For small projects, all code can go to this file except the main project setup script.
# For large projects, this file should only call other sub-control files and/or functions.
# To add a script using envimaR see ?envimaR::addScript.

### Compile dataset
sen2_plots <- compileDataset()


### Define settings
meta <- createMeta("Sen2LUI")
meta$years <- c("2018", "2019")
meta$predictors <- c("NDVI", "NDII", "SATVI", "REIP")
meta$explos <- c("Alb", "Hai", "Sch")
meta$jd_range <- c(90, 240)


### Compile predictors
meta_cols <- c(seq(1, grep("JD", names(sen2_plots[[1]][[1]]))[1]-1))
pid <- apply(expand.grid(meta$years, meta$explos, meta$predictors, c("mean", "sd")), 1, paste, collapse = "_")
psets <- lapply(pid, function(d) {
  print(paste0("Compiling predicor set: ", d))
  act <- compilePredictors(
    data = sen2_plots[[grep(substr(d, 1, 4), names(sen2_plots))]][[d]],
    info_year = substr(d, 1, 4), png_prefix = d
  )
  names(act$tp_info)[-meta_cols] <- paste(substr(d, (str_locate_all(pattern = "_", d)[[1]][2, 1] + 1), nchar(d)),
    names(act$tp_info)[-meta_cols],
    sep = "_"
  )
  return(act)
})
names(psets) <- pid

# Compile dataframe containig actual predictor variables
df <- lapply(psets, "[[", 1)
names(df) <- names(psets)

pvid <- apply(expand.grid(meta$years, meta$explos), 1, paste, collapse = "_")
df_cmb <- lapply(pvid, function(e) {
  Reduce(function(x, y) merge(x, y, all = TRUE), df[grep(e, names(df))])
})
names(df_cmb) <- pvid


### Define model and training data
meta$method <- "rf"
meta$act_model <- c("2018_Alb", "2019_Alb", "2018_Hai", "2019_Hai", "2018_Sch", "2019_Sch")
model_data <- Reduce(function(x, y) rbind(x, y), df_cmb[meta$act_model])
model_data <- model_data[complete.cases(model_data), ]
# model_data <- model_data[, -grep("jd", names(model_data))]
meta$model_rows <- nrow(model_data)

cl <- makeCluster(3)
registerDoParallel(cl)

set.seed(11081974)
space_var <- "Explo_Year"
folds <- CreateSpacetimeFolds(model_data, spacevar = space_var, k = 10)
meta$spacefolds <- unlist(lapply(folds$indexOut, function(f){
  unique(model_data[f, space_var])
}))
meta$model_rows

set.seed(11081974)
ffs_model <- ffs(model_data[, -meta_cols],
  model_data$LUI,
  method = meta$method,
  metric = "RMSE",
  trControl = trainControl(method = "cv", index = folds$index)
)

meta$model <- paste0("model_", paste(meta$act_model, collapse = "_"), "_", meta$method, ".rds")
enviSave(ffs_model, file = file.path("data/results/models/", meta$model), meta)

stopCluster(cl)

varImp(ffs_model)

alb <- df_cmb$`2019_Alb`[complete.cases(df_cmb$`2019_Alb`), ]
alb <- df_cmb$`2018_Alb`[complete.cases(df_cmb$`2018_Alb`), ]
alb <- rbind(df_cmb$`2018_Alb`[complete.cases(df_cmb$`2018_Alb`), ], df_cmb$`2019_Alb`[complete.cases(df_cmb$`2019_Alb`), ])
p2018alb_ndvi <- predict(ffs_model, alb)
plot(alb$LUI, p2018alb_ndvi)
summary(lm(p2018alb_ndvi ~ alb$LUI))


hai <- df_cmb$`2019_Hai`[complete.cases(df_cmb$`2019_Hai`), ]
hai <- df_cmb$`2018_Hai`[complete.cases(df_cmb$`2018_Hai`), ]
p2018hai_ndvi <- predict(ffs_model, hai)
plot(hai$LUI, p2018hai_ndvi)
summary(lm(p2018hai_ndvi ~ hai$LUI))

sch <- df_cmb$`2018_Sch`[complete.cases(df_cmb$`2018_Sch`), ]
sch <- df_cmb$`2019_Sch`[complete.cases(df_cmb$`2019_Sch`), ]
p2018sch_ndvi <- predict(ffs_model, sch)
plot(sch$LUI, p2018sch_ndvi)
summary(lm(p2018sch_ndvi ~ sch$LUI))


plot(model_data$LUI, ffs_model$finalModel$predicted)
summary(lm(ffs_model$finalModel$predicted ~ model_data$LUI))

