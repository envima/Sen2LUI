#' Main control script
#'
#' @description Predict land use intensity from satellite and environmental observations.
#'
#'

### Set evnironment
library(envimaR)
if (Sys.info()[["nodename"]] == "PC19616") {
  root_folder <- "D:/plygrnd/Sen2LUI/Sen2LUI"
  ncors <- 2
} else {
  root_folder <- "~/plygrnd/Sen2LUI"
  ncors <- 4
}
source(file.path(root_folder, "src/functions/000_setup.R"))



### Define settings
compute <- FALSE
train_model <- TRUE
meta <- createMeta("Sen2LUI")
meta$explos <- c("Alb", "Hai", "Sch")
meta$years <- c("2017", "2018", "2019")
meta$jd_range <- c(90, 300)
meta$predictors <- c("NDVI", "REIP", "DSWI", "MCARI", "NDII", "SATVI", "B12")
meta$met_predictors <- c("Ta_200", "precipitation_radolan")
meta$use_met_predictory <- FALSE
meta$model_dataset <- c(
  "2017_Alb", "2018_Alb", "2019_Alb",
  "2017_Hai", "2018_Hai", "2019_Hai",
  "2017_Sch", "2018_Sch", "2019_Sch"
)
meta$method <- "cubist"
meta$space_vars <- c("Explo_Year", "Year", "Explo")



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
if (compute) {
  cp <- compilePredictors(
    satellite_plots = sen2_plots, meteorological_plots = met_plots,
    meta = meta, root_folder = root_folder
  )
  ssets <- cp$ssets
  msets <- cp$msets
  meta <- cp$meta
  rm(cp)
  enviSave(ssets, file.path(root_folder, "data/compiled_data/", "ssets.rds"), meta = meta)
  enviSave(msets, file.path(root_folder, "data/compiled_data/", "msets.rds"), meta = meta)
} else {
  ssets <- enviLoad(file.path(root_folder, "data/compiled_data/", "ssets.rds"))$dat
  msets <- enviLoad(file.path(root_folder, "data/compiled_data/", "msets.rds"))$dat
  meta <- enviLoad(file.path(root_folder, "data/compiled_data/", "msets.rds"))$meta
}



### Extract actual predictor variables from the overall predictor dataset.
if (compute) {
  cmd <- compileModelDataset(ssets = ssets, msets = msets, meta = meta, cor_cutoff = 0.95)
  model_data_explo <- cmd$model_data_explo
  meta <- cmd$meta
  enviSave(cmd$model_data_explo, file.path(root_folder, "data/compiled_data/", "model_data_explo.rds"), meta = meta)
  rm(cmd)
} else {
  model_data_explo <- enviLoad(file.path(root_folder, "data/compiled_data/", "model_data_explo.rds"))$dat
  meta <- enviLoad(file.path(root_folder, "data/compiled_data/", "model_data_explo.rds"))$meta
}



### Free memory
rm(sen2_plots, met_plots, ssets, msets)
gc()



### Train model(s)
if (train_model) {
  compileModels(model_data_explo = model_data_explo, meta = meta, root_folder = root_folder, ncors = ncors)
}
