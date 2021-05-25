#' Compile models using a forward feature selection approach.
#'
#' @description Predict land use intensity from satellite and environmental observations.
#'
#'

### Set evnironment
library(envimaR)
if (Sys.info()[["nodename"]] == "PC19616") {
  root_folder <- "D:/plygrnd/Sen2LUI/Sen2LUI"
  ncors_compile_models <- 2
  ncors_ffsp <- 4
} else {
  root_folder <- "~/plygrnd/Sen2LUI"
  ncors_compile_models <- 2
  ncors_ffsp <- 20
}
source(file.path(root_folder, "src/functions/000_setup.R"))



### Define settings
compute <- TRUE
train_model <- TRUE
use_predictor_group <- c("sat", "met", "sat_met")
meta <- createMeta("Sen2LUI")
meta$explos <- c("Alb", "Hai", "Sch")
meta$years <- c("2017", "2018", "2019")
meta$jd_range <- c(90, 300)
meta$predictors <- c("NDVI", "REIP", "DSWI", "MCARI", "NDII", "SATVI", "B12")
meta$met_predictors <- c("Ta_200", "precipitation_radolan")
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
  model_datasets <- lapply(use_predictor_group, function(g){
    cmd <- compileModelDataset(ssets = ssets, msets = msets, meta = meta, act_predictor_group = g, cor_cutoff = 0.95)
    enviSave(cmd$model_data_explo, file.path(root_folder, "data/compiled_data/",
                                             paste0("model_data_explo_", meta$predictor_group, ".rds")), meta = meta)
    return(list(dat = cmd$model_data_explo, meta = cmd$meta))
  })
  names(model_datasets) <- use_predictor_group
} else {
  model_datasets <- lapply(use_predictor_group, function(g){
  enviLoad(file.path(root_folder, "data/compiled_data/", paste0("model_data_explo_", g, ".rds")))
  })
  names(model_datasets) <- use_predictor_group
}



### Free memory
rm(sen2_plots, met_plots, ssets, msets)
gc()



### Train model(s)
if (train_model) {
  for(g in names(model_datasets)){
    model_data_explo <- model_datasets[[g]]$dat
    meta <- model_datasets[[g]]$meta
    compileModels(model_data_explo = model_data_explo, meta = meta, root_folder = root_folder,
                  ncors_compile_models = ncors_compile_models, ncors_ffsp = ncors_ffsp)
  }
}
