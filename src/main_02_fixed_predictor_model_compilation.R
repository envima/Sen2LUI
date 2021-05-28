#' Compile models using a fixed set of predictor variables.
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
train_model <- TRUE
use_predictor_group <- c("sat")
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



### Get predictor information
predictors_sat <- enviLoad(file.path(root_folder, "data/results/info/predictors_sat_ffs.rds"))



### Get actual predictor dataset.
model_datasets <- lapply(use_predictor_group, function(g){
enviLoad(file.path(root_folder, "data/compiled_data/", paste0("model_data_explo_", g, ".rds")))
})
names(model_datasets) <- use_predictor_group

model_datasets$sat$meta$predictor_group_final <- predictors_sat$dat[predictors_sat$dat$Importance >= 10, "Predictors"]



### Train model(s)
if (train_model) {
  for(g in names(model_datasets)){
    model_data_explo <- model_datasets[[g]]$dat
    meta <- model_datasets[[g]]$meta
    compileModels(model_data_explo = model_data_explo, meta = meta, root_folder = root_folder,
                  ncors_compile_models = ncors_compile_models, ncors_ffsp = ncors_ffsp, use_ffs = FALSE)
  }
}
