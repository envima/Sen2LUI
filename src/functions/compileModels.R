#' Train models.
#'
#' @description Compile model dataset from individual predictor data..
#'
#' @param ssets Satellite predictors.
#' @param msets Meteorological predictors.
#' @param meta Meta information dataset (initialized with envimaR::createMeta)
#' @param cor_cutoff Cut off correlation value for removing highly correlated predictors from final set.
#' @param root_folder Path to folder for saving pngs. The pngs are not required and have just an informative purpose
#' @param ncors Number of cores to be used.
#'
#' @return Model dataset.
#'
#' @details
#'
#' @name compileModels
#'
#' @examples
#' \dontrun{
#'
#' }
#'
compileModels <- function(model_data_explo, meta, root_folder, ncors_compile_models, ncors_ffsp) {
  cl_ncors_compile_models <- makeCluster(ncors_compile_models)
  registerDoParallel(cl_ncors_compile_models)

  foreach(
    mde = seq(length(model_data_explo)), errorhandling = "stop",
    .packages = c("CAST", "caret", "doParallel", "envimaR"), .export = c("trainActualModel", "ffsp")
  ) %dopar% {
    for (i in seq(length(model_data_explo[[mde]]))) {

      m <- model_data_explo[[mde]][[i]]
      meta$model_run <- names(model_data_explo[[mde]])[i]
      if (grepl("ALL", meta$model_run)) {
        space_vars <- meta$space_vars
      } else {
        space_vars <- meta$space_vars[!grepl("Explo", meta$space_vars)]
      }
      for (sv in space_vars) {

        trainActualModel(m = m, meta = meta, sv = sv, root_folder = root_folder, ncors_ffsp = ncors_ffsp)
        gc()
      }
    }
  }
  stopCluster(cl_ncors_compile_models)
}
