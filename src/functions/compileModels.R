#' Train models.
#'
#' @description Compile model dataset from individual predictor data..
#'
#' @param ssets Satellite predictors.
#' @param msets Meteorological predictors.
#' @param meta Meta information dataset (initialized with envimaR::createMeta)
#' @param cor_cutoff Cut off correlation value for removing highly correlated predictors from final set.
#' @param root_folder Path to folder for saving pngs. The pngs are not required and have just an informative purpose
#' @param ncors_compile_models Number of cores to be used within this function.
#' @param ncors_ffsp Number of cores to be used within model training.
#' @param use_ffs Use forward feature selection (otherwise just train the model with all predictor variables)
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
compileModels <- function(model_data_explo, meta, root_folder, ncors_compile_models, ncors_ffsp, use_ffs = TRUE) {
  print(use_ffs)
  cl_ncors_compile_models <- makeCluster(ncors_compile_models,
    outfile = file.path(root_folder, "/data/tmp/ncors_compile_models.log")
  )
  registerDoParallel(cl_ncors_compile_models)

  foreach(
    mde = seq(length(model_data_explo)), errorhandling = "stop",
    .packages = c("CAST", "caret", "doParallel", "envimaR"), .export = c("trainActualModel", "ffsp")
  ) %dopar% {
    for (i in seq(length(model_data_explo[[mde]]))) {
      m <- model_data_explo[[mde]][[i]]
      meta$model_run <- names(model_data_explo[[mde]])[i]
      meta$mde <- mde
      meta$i <- i
      if (grepl("ALL", meta$model_run)) {
        space_vars <- meta$space_vars
      } else {
        space_vars <- meta$space_vars[!grepl("Explo", meta$space_vars)]
      }
      for (sv in space_vars) {
        tryCatch(trainActualModel(
          m = m, meta = meta, sv = sv, root_folder = root_folder, ncors_ffsp = ncors_ffsp,
          use_ffs = use_ffs
        ),
        error = function(e) e, finally = print(paste("compileModels", mde, i, meta$model_run, sep = "_"))
        )
        gc()
      }
    }
  }

  stopCluster(cl_ncors_compile_models)
}
