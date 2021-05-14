#' Train models.
#'
#' @description Compile model dataset from individual predictor data..
#'
#' @param ssets Satellite predictors.
#' @param msets Meteorological predictors.
#' @param meta Meta information dataset (initialized with envimaR::createMeta)
#' @param cor_cutoff Cut off correlation value for removing highly correlated predictors from final set.
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
compileModels <- function(model_data_explo, ncors) {
  cl <- makeCluster(ncors)
  registerDoParallel(cl)

  foreach(mde = seq(length(model_data_explo)), .packages = c("CAST", "caret", "doParallel", "envimaR")) %dopar% {
    for (i in seq(length(model_data_explo[[mde]]))) {
      cl <- makeCluster(ncors)
      registerDoParallel(cl)

      m <- model_data_explo[[mde]][[i]]
      meta$model_run <- names(model_data_explo[[mde]])[i]
      for (sv in space_var) {
        meta$space_var <- sv
        if (length(unique(m[, meta$space_var])) > 1) {
          print(meta$space_var)
          set.seed(11081974)
          folds <- CreateSpacetimeFolds(m, spacevar = meta$space_var, k = 10, seed = 11081974)
          meta$spacefolds <- unlist(lapply(folds$indexOut, function(f) {
            unique(m[f, meta$space_var])
          }))

          set.seed(11081974)
          ffs_model <- ffs(m[, meta$predictor_group_final],
                           m$LUI,
                           method = meta$method,
                           metric = "RMSE",
                           seed = 11081974,
                           withinSE = FALSE,
                           trControl = trainControl(method = "cv", index = folds$index)
          )

          meta$model <- paste0(
            "model_", format(Sys.time(), "%Y%m%d_%H%M%S_"),
            paste(meta$model_dataset, collapse = "_"), "_", meta$method, ".rds"
          )
          enviSave(ffs_model, file = file.path(root_folder, "data/results/models/", meta$model), meta)
        }
        gc()
      }
      stopCluster(cl)
    }
  }
  stopCluster(cl)
}
