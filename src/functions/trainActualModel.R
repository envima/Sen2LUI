#' Train actual model.
#'
#' @description Compile actual model dataset from individual predictor data.
#'
#' @param m Actual model dataset.
#' @param meta Meta information dataset (initialized with envimaR::createMeta)
#' @param sv Actual space variable used for creating the training/test folds.
#' @param root_folder Path to folder for saving pngs. The pngs are not required and have just an informative purpose
#' @param ncors_ffsp Cores to be used in ffs.
#'
#' @return Nothing.
#'
#' @details Save trained model to disk.
#'
#' @name trainActualModel
#'
#' @examples
#' \dontrun{
#'
#' }
#'
trainActualModel <- function(m, meta, sv, root_folder, ncors_ffsp) {
  meta$space_var <- sv
  if (length(unique(m[, meta$space_var])) > 1) {
    print(meta$space_var)
    set.seed(11081974)
    folds <- CreateSpacetimeFolds(m, spacevar = meta$space_var, k = 10, seed = 11081974)
    meta$spacefolds <- unlist(lapply(folds$indexOut, function(f) {
      unique(m[f, meta$space_var])
    }))

    log_info <- meta
    log_info$name <- paste0(
      "model_", format(Sys.time(), "%Y%m%d_%H%M%S_"),
      paste(meta$model_dataset, collapse = "_"), "_", meta$method, "_", meta$predictor_group
    )
    log_info$mde <- mde
    log_info$i <- i
    yaml::write_yaml(log_info, file.path(root_folder, "data/tmp/", paste0(log_info$name, ".yaml")))

    set.seed(11081974)

    ffs_model <- tryCatch(ffsp(
      predictors = m[, meta$predictor_group_final],
      response = m$LUI,
      method = meta$method,
      metric = "RMSE",
      seed = 11081974,
      withinSE = FALSE,
      trControl = trainControl(method = "cv", index = folds$index),
      ncors = ncors_ffsp
    ),
    error = function(e) e
    )

    meta$model <- paste0(
      "model_", format(Sys.time(), "%Y%m%d_%H%M%S_"),
      paste(meta$model_dataset, collapse = "_"), "_", meta$method, "_", meta$predictor_group, ".rds"
    )

    if (any(class(ffs_model) == "train")) {
      enviSave(ffs_model, file = file.path(root_folder, "data/results/models/", meta$model), meta)
    } else {
      meta <- c(meta, log_info)
      meta$model <- paste0("error_", meta$model)
      enviSave(ffs_model, file = file.path(root_folder, "data/results/models/", meta$model), meta)
    }
  }
}
