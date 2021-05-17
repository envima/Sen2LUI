#' Compile key features from irregulary sampled Sentinel-2 observations.
#'
#' @description A gam is used to compile a continous temporal signal (1 day resolution) from irregular Sentinel 2
#' observations. Key features of the continous signal are extracted.
#'
#' @param satellite_plots Satellite dataset with one study plot per row and columns as julian days.
#' @param meteorological_plots Meteorological dataset with one study plot per row and columns as julian days.
#' @param meta Meta information dataset (initialized with envimaR::createMeta)
#' @param root_folder Path to folder for saving pngs. The pngs are not required and have just an informative purpose
#' If NULL, no pngs are saved.
#'
#' @return List containing the satellite and meteorological predictors.
#'
#' @details
#'
#' @name compilePredictors
#'
#' @examples
#' \dontrun{
#'
#' }
#'
compilePredictors <- function(satellite_plots, meteorological_plots, meta, root_folder = NULL) {
  cols_meta <- c(seq(1, grep("JD", names(satellite_plots[[1]][[1]]))[1] - 1))
  meta$cols_meta <- names(satellite_plots[[1]][[1]])[cols_meta]
  meta$pid <- apply(expand.grid(meta$years, meta$explos, meta$predictors, c("mean", "sd")), 1, paste, collapse = "_")

  ssets <- lapply(meta$pid, function(d) {
    print(paste0("Compiling predicor set: ", d))
    act <- smoothPredictors(
      data = satellite_plots[[grep(substr(d, 1, 4), names(satellite_plots))]][[d]],
      info_year = substr(d, 1, 4), jd_start = meta$jd_range[1], jd_end = meta$jd_range[2], met = FALSE,
      root_folder = root_folder, png_prefix = d
    )
    if (!is.null(act$tp_info)) {
      names(act$tp_info)[-cols_meta] <- paste(substr(
        d, (str_locate_all(pattern = "_", d)[[1]][2, 1] + 1),
        nchar(d)
      ), names(act$tp_info)[-cols_meta], sep = "_")
    }
    return(act)
  })
  names(ssets) <- meta$pid
  meta$smoothing <- collectMetaSmoothing(data = ssets)

  meta$met_pid <- apply(expand.grid(meta$years, meta$explos, meta$met_predictors), 1, paste, collapse = "_")
  msets <- lapply(meta$met_pid, function(d) {
    print(paste0("Compiling predicor set: ", d))
    act <- smoothPredictors(
      data = meteorological_plots[[grep(substr(d, 1, 4), names(meteorological_plots))]][[d]],
      info_year = substr(d, 1, 4), jd_start = meta$jd_range[1], jd_end = meta$jd_range[2], met = TRUE,
      root_folder = root_folder, png_prefix = d
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
  meta$met_smoothing <- collectMetaSmoothing(data = msets)

  return(list(ssets = ssets, msets = msets, meta = meta))
}
