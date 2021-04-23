#' Collect some meta data.
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]
#'
#' @param
#'
#' @return
#'
#' @details
#'
#' @name compileDataset
#'
#' @examples
#' \dontrun{
#'
#' }
#'
collectMetaSmoothing <- function(data) {
  tmp <- lapply(data, "[[", 2)
  smoothing <- lapply(seq(length(tmp)), function(i) {
    smoothing <- NULL
    p <- compact(tmp[[i]])
    if (!is_empty(p)) {
      smoothing <- lapply(seq(length(p)), function(j) {
        data.frame(
          predictor = paste(names(tmp[i]), names(p[j]), sep = "_"),
          gam_smooth_term = unique(p[[j]]$tp_smooth_term)
        )
      })
      return(do.call("rbind", smoothing))
    }
    return(smoothing)
  })
  meta_smoothing <- do.call("rbind", smoothing)
  return(meta_smoothing)
}
