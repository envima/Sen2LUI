#' Smooth predictor variables.
#'
#' @description A gam is used to compile a continous temporal signal (1 day resolution) from irregular Sentinel 2
#' observations. Key features of the continous signal are extracted.
#'
#' @param data Dataset with one study plot per row and columns as julian days.
#' @param info_year Metadata providing the year of the observation
#' @param jd_start Start day of the compiled continous time series (not the starting day of the observations)
#' @param jd_end End day of the compiled continous time series (not the last day of the observations)
#' @param png_prefix Prefix of png graphics to be saved. The pngs are not required and have just an informative purpose.
#' If NULL, no pngs are saved.
#'
#' @return List containing meta information on smoothing and smoothed variables.
#'
#' @details
#'
#' @name smoothPredictors
#'
#' @examples
#' \dontrun{
#'
#' }
#'
smoothPredictors <- function(data, info_year, jd_start, jd_end, met = FALSE, root_folder = NULL, png_prefix = NULL) {
  jds <- seq(jd_start, jd_end)

  tp_data <- lapply(seq(nrow(data)), function(p) {
    col_ids <- grep("JD", colnames(data))
    act_df <- data.frame(
      d = as.numeric(sub("JD", "", colnames(data)[col_ids])),
      v = as.numeric(data[p, col_ids])
    )
    return_list <- list(tp_detail = NULL, tp_info = NULL, gm_model = NULL)

    # ggplot(data = act_df, aes(x = d, y = v)) + geom_point() + geom_smooth(method = "gam")

    if ((nrow(act_df) - 10) >= sum(is.na(act_df$v))) {
      if(met == FALSE){
        set.seed(11081974)
        gm <- gam(v ~ s(d, k = -1, bs = "cr"), data = act_df)
        # coef(gm)
        gm_pred <- predict(gm, data.frame(d = jds))
        ts_y <- ts(as.numeric(gm_pred), start = jd_start, end = jd_end)
        tp <- turnpoints(ts_y)
        smooth_term <- 's(d, k = -1, bs = "cr")'

        # If standard smooth term approach leads to 0 turning points, use cyclic cubic regression splines instead to
        # get a fit which is not linear.
        if (tp$nturns == 0) {
          set.seed(11081974)
          gm <- gam(v ~ s(d, k = -1, bs = "cc"), data = act_df)
          gm_pred <- predict(gm, data.frame(d = jds))
          ts_y <- ts(as.numeric(gm_pred), start = jd_start, end = jd_end)
          tp <- turnpoints(ts_y)
          smooth_term <- 's(d, k = -1, bs = "cc")'
        }
      } else {
        gm <- NULL
        gm_pred <- as.numeric(act_df[act_df$d >= jd_start & act_df$d <= jd_end, "v"])
        ts_y <- ts(gm_pred, start = jd_start, end = jd_end)
        tp <- turnpoints(ts_y)
        smooth_term <- 'original data series'
      }


      if (tp$firstispeak == FALSE | is.na(tp$firstispeak)) {
        typep <- c("pit", "peak")
      } else {
        typep <- c("peak", "pit")
      }

      if (tp$nturns > 0) {
        tp_detail <- data.frame(
          tp_jd = jds[tp$tppos],
          tp_values = gm_pred[tp$tppos],
          tp_type = rep(typep, length.out = tp$nturns),
          tp_proba = tp$proba,
          tp_info = tp$info,
          tp_smooth_term = smooth_term
        )
        tp_info <- data.frame(
          plotID = data$plotID[p],
          LUI = data$LUI[p],
          Year = info_year,
          Explo = data$Explo[p],
          Explo_Year = data$Explo_Year[p],
          gam_q000_val = quantile(gm_pred, probs = 0),
          gam_q025_val = quantile(gm_pred, probs = 0.25),
          gam_q050_val = quantile(gm_pred, probs = 0.50),
          gam_q075_val = quantile(gm_pred, probs = 0.75),
          gam_q100_val = quantile(gm_pred, probs = 1),
          gam_mean_val = mean(gm_pred),
          gam_sd_val = sd(gm_pred),
          gam_auc = sum(diff(jds) * (head(gm_pred, -1) + tail(gm_pred, -1))) / 2,
          tp_turns = tp$nturns,
          tp_peaks = sum(tp$peaks),
          tp_pits = sum(tp$pits),
          tp_1st_max_jd = ifelse(sum(tp$peaks) == 0, -1, jds[which(tp$peaks)[1]]),
          tp_max_jd = tp_detail$tp_jd[which.max(tp_detail$tp_values)],
          tp_min_jd = tp_detail$tp_jd[which.min(tp_detail$tp_values)],
          tp_max_value = ifelse(sum(tp$peaks) == 0, -1, max(tp_detail$tp_values[tp_detail$tp_type == "peak"])),
          tp_mean_max_values = ifelse(sum(tp$peaks) == 0, -1, mean(tp_detail$tp_values[tp_detail$tp_type == "peak"])),
          tp_sd_max_values = ifelse(sum(tp$peaks) > 1, sd(tp_detail$tp_values[tp_detail$tp_type == "peak"]), 0),
          tp_min_value = ifelse(sum(tp$pits) == 0, -1, min(tp_detail$tp_values[tp_detail$tp_type == "pit"])),
          tp_mean_min_values = ifelse(sum(tp$pits) == 0, -1, mean(tp_detail$tp_values[tp_detail$tp_type == "pit"])),
          tp_sd_min_values = ifelse(sum(tp$pits) > 1, sd(tp_detail$tp_values[tp_detail$tp_type == "pit"]), 0)
        )

        return_list <- list(tp_detail = tp_detail, tp_info = tp_info, gm_model = gm)

        if (!is.null(root_folder)) {
          png(
            file = file.path(root_folder, "data/results/pngs/", paste0(png_prefix, "_", p, ".png")),
            width = 600, height = 350
          )
          plot(jds, gm_pred, main = data$plotID[p])
          points(jds[tp$tppos], ts_y[tp$tppos], col = "red")
          points(act_df$d, act_df$v, col = "blue")
          dev.off()
        }
      }
    } else {
      print(paste0("Excluding ", data[p, "plotID"]))
    }
    return(return_list)
  })
  names(tp_data) <- data$plotID

  return(list(
    tp_info = do.call("rbind", lapply(tp_data, "[[", 2)), tp_details = lapply(tp_data, "[[", 1),
    gm_model = lapply(tp_data, "[[", 3)
  ))
}
