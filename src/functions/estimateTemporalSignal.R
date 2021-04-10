#' Compile key features from irregulary sampled Sentinel-2 observations.
#'
#' @description A gam is used to compile a continous temporal signal (1 day resolution) from irregular Sentinel 2
#' observations. Key features of the continous signal are extracted.
#'
#' @param data Dataset with one study plot per row and columns as julian days.
#' @param info_year Metadata providing the year of the observation
#' @param jd_start Start day of the compiled continous time series (not the starting day of the observations)
#' @param jd_end End day of the compiled continous time series (not the last day of the observations)
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
estimateTemporalSignal <- function(data, info_year, jd_start = 90, jd_end = 300, save_png = FALSE) {
  jds <- seq(jd_start, jd_end)

  tp_data <- lapply(seq(nrow(data)), function(p) {
    col_ids <- grep("JD", colnames(data))
    act_df <- data.frame(
      d = as.numeric(sub("JD", "", colnames(data)[col_ids])),
      v = as.numeric(data[p, col_ids])
    )

    # ggplot(data = act_df, aes(x = d, y = v)) + geom_point() + geom_smooth(method = "gam")

    if ((nrow(act_df) - 10) >= sum(is.na(act_df$v))) {
      set.seed(11081974)
      gm <- gam(v ~ s(d, k = -1, bs = "cr"), data = act_df)
      # coef(gm)
      gm_pred <- predict(gm, data.frame(d = jds))
      ts_y <- ts(as.numeric(gm_pred), start = jd_start, end = jd_end)
      tp <- turnpoints(ts_y)

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
          tp_info = tp$info
        )

        tp_info <- data.frame(
          plotID = data$plotID[p],
          LUI = data$LUI[p],
          Year = info_year,
          gam_q000_val = quantile(gm_pred, probs = 0),
          gam_q025_val = quantile(gm_pred, probs = 0.25),
          gam_q050_val = quantile(gm_pred, probs = 0.50),
          gam_q075_val = quantile(gm_pred, probs = 0.75),
          gam_q100_val = quantile(gm_pred, probs = 1),
          gam_mean_val = mean(gm_pred),
          gam_sd_val = sd(gm_pred),
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
      }

      if (save_png) {
        png(file = file.path("data/tmp/", paste0(p, ".png")), width = 600, height = 350)
        plot(jds, gm_pred, main = data$plotID[p])
        points(jds[tp$tppos], ts_y[tp$tppos], col = "red")
        points(act_df$d, act_df$v, col = "blue")
        dev.off()
      }
    } else {
      print(paste0("Excluding ", data[p, "plotID"]))
      tp_detail <- NULL
      tp_info <- NULL
      gm <- NULL
    }


    return(list(tp_detail = tp_detail, tp_info = tp_info, gm_model = gm))
  })
  names(tp_data) <- data$plotID

  return(list(
    tp_info = do.call("rbind", lapply(tp_data, "[[", 2)), tp_details = lapply(tp_data, "[[", 1),
    gm_model = lapply(tp_data, "[[", 3)
  ))
}
