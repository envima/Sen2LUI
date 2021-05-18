#' Compile model dataset.
#'
#' @description Compile model dataset from individual predictor data..
#'
#' @param ssets Satellite predictors.
#' @param msets Meteorological predictors.
#' @param meta Meta information dataset (initialized with envimaR::createMeta)
#' @param act_predictor_group Group of predictors to be considered (one of "sat", "met" or "both").
#' @param cor_cutoff Cut off correlation value for removing highly correlated predictors from final set.
#'
#' @return Model dataset.
#'
#' @details
#'
#' @name compileModelDataset
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' ### Extract actual predictor variables from the overall predictor dataset.
compileModelDataset <- function(ssets, msets, meta, act_predictor_group, cor_cutoff = 0.95) {
  meta$predictor_group = act_predictor_group

  df <- lapply(ssets, "[[", 1)
  names(df) <- names(ssets)

  df_met <- lapply(msets, "[[", 1)
  names(df_met) <- names(msets)

  meta$pvid <- apply(expand.grid(meta$years, meta$explos), 1, paste, collapse = "_")
  df_cmb <- lapply(meta$pvid, function(e) {
    tmp <- Reduce(function(x, y) merge(x, y, all = TRUE), df[grep(e, names(df))])
    tmp_met <- Reduce(function(x, y) merge(x, y, all = TRUE), df_met[grep(e, names(df_met))])
    return(merge(tmp, tmp_met))
  })
  names(df_cmb) <- meta$pvid



  ### Compile model dataset
  model_data <- Reduce(function(x, y) rbind(x, y), df_cmb[meta$model_dataset])
  model_data <- model_data[complete.cases(model_data), ]
  meta$model_rows <- nrow(model_data)

  if (act_predictor_group == "sat") {
    meta$cols_meta <- c(
      meta$cols_meta,
      colnames(model_data)[which(!is.na(str_locate(
        colnames(model_data),
        paste(meta$met_predictors, collapse = "|")
      )[, 1]))]
    )
  } else if (act_predictor_group == "met") {
    meta$cols_meta <- c(
      meta$cols_meta,
      colnames(model_data)[!colnames(model_data) %in% c(
        meta$cols_meta,
        colnames(model_data)[which(!is.na(str_locate(
          colnames(model_data),
          paste(meta$met_predictors, collapse = "|")
        )[, 1]))])]
    )
  }

  meta$predictor_with_unique_values <- names(which(apply(model_data[, -which(names(model_data) %in% meta$cols_meta)],
                                                         2, function(x) length(unique(x))) == 1))
  cols_exclude <- c(meta$cols_meta, meta$predictor_with_unique_values)

  meta$correlated_predictors <- findCorrelation(cor(model_data[, -which(names(model_data) %in% cols_exclude)]),
    cutoff = cor_cutoff, names = TRUE, exact = TRUE
  )
  meta$predictor_group_final <- colnames(model_data)[!colnames(model_data) %in% cols_exclude]


  ### Split data frame by exploratories
  explos <- c("ALL", unique(model_data$Explo))
  model_data_explo <- lapply(explos, function(e) {
    if (e == "ALL") {
      act_explo <- model_data
    } else {
      act_explo <- model_data[model_data$Explo == e, ]
    }
    year_comb <- combn(unique(act_explo$Year), m = 2)

    act_explo_2y <- lapply(seq(ncol(year_comb)), function(i) {
      act_year <- as.character(unlist(year_comb[, i]))
      act <- act_explo[act_explo$Year %in% act_year, ]
      return(list(data = act, act_year = act_year))
    })
    names <- lapply(act_explo_2y, `[[`, 2)
    act_explo_2y <- lapply(act_explo_2y, `[[`, 1)
    for (i in seq(length(act_explo_2y))) {
      names(act_explo_2y)[i] <- paste(e, paste(names[[i]], collapse = "_"), sep = "_")
    }
    act_explo_2y <- c(list(act_explo), act_explo_2y)
    names(act_explo_2y)[1] <- paste(e, paste(meta$years, collapse = "_"), sep = "_")
    return(act_explo_2y)
  })
  names(model_data_explo) <- explos

  return(list(model_data_explo = model_data_explo, meta = meta))
}
