#' Compile plotwise datasets from meteorological observations.
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
compileMetDataset <- function(root_folder, met_pars = c("Ta_200", "precipitation_radolan"), jd_range = c(90, 300)) {

  met_obs <- read.csv(file.path(root_folder, "data/raw_data/meteorological_variables/plots.csv"))
  met_obs$datetime <- as.POSIXct(met_obs$datetime, tz = "UTC")

  years <- unique(met_obs$year)
  year_explo_pars <- lapply(years, function(y){
    act_year <- met_obs[met_obs$year == y, ]
    explos <- unique(substr(act_year$plotID, 1, 3))

    explo <- lapply(explos, function(e){


      plots <- lapply(met_pars, function(p){

        act_explo <- act_year[substr(act_year$plotID, 1, 3) == e, ]

        tmp <- data.frame(plotID = act_explo$plotID,
                          datetime = julian(act_explo$datetime,
                                            origin = as.POSIXct(paste0(as.character(y-1), "-12-31"), tz = "UTC")),
                          var = act_explo[, p])

        tmp <- as.data.frame(pivot_wider(tmp[tmp$datetime >= jd_range[1] & tmp$datetime <= jd_range[2], ],
                                         names_from = datetime, values_from = var))

        names(tmp)[-1] <- paste0("JD", names(tmp)[-1])

        return(tmp)
      })
      names(plots) <- met_pars
      return(plots)
    })
    names(explo) <- explos
    return(unlist(explo, recursive = FALSE))
  })
  names(year_explo_pars) <- years
  year_explo_pars <- unlist(year_explo_pars, recursive = FALSE)
  names(year_explo_pars) <- gsub("[.]", "_", names(year_explo_pars))

  year_explo_pars <- lapply(years, function(y){
    year_explo_pars[grep(y, names(year_explo_pars))]
  })
  names(year_explo_pars) <- paste0("obs", years)

  return(year_explo_pars)
}
