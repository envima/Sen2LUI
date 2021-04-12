#' Compile plotwise datasets from Sentinel-2 observations.
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
compileDataset <- function() {
  sen2_pixel_files <- list.files("data/source/tabs_bands_pixelwise_pure/")
  sen2_plotID_pixels_files <- list.files("data/source/plot_id_pixelwise/")
  lui_files <- list.files("data/source/LUI/")

  # Load individual pixel values of all plots.
  sen2_pixels <- lapply(sen2_pixel_files, function(f) {
    act_explo <- substr(f, 1, 8)
    act_pixels <- read.csv(file.path("data/source/tabs_bands_pixelwise_pure/", f))
    act_plotID_pixels <- read.csv(file.path(
      "data/source/plot_id_pixelwise/",
      sen2_plotID_pixels_files[grep(act_explo, sen2_plotID_pixels_files)]
    ))
    act_pixels <- act_pixels[, -1]
    act_data <- cbind(act_plotID_pixels[, 2], act_pixels)
    names(act_data)[1] <- "plotID_pixels"

    return(act_data)
  })
  names(sen2_pixels) <- substr(sen2_pixel_files, 1, (nchar(sen2_pixel_files)) - 4)


  # Compute some more indices
  years <- c("2018", "2019")
  explos <- c("Alb", "Hai", "Sch")
  years_explos <- apply(expand.grid(years, explos), 1, paste, collapse = "_")

  for(p in years_explos){

    NDII <- (sen2_pixels[[paste0(p, "_B8")]][, -1] - sen2_pixels[[paste0(p, "_B11")]][, -1]) /
      (sen2_pixels[[paste0(p, "_B8")]][, -1] + sen2_pixels[[paste0(p, "_B11")]][, -1])
    NDII <- cbind(sen2_pixels[[paste0(p, "_B11")]]$plotID_pixels, NDII)
    names(NDII)[1] <- "plotID_pixels"

    sen2_pixels <- c(sen2_pixels, list(NDII = NDII))
    names(sen2_pixels)[length(sen2_pixels)] <- paste0(p, "_NDII")

    SATVI <- ((sen2_pixels[[paste0(p, "_B11")]][, -1] - sen2_pixels[[paste0(p, "_B4")]][, -1]) /
                (sen2_pixels[[paste0(p, "_B11")]][, -1] + sen2_pixels[[paste0(p, "_B4")]][, -1] + 0.5)) *
      (1 + 0.5) - (sen2_pixels[[paste0(p, "_B12")]][, -1] / 2)
    SATVI <- cbind(sen2_pixels[[paste0(p, "_B11")]]$plotID_pixels, SATVI)
    names(SATVI)[1] <- "plotID_pixels"
    sen2_pixels <- c(sen2_pixels, list(SATVI = SATVI))
    names(sen2_pixels)[length(sen2_pixels)] <- paste0(p, "_SATVI")
  }


  # Compute mean over plot for all bands/indices
  sen2_plots_mean <- lapply(seq(length(sen2_pixels)), function(i) {

    year <- substr(names(sen2_pixels)[i], 1, 4)

    f <- sen2_pixels[[i]]
    f$plotID <- substr(f$plotID_pixels, 1, (unlist(gregexpr(pattern = "_", f$plotID_pixels)) - 1))

    act_plots_mean <- f %>%
      group_by(plotID) %>%
      summarise(across(starts_with("X"), mean, na.rm = TRUE))

    names(act_plots_mean) <- str_replace(names(act_plots_mean), "X", "JD")

    lui <- read.csv(file.path(
      "data/source/LUI/",
      lui_files[grep(
        paste0(year, "_", substr(act_plots_mean$plotID[1], 1, 1)),
        lui_files
      )]
    ))
    lui$Year <- year
    names(lui)[which("EP.Plotid" == names(lui))] <- "plotID"
    lui$Explo <- substr(lui$plotID, 1, 3)
    lui$Explo_Year <- paste(lui$Explo, lui$Year, sep = "_")
    act_plots_mean <- merge(lui[, -1], act_plots_mean, by = "plotID")

    act_plots_mean$plotID[which(nchar(act_plots_mean$plotID) == 4)] <-
      gsub("^(.{3})(.*)$", "\\10\\2", act_plots_mean$plotID[which(nchar(act_plots_mean$plotID) == 4)])

    act_plots_mean <- act_plots_mean[order(act_plots_mean$plotID), ]

    return(act_plots_mean)
  })
  names(sen2_plots_mean) <- paste0(names(sen2_pixels), "_mean")


  # Compute sd over plot for indices
  index_pos <- grep(pattern = c("NDVI|NDII|SATVI|REIP"), names(sen2_pixels))
  sen2_plots_sd <- lapply(index_pos, function(i) {
    year <- substr(names(sen2_pixels)[i], 1, 4)

    f <- sen2_pixels[[i]]
    f$plotID <- substr(f$plotID_pixels, 1, (unlist(gregexpr(pattern = "_", f$plotID_pixels)) - 1))

    act_plots_sd <- f %>%
      group_by(plotID) %>%
      summarise(across(starts_with("X"), sd, na.rm = TRUE))

    names(act_plots_sd) <- str_replace(names(act_plots_sd), "X", "JD")

    lui <- read.csv(file.path(
      "data/source/LUI/",
      lui_files[grep(
        paste0(year, "_", substr(act_plots_sd$plotID[1], 1, 1)),
        lui_files
      )]
    ))
    lui$Year <- year
    names(lui)[which("EP.Plotid" == names(lui))] <- "plotID"
    lui$Explo <- substr(lui$plotID, 1, 3)
    lui$Explo_Year <- paste(lui$Explo, lui$Year, sep = "_")
    act_plots_sd <- merge(lui[, -1], act_plots_sd, by = "plotID")

    act_plots_sd$plotID[which(nchar(act_plots_sd$plotID) == 4)] <-
      gsub("^(.{3})(.*)$", "\\10\\2", act_plots_sd$plotID[which(nchar(act_plots_sd$plotID) == 4)])

    act_plots_sd <- act_plots_sd[order(act_plots_sd$plotID), ]

    return(act_plots_sd)
  })
  names(sen2_plots_sd) <- paste0(names(sen2_pixels[index_pos]), "_sd")

  sen2_plots <- c(sen2_plots_mean, sen2_plots_sd)

  sen2_plots <- list(
    obs2018 = sen2_plots[grep("2018", names(sen2_plots))],
    obs2019 = sen2_plots[grep("2019", names(sen2_plots))]
  )

  saveRDS(sen2_plots, "data/source/Sen2/sen2_plots.R")

  return(sen2_plots)
}


