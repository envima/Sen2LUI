#' Compile graphics.
#'
#' @description Compile graphics for publication.
#'
#'

### Set evnironment
library(envimaR)
if (Sys.info()[["nodename"]] == "PC19616") {
  root_folder <- "D:/plygrnd/Sen2LUI/Sen2LUI"
} else {
  root_folder <- "~/plygrnd/Sen2LUI"
}
source(file.path(root_folder, "src/functions/000_setup.R"))



### Define settings
train_model <- TRUE
use_predictor_group <- c("sat", "met", "sat_met")
meta <- createMeta("Sen2LUI")
meta$explos <- c("Alb", "Hai", "Sch")
meta$years <- c("2017", "2018", "2019")
meta$jd_range <- c(90, 300)
meta$predictors <- c("NDVI", "REIP", "DSWI", "MCARI", "NDII", "SATVI", "B12")
meta$met_predictors <- c("Ta_200", "precipitation_radolan")
meta$model_dataset <- c(
  "2017_Alb", "2018_Alb", "2019_Alb",
  "2017_Hai", "2018_Hai", "2019_Hai",
  "2017_Sch", "2018_Sch", "2019_Sch"
)
meta$method <- "cubist"
meta$space_vars <- c("Explo_Year", "Year", "Explo")



### Compile dataset
# Sentinel 2
sen2_plots <- enviLoad(file.path(root_folder, "data/compiled_data/", "sen2_plots.rds"))$dat

# Climate stations
met_plots <- enviLoad(file.path(root_folder, "data/compiled_data/", "met_plots.rds"))$dat




### Compile predictors
ssets <- enviLoad(file.path(root_folder, "data/compiled_data/", "ssets.rds"))$dat
msets <- enviLoad(file.path(root_folder, "data/compiled_data/", "msets.rds"))$dat
meta <- enviLoad(file.path(root_folder, "data/compiled_data/", "msets.rds"))$meta



### Extract actual predictor variables from the overall predictor dataset.
model_datasets <- lapply(use_predictor_group, function(g) {
  enviLoad(file.path(root_folder, "data/compiled_data/", paste0("model_data_explo_", g, ".rds")))
})
names(model_datasets) <- use_predictor_group



### Number of satellite scenes
yrs <- c("obs2017", "obs2018", "obs2019")
ids <- c("Alb_B2_mean", "Hai_B2_mean", "Sch_B2_mean")

nss <- do.call("rbind", lapply(yrs, function(y){
  do.call("rbind", lapply(ids, function(i){
    lid <- which(grepl(i, names(sen2_plots[[y]])))
    n_min <- min(unlist(lapply(seq(nrow(sen2_plots[[y]][[lid]])), function(r) {
      sum(!is.na(
        sen2_plots[[y]][[lid]][r, grep("JD", colnames(sen2_plots[[y]][[lid]]))]
      ))
    })))
    n_max <- max(unlist(lapply(seq(nrow(sen2_plots[[y]][[lid]])), function(r) {
      sum(!is.na(
        sen2_plots[[y]][[lid]][r, grep("JD", colnames(sen2_plots[[y]][[lid]]))]
      ))
    })))
    return(data.frame(year = y, explo = i, n_min = n_min, n_max = n_max))
  }))
}))
nss[order(nss$explo),]



### LUI
figure_LUI <- ggplot(
  data = model_datasets$sat$dat$ALL$ALL_2017_2018_2019,
  mapping = aes(x = Year, y = LUI, fill = Explo)
) +
  geom_boxplot(notch = TRUE) +
  scale_fill_grey(start = 0.4, end = 0.8) +
  theme_bw() +
  theme(
    text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 10),
    legend.position = "right", axis.text.x = element_text(angle = 00, hjust = 0.5, vjust = 0.2),
    legend.title = element_blank(), panel.background = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

ggsave(file.path(root_folder, "data/results/figures/figure_2_LUI.png"),
  plot = figure_LUI,
  width = 175, height = 110, units = "mm", dpi = 300
)
