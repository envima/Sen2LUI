#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]
#'

library(envimaR)
if (Sys.info()[["nodename"]] == "MyComputer") {
  root_folder = "D:/plygrnd/Sen2LUI/Sen2LUI"
} else {
  root_folder = "D:/plygrnd/Sen2LUI/Sen2LUI"
}
source(file.path(root_folder, "src/functions/000_setup.R"))

# Note: For small projects, all code can go to this file except the main project setup script.
# For large projects, this file should only call other sub-control files and/or functions.
# To add a script using envimaR see ?envimaR::addScript.

meta <- createMeta("Sen2LUI")

# Compile dataset
sen2_plots <- compileDataset()

# Compile predictors
meta$act_year = c("2018", "2019")
meta$act_pred = c("NDVI", "NDII", "SATVI")
explos <- c("Alb", "Hai", "Sch")

predictor_datasets_ids <- apply(expand.grid(meta$act_year, explos, meta$act_pred, "mean"), 1, paste, collapse = "_")

predictor_datasets <- lapply(predictor_datasets_ids, function(d){
  print(d)
  act <- estimateTemporalSignal(data = sen2_plots[[grep(substr(d, 1, 4), names(sen2_plots))]][[d]],
                         info_year = substr(d, 1, 4), save_png = FALSE)
  names(act$tp_info)[-c(1:3)] <- paste(substr(d, (str_locate_all(pattern ='_', d)[[1]][2,1]+1), nchar(d)),
                                       names(act$tp_info)[-c(1:3)], sep = "_")
  return(act)
})
names(predictor_datasets) <- predictor_datasets_ids

df <- lapply(predictor_datasets, "[[", 1)
names(df) <- names(predictor_datasets)

ids <- apply(expand.grid(meta$act_year, explos), 1, paste, collapse = "_")
df_cmb <- lapply(ids, function(e){
  Reduce(function(x, y) merge(x, y, all=TRUE), df[grep(e, names(df))])
})
names(df_cmb) <- ids

meta$act_model <- c("2018_Hai", "2019_Hai", "2018_Sch", "2019_Sch")
meta$method <- "rf"


Reduce(function(x, y) rbind(x, y), df[grep(e, names(df))])


act_model_data <- rbind(df_cmb[[meta$act_model[1]]], df_cmb[[meta$act_model[2]]])
act_model_data <- act_model_data[complete.cases(act_model_data), ]

cl <- makeCluster(3)
registerDoParallel(cl)

set.seed(11081974)
folds <- CreateSpacetimeFolds(act_model_data, spacevar = "plotID", k = 10)

set.seed(11081974)
ffs_model <- ffs(act_model_data[, 4:ncol(act_model_data)],
                 act_model_data$LUI,
                 method = meta$method,
                 metric = "RMSE",
                 trControl = trainControl(method = "cv", index = folds$index))

saveRDS(ffs_model, file = file.path("data/tmp/", paste0("model_", paste(meta$act_model, meta$method, sep = "_", collapse = "_"), ".rds")))

stopCluster(cl)


alb <- df_cmb$`2019_Alb`[complete.cases(df_cmb$`2019_Alb`),]
names(alb) <- gsub("AEG", "HEG", names(hai))
p2018alb_ndvi <- predict(ffs_model, alb)
plot(alb$LUI, p2018alb_ndvi)
summary(lm(p2018alb_ndvi ~ alb$LUI))


hai <- df_cmb$`2019_Hai`[complete.cases(df_cmb$`2019_Hai`),]
names(hai) <- gsub("HEG", "AEG", names(hai))
p2018hai_ndvi <- predict(ffs_model, hai)
plot(hai$LUI, p2018hai_ndvi)
summary(lm(p2018hai_ndvi ~ hai$LUI))

sch <- df_cmb$`2018_Sch`[complete.cases(df_cmb$`2018_Sch`),]
names(sch) <- gsub("Sch", "Alb", names(sch))
p2018sch_ndvi <- predict(ffs_model, sch)
plot(sch$LUI, p2018sch_ndvi)
summary(lm(p2018sch_ndvi ~ sch$LUI))
