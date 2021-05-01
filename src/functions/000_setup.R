#' Setup project environment
#'
#' @description This script configures the project environment.
#'
#'

require(envimaR)

# Define libraries
libs <- c("CAST", "caret", "doParallel", "foreach", "mgcv", "pastecs", "tidyverse")

# Load libraries and create environment object to be used in other scripts for path navigation
project_folders <- list.dirs(path = root_folder, full.names = FALSE, recursive = TRUE)
project_folders <- project_folders[!grepl("\\..", project_folders)]
envrmt <- createEnvi(
  root_folder = root_folder, fcts_folder = file.path(root_folder, "src/functions/"),  folders = project_folders,
  libs = libs, create_folders = FALSE)
meta <- createMeta(root_folder)

# Define more variables

# Load more data
