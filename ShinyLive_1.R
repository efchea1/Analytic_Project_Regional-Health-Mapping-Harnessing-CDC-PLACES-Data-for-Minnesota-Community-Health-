# This helps locate the directory the project is in. If it is returns, TRUE then that's fine. 
# If it returns FALSE, then that's not the right directory that you're in.
# file.exists("analytics_project_cdc_places_mn_region")
# 
# Check if the directory exists
# if (!fs::dir_exists("analytics_project_cdc_places_mn_region")) {
#   stop("The directory 'analytics_project_cdc_places_mn_region' does not exist.")
# }

# library
library(httpuv)
library(shinylive)

# Building a serverless shiny app for the Analytics_project
shinylive::export(
  appdir = "application",
  destdir = "docs"
)

# remotes::install_github("rstudio/httpuv)
httpuv::runStaticServer(
  dir = "docs",
  port = 8888
)
