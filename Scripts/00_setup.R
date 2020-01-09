# Purpose: Set up repository for Cambodia Analysis and Capacity Building
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_09_10
# Audience: Cambodia Mission


# Create folders for project (if they do not exist)
folder_list <- list("Data", "Images", "Scripts", "Dataout", "GIS", "Documents", "Graphics")
purrr::map(folder_list, ~dir.create(.))

datapath <- "Data"
dataout <- "Dataout"
gispath <- "GIS"
graphpath <- "Graphics"
imagepath <- "Images"
rpath <- "Scripts"