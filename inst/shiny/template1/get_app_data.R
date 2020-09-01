# use this script to get data, and put it in the app
# this should include any processing not required to be done in the app
# this should include making a zip file for download from the app

# data <- ggplot2::diamonds %>% 
#   slice_sample(prop = 0.1)
# 
# folder <- "inst/shiny/template1/data/"
# 
# readr::write_csv(data, "data.csv")
# 
# zip::zipr(zipfile = paste0(data_folder, "data.zip"), files = c("data.csv"))
# 
# file.remove("data.csv")
# 
# saveRDS(data, paste0(data_folder, "data.RDS"))
