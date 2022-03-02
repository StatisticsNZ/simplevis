# use this script to get data, and put it in the app
# if needed, add a zip file for download from the app
# 
# data_folder <- "inst/shiny/template2/data/"
# dir.create(data_folder)
# 
# data1 <- ggplot2::diamonds %>% 
#   slice_sample(prop = 0.1)
# 
# data2 <- simplevis::example_sf_point %>% 
#   filter(indicator %in% c("Nitrate-nitrogen", "Total nitrogen", "Ammoniacal nitrogen")) %>% 
#   slice_sample(prop = 0.1)
# 
# readr::write_csv(data1, "data1.csv")
# readr::write_csv(data2, "data2.csv")
# 
# zip::zipr(zipfile = paste0(data_folder, "data.zip"), 
#           files = c("data1.csv", "data2.csv"))
# 
# file.remove("data1.csv")
# file.remove("data2.csv")
# 
# saveRDS(data1, paste0(data_folder, "data1.RDS"))
# saveRDS(data2, paste0(data_folder, "data2.RDS"))
