# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(usethis, here)

# clean data ----
exkansas <- read.csv(here::here("data-raw","exkansas.csv"), 
                     sep = ',', header = TRUE)

exkansas <- as.data.frame(exkansas)

# write data in correct format to data folder ----
usethis::use_data(exkansas, overwrite = TRUE)