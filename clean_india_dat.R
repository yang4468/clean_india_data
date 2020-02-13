if (!require(ipumsr)) install.packages("ipumsr")

library(tidyverse)
library(ipumsr)


india_dat_file <- "IPUMS.dat"
india_xml_file <- "IPUMS.xml"

india_ddi <- read_ipums_ddi(india_xml_file)
india_data_raw <- read_ipums_micro(india_ddi, data_file = india_dat_file )



factors <- c( "urban", "sex", "edattain", "geo1_in1999" )
names(india_data_raw) <- tolower(names(india_data_raw))

india02 <- india_data_raw %>%
  as.tibble() %>%
  mutate(age = as.integer(age)) %>%
  select(year, urban, age, sex,edattain, incwage, perwt, geo1_in1999) %>%
  mutate_at( funs(as.character(as_factor(ipumsr::lbl_clean(.)))), .vars = vars(factors)) %>%
  dplyr::filter(age <= 65, age >= 22)  %>%
  mutate(incwage  = ifelse(incwage > 99999, NA, incwage)) %>%
  mutate(incwage  = ifelse(incwage == 0, NA, incwage)) %>%
  as.tibble() %>%
  mutate(edattain = ifelse(edattain == "Less than primary completed", 
                           "<primary", edattain) ,
         edattain = ifelse(edattain == "Primary completed",
                           "primary", edattain),
         edattain = ifelse(edattain == "Secondary completed",
                           "secondary", edattain),
         edattain = ifelse(edattain == "University completed",
                           "university", edattain))



save(india02 , file = "india02.Rdata")

.rs.restartR() 