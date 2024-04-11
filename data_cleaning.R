library(readxl)
library(stringr)
library(dplyr)
library(tidyverse)

sitenames <- read_excel("sitenames.xlsx") %>% 
  rename("site" = "name_new") %>%
  select(-name_long, -name_old)

#First, fix names
rawdata <- read_excel("rawdata.xlsx") %>%
  select(-coordinates) %>%
  filter(str_detect(tolower(sample), "sample")) %>%
  select(site_id:Time_difference) %>%
  select(-starts_with("User"), -COMMENTS, -Time_difference, -Date_return_shipped_to_ID, -Tracking) %>%
  left_join(., sitenames, by="site_id") %>%
  separate(coordinates, into = c("lat", "long"), sep = ",", remove = FALSE) %>%
  relocate(site) %>%
  relocate(benzene, benzene_flag, toluene, toluene_flag, etbenz, etbenz_flag,
           mpxylene,	mpxylene_flag,	oxylene,	oxylene_flag, .after = end_date) %>%
  mutate(
    xylenes = mpxylene + oxylene,
    xylenes_flag = case_when(
      mpxylene_flag == "ULOD" | oxylene_flag == "ULOD" ~ "ULOD",
      TRUE ~ "REG"
    ),
    btex = benzene + toluene + etbenz + mpxylene + oxylene,
    btex_flag = case_when(
      benzene_flag == "ULOD" | toluene_flag == "ULOD" & etbenz_flag == "ULOD" |mpxylene_flag == "ULOD" | oxylene_flag == "ULOD" ~ "ULOD",
      TRUE ~ "REG"
    )
    ) %>%
  relocate(xylenes, xylenes_flag, .after = etbenz_flag) %>%
  relocate(btex, btex_flag, .after = xylenes_flag) %>%
  select(-LOCATION, -coordinates, -sample)

  
  
  
write.csv(rawdata, "dat.csv")


