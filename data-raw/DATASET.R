# usethis::use_data_raw()
#I used this to create data_raw folder

## code to prepare `DATASET` dataset goes here

unemp_2013 <- readr::read_csv("https://raw.githubusercontent.com/b-rodrigues/modern_R/master/datasets/unemployment/unemp_2013.csv")
unemp_2014 <- readr::read_csv("https://raw.githubusercontent.com/b-rodrigues/modern_R/master/datasets/unemployment/unemp_2014.csv")
unemp_2015 <- readr::read_csv("https://raw.githubusercontent.com/b-rodrigues/modern_R/master/datasets/unemployment/unemp_2015.csv")

library(dplyr)
clean_data <- function(x){
  x %>%
    janitor::clean_names() %>%
    mutate(level = case_when(
      grepl("Grand-D.*", commune) ~ "Country",
      grepl("Canton", commune) ~ "Canton",
      !grepl("(Canton|Grand-D.*)", commune) ~ "Commune"
    ),
    commune = ifelse(grepl("Canton", commune),
                     stringr::str_remove_all(commune, "Canton "),
                     commune),
    commune = ifelse(grepl("Grand-D.*", commune),
                     stringr::str_remove_all(commune, "Grand-Duche de "),
                     commune),
    ) %>%
    select(year,
           place_name = commune,
           level,
           everything())
}


my_datasets <- list(
  unemp_2013,
  unemp_2014,
  unemp_2015
)

unemp <- purrr::map_dfr(my_datasets, clean_data)

usethis::use_data(unemp, overwrite = TRUE)
# (param =NULL) allows function to not put anything
# for instance, by not providing the year the user gets everything


clean_unemp(unemp,
            active_population) %>% 
  filter(place_name == "Luxembourg", level=="Country") %>% 
  readr::write_csv("tests/testthat/test_data_communes.csv")
