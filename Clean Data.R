library(tidyverse)
library(data.table)
library(rvest)
library(RCurl)
#4608

# Inside Airbnb Data

webpage <- read_html("http://insideairbnb.com/get-the-data.html") %>%
  html_nodes("td")

# A vector of dates that Inside Airbnb has snapshots of data for New York City
nyc_dates <- html_text(webpage[min(which(html_text(webpage) == "New York City")):
                                 max(which(html_text(webpage) == "New York City"))]) %>%
  as.Date(., format = "%d %B, %Y") %>%
  unique() %>%
  na.omit() %>%
  sort()

rm(webpage)

# All downloadable files follow a fixed format with date in YYYY-MM-DD
base_url <- "http://data.insideairbnb.com/united-states/ny/new-york-city/"
end_url <- "/data/listings.csv.gz"

airbnb <- map(nyc_dates, ~read_csv(paste0(base_url, .x, end_url),
                                   col_types = cols_only(
                                     last_scraped = "?", id = "?", name = "?", 
                                     host_id = "?", host_name = "?", 
                                     neighborhood_cleansed = "?", 
                                     neighborhood_group_cleansed = "?",
                                     latitude = "?", longitude = "?", 
                                     is_location_exact = "?", property_type = "?", 
                                     room_type = "?", 
                                     bathrooms = "?", bedrooms = "?", 
                                     price = "n", weekly_price = "n", monthly_price = "n",
                                     minimum_nights = "?", maximum_nights = "?"
                                   )) %>%
                filter(price > 0 & !is.na(price)) %>%
                filter(maximum_nights >= 180) %>%
                mutate(rent = if_else(!is.na(monthly_price), monthly_price,
                                      if_else(!is.na(weekly_price), weekly_price*4,
                                              price*30))) %>%
                select(-monthly_price, -weekly_price, -price))

# US Census Data

census_url <- "https://raw.githubusercontent.com/ksw-angela/airbnb-nyc/master/Data/"
median_rent <- map(2015:2017, ~read.csv(paste0(census_url,
                                              "Median%20Gross%20Rent%20By%20Bedrooms%20",
                                              .x, ".csv")) %>%
                     select(census_tract = `GEO.display.label`, 
                            med_rent_0bed = HD01_VD03,
                            med_rent_1bed = HD01_VD04,
                            med_rent_2bed = HD01_VD05,
                            med_rent_3bed = HD01_VD06,
                            med_rent_4bed = HD01_VD07,
                            med_rent_5bed = HD01_VD08) %>%
                     filter(census_tract != "Geography") %>%
                     mutate(census_tract = gsub("Census Tract ", "",
                                                gsub(", New York County, New York", "",
                                                     census_tract)),
                            year = .x) %>%
                     mutate_at(vars(starts_with("med")), ~as.numeric(gsub("3,500\\+", 
                                                                          "3500", .)))) %>%
  bind_rows()
median_income <- map(2015:2017, ~read_csv(paste0(census_url,
                                                 "Median%20Household%20Income%20",
                                                 .x, ".csv"), skip = 1) %>%
                       select(census_tract = Geography, 
                              med_hh_income = starts_with("Estimate")) %>%
                       mutate(census_tract = gsub("Census Tract ", "",
                                                  gsub(", New York County, New York", "",
                                                       census_tract)),
                              med_hh_income = as.numeric(gsub("250,000\\+", "250000",
                                                              med_hh_income)),
                              year = .x)) %>%
  bind_rows()

# Census Tract Boundaries

shp_url <- "https://archive.nyu.edu/retrieve/74712/nyu_2451_34513.zip"

