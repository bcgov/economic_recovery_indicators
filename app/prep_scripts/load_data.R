# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

## !!!! DON'T NEED TO CHANGE ANYTHING BEYOND THIS POINT !!!!
## Script will output two .rds data files to app/data that app will read in ##

#### load packages ----
library(dplyr)
library(stringr)
library(here)
library(tidyverse)
library(openxlsx)
#if (!require(RODBC)) install.packages("RODBC"); library(RODBC)  ## needed for statbase connection


#### prep ----

## get statbase connection and folder locations
source("app/prep_scripts/private_setup.R")


## months conversion
months <- data.frame(Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     m = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                     stringsAsFactors = FALSE)

## get indicators_list.csv
indicators_list <- read_csv(here("app", "indicators_list.csv"), show_col_types = FALSE)


#### get exports data ----
### * (trip) Key Exports by Destination and Commodity ----
## Source: Statistics Canada (open-data license by Stats Can)
## TRIP: Pandemic Indicators Report
## this data is manually updated by running the TRIP Application 'Pandemic Indicators Report' job and
##   then copying as TRIPexportdata.csv

data_trip <- openxlsx::readWorkbook(xlsxFile = paste0(DRIVE_LOCATION, PROJECT_LOCATION, 
                                                      "/Data/TRIPexportdata.xlsx"),
                                    startRow = 2) %>%
  mutate(Month = str_sub(Month, start = 1, 3)) %>%
  left_join(months, by = "Month") %>%
  mutate(ref_date = paste0(Year, "-", m, "-01")) %>%
  select(-Year, -Month, -m) %>%
  pivot_longer(-ref_date, names_to = "temp", values_to = "value") %>%
  mutate(value = janitor::round_half_up(value/1000, digits = 0),  ## b/c shown in $Thousands
         commodity = case_when(str_detect(temp, "Lumber") ~ "Lumber",
                               str_detect(temp, "Pulp") ~ "Pulp",
                               str_detect(temp, "Copper") ~ "Copper",
                               str_detect(temp, "Aluminum") ~ "Aluminum",
                               str_detect(temp, "Gas") ~ "Natural Gas",
                               str_detect(temp, "Coal") ~ "Coal",
                               str_detect(temp, "All") ~ "All"),
         # commodity = factor(commodity, levels = c("Lumber", "Pulp", "Copper", "Aluminum", 
         #                                          "Natural Gas", "Coal", "All")),
         destination = case_when(str_detect(temp, "Asia") ~ "Other Asia",
                                 str_detect(temp, "Japan") ~ "Japan",
                                 str_detect(temp, "China") ~ "Mainland China",
                                 str_detect(temp, "Taiwan") ~ "Taiwan",
                                 str_detect(temp, "United.States") ~ "United States",
                                 str_detect(temp, "World") ~ "World"),
         INDICATOR = case_when(commodity == "All" ~ as.character(destination),
                               destination == "World" ~ as.character(commodity),
                               TRUE ~ paste0("     ", commodity)),
         title = factor(paste0(destination, "_", commodity),
                        levels = c("United States_All", "United States_Lumber", "United States_Pulp",
                                   "United States_Copper", "United States_Aluminum",
                                   "United States_Natural Gas", "United States_Coal",
                                   "Mainland China_All", "Mainland China_Lumber", "Mainland China_Pulp",
                                   "Mainland China_Copper", "Mainland China_Aluminum",
                                   "Mainland China_Natural Gas", "Mainland China_Coal",
                                   "Japan_All", "Japan_Lumber", "Japan_Pulp", "Japan_Copper",
                                   "Japan_Aluminum", "Japan_Natural Gas", "Japan_Coal",
                                   "Taiwan_All", "Taiwan_Lumber", "Taiwan_Pulp", "Taiwan_Copper",
                                   "Taiwan_Aluminum", "Taiwan_Natural Gas", "Taiwan_Coal",
                                   "Other Asia_All", "Other Asia_Lumber", "Other Asia_Pulp",
                                   "Other Asia_Copper", "Other Asia_Aluminum",
                                   "Other Asia_Natural Gas", "Other Asia_Coal",
                                   "World_All", "World_Lumber", "World_Pulp", "World_Copper",
                                   "World_Aluminum", "World_Natural Gas", "World_Coal"))) %>%
  dplyr::filter(title != "World_All") %>%
  arrange(title) %>%
  select(title, INDICATOR, ref_date, destination, commodity, value)


#### get non-CANSIM data ----
### * (statbase) US Housing Starts (Thousands, SAAR) ----
## Source: US Census Bureau (open-data license by US Bureau)
temp <- indicators_list %>% filter(str_detect(title, pattern = "US Housing Starts"))

data_ushs <- tbl(cn, "vwDataPoints") %>%
  select(VectorNumber,ValueDate,Value, SeriesId) %>%
  filter(VectorNumber == 'V122103', ValueDate>='01-Jan-2010') %>%
  collect() %>%
  ## create vars needed for app
  mutate(title = temp$title, #"<b>US Housing Starts</b><br>(Thousands, SAAR)",
         label = temp$label, #"US Housing Starts",
         filter_var = temp$filter_var, #"overall",
         ref_date = as.Date(ValueDate, "%Y-%m-%d")) %>%
  ## re-order/name columns
  select(title, label, filter_var, ref_date, value = Value)

### * (statbase) Hotel Occupancy Rate (%, NSA) ----
## Source: CBRE Hotels' Trends (data all available on website noted below)
## alternate source: http://www.mtc-currentperformance.com/Hotel.aspx
##      choose: By Month, Geography: BC, Measure Occupancy rate, Refresh, Show Table; = C40
##      selections ~ http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2121&em=12&MS=1&GA=2&PR=&PSR=
##temp <- indicators_list %>% filter(str_detect(title, pattern = "Hotel Occupancy"))

##data_hor <- tbl(cn, "vwDataPoints") %>%
  ##select(VectorNumber, ValueDate, Value) %>%
  ##filter(VectorNumber == "C40" & ValueDate >= "01-Jan-2010") %>%
  ##collect() %>% 
  ## create vars needed for app
  ##mutate(title = temp$title, #"<b>Hotel Occupancy Rate</b><br>(%, NSA)",
         ##label = temp$label, #"Hotel Occupancy Rate",
         ##filter_var = temp$filter_var, #"businesses",
         ##ref_date = as.Date(ValueDate, "%Y-%m-%d")) %>%
  ## re-order/name columns
  ##select(title, label, filter_var, ref_date, value = Value)


# ### * (web) International Merchandise Exports ($Thousands, SA) ----
# ## Source: BC Data Catalogue (we're already publishing this on our website)
# ## https://catalogue.data.gov.bc.ca/dataset/ca3ad618-b023-4f22-b3f2-e9de1bee92d3/resource/3f0a7e31-a998-463e-9287-20a6631832e4/download/seasonally_adjusted_exports.xlsx
# ## monthly sa (seasonally adjusted) data file
temp <- indicators_list %>% filter(str_detect(title, pattern = "International Merchandise"))

data_ime <- openxlsx::readWorkbook(xlsxFile = "https://catalogue.data.gov.bc.ca/dataset/ca3ad618-b023-4f22-b3f2-e9de1bee92d3/resource/3f0a7e31-a998-463e-9287-20a6631832e4/download/seasonally_adjusted_exports.xlsx",
                                   startRow = 2) %>%
  dplyr::filter(str_detect(Month, pattern = "Source", negate = TRUE)) %>%
  ## prep for YYYY-MM-DD ref_date
  mutate(Year = case_when(str_detect(Month, "'") ~ str_sub(Month, start = -2), TRUE ~ NA_character_)) %>%
  fill(Year) %>%
  mutate(Year = case_when(Year >= 88 ~ paste0("19", Year), TRUE ~ paste0("20", Year)),
         Month = str_sub(Month, start = 1, end = 3)) %>%
  left_join(months, by = "Month") %>%
  ## create vars needed for app
  mutate(title = temp$title, #"<b>International Merchandise Exports</b><br>($Thousands, SA)",
         label = temp$label, #"International Merchandise Exports",
         filter_var = temp$filter_var, #"overall",
         ref_date = as.Date(paste0(Year, "-", m, "-01"), "%Y-%m-%d")) %>%
  ## re-order/name columns and drop all export columns other than Total
  select(title, label, filter_var, ref_date, value = Total)


### * (web) Hotel Occupancy Rate (%) ----
# ## Source: BC Data Catalogue
# ## https://catalogue.data.gov.bc.ca/dataset/cace513c-9506-4f20-8dd1-7a072034f5fe/resource/069e0cf9-b75d-499a-95eb-187331fb2e5b/download/monthly_tourism_indicators.csv

temp <- indicators_list %>% filter(str_detect(title, pattern = "Hotel Occupancy Rate"))

# Read the CSV file
url <- "https://catalogue.data.gov.bc.ca/dataset/cace513c-9506-4f20-8dd1-7a072034f5fe/resource/069e0cf9-b75d-499a-95eb-187331fb2e5b/download/monthly_tourism_indicators.csv"
data_raw <- readr::read_csv(url, col_types = cols(.default = "c"))

# Extract column BP (68th column), starting from row 44
bp_values <- data_raw[[68]][43:length(data_raw[[68]])]
bp_values <- bp_values[bp_values != ""] # remove blanks

# Create a date sequence starting from January 2001
ref_dates <- seq.Date(from = as.Date("2001-01-01"), by = "month", length.out = length(bp_values))

# Create the final data frame
data_hotel <- tibble(
  title = temp$title,
  label = temp$label,
  filter_var = temp$filter_var,
  ref_date = ref_dates,
  value = as.numeric(bp_values)
)

# Remove rows with NA values in the 'value' column
data_hotel <- data_hotel %>% filter(!is.na(value))


#### cleanup ----

odbc::dbDisconnect(cn)
rm(temp, 
   cn, DRIVE_LOCATION, PROJECT_LOCATION, months)

# library(dplyr)
# library(stringr)



#### combine non-CANSIM datasets ----
non_cansim_data <- bind_rows(data_ushs, data_ime, data_hotel)

#### save datasets (unchanged) ----
saveRDS(data_trip, here::here("app", "data", "exports_data.rds"))
saveRDS(non_cansim_data, here::here("app", "data", "non_cansim_data.rds"))

#### modify data_trip for CSV export ----
data_trip_csv <- data_trip %>%
  select(-title, -INDICATOR) %>%        # Remove 'title' and 'INDICATOR'
  rename(value_dollars = value)         # Rename 'value' to 'value_dollars'

#### modify non_cansim_data for CSV export ----
non_cansim_data_csv <- non_cansim_data %>%
  # Extract text inside parentheses after </b><br>
  mutate(units_desc = str_match(title, "</b><br>\\(([^)]*)\\)")[, 2]) %>%
  # Rename columns and remove unwanted ones
  rename(
    indicator = label,
    value_desc = units_desc
  ) %>%
  select(-title, -filter_var)  # Drop title and filter_var

#### also save as CSV ----
write.csv(data_trip_csv, here::here("app", "data", "exports_data.csv"), row.names = FALSE)
write.csv(non_cansim_data_csv, here::here("app", "data", "non_cansim_data.csv"), row.names = FALSE)




