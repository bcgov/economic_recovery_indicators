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
temp <- indicators_list %>% filter(str_detect(title, pattern = "Hotel Occupancy"))

data_hor <- tbl(cn, "vwDataPoints") %>%
  select(VectorNumber, ValueDate, Value) %>%
  filter(VectorNumber == "C40" & ValueDate >= "01-Jan-2010") %>%
  collect() %>% 
  ## create vars needed for app
  mutate(title = temp$title, #"<b>Hotel Occupancy Rate</b><br>(%, NSA)",
         label = temp$label, #"Hotel Occupancy Rate",
         filter_var = temp$filter_var, #"businesses",
         ref_date = as.Date(ValueDate, "%Y-%m-%d")) %>%
  ## re-order/name columns
  select(title, label, filter_var, ref_date, value = Value)

### * (ivt) Indigenous Employment, Off-Reserve (Thousands, 3MMA) ----
## install: https://www.statcan.gc.ca/eng/public/beyond20-20
## Source: Statistics Canada (open-data license by Stats Can)
## Beyond 2020 Tables (.ivt files)
## this data is manually updated by opening the .ivt file for the corresponding month and year
##   then copying to the appropriate column in Beyond2020data.csv
## 3MMA = Three-Month Moving Average
## aboriginal employment, both sexes, 15 years and up data totals for BC
## Make sure Aboriginal data is set to aboriginal-aboriginal instead of aboriginal-total
temp <- indicators_list %>% filter(str_detect(title, pattern = "Indigenous Employment"))

data_ind <- read_csv(file = paste0(DRIVE_LOCATION, PROJECT_LOCATION, "/Data/Beyond2020data.csv"), show_col_types = FALSE) %>%
  ## prep for YYYY-MM-DD ref_date
  mutate(Year = paste0("20", str_sub(Month, start = 1, end = 2)),   ##Year = paste0("20", str_sub(Month, start = -2)),
         Month = str_sub(Month, start = 4)) %>%
  dplyr::filter(Year >= 2010) %>%
  left_join(months, by = "Month") %>%
  mutate(ref_date = as.Date(paste0(Year, "-", m, "-01"), "%Y-%m-%d")) %>%
  ## create vars needed for app
  mutate(title = temp$title, #"<b>Indigenous Employment, Off-Reserve</b><br>(Thousands, 3MMA)",
         label = temp$label, #"Indigenous Employment, Off-Reserve",
         filter_var = temp$filter_var, #"chart",
         Indigenous = as.numeric(Indigenous)) %>%
  ## re-order/name columns
  select(title, label, filter_var, ref_date, value = Indigenous)

# ### * (web) International Merchandise Exports ($Thousands, SA) ----
# ## Source: BC Stats (we're already publishing this on our website)
# ## https://www2.gov.bc.ca/gov/content/data/statistics/business-industry-trade/trade/trade-data
# ## monthly sa (seasonally adjusted) data file
temp <- indicators_list %>% filter(str_detect(title, pattern = "International Merchandise"))

data_ime <- openxlsx::readWorkbook(xlsxFile = "https://www2.gov.bc.ca/assets/gov/data/statistics/business-industry-trade/trade/seasonally_adjusted_exports.xlsx",
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


### * bind non-cansim datasets ----
# temp <- indicators_list %>%    #read_csv(here::here("app", "indicators_list.csv")) %>%
#   dplyr::filter(dataset == "manual") %>% select(title) %>% pull()
# titles_nc <- c(rep(temp[1], dim(data_ushs)[1]),
#                rep(temp[2], dim(data_ind)[1]))
non_cansim_data <- bind_rows(data_ime, data_ushs, data_hor, data_ind) %>%
  mutate(title = fct_inorder(title),
         label = factor(label), 
         filter_var = factor(filter_var))

#### cleanup ----

odbc::dbDisconnect(cn)
rm(temp, #titles_nc, 
   data_ushs, data_ind,
   cn, DRIVE_LOCATION, PROJECT_LOCATION, months)

#### save datasets ----
saveRDS(data_trip, here::here("app", "data", "exports_data.rds"))
saveRDS(non_cansim_data, here::here("app", "data", "non_cansim_data.rds"))

#### UNUSED old code ----
# if (!require('zoo')) install.packages('zoo'); library(zoo)      ## needed for the Haver interface
# if (!require('Haver')) install.packages('Haver', repos='http://www.haver.com/r/'); library(Haver)

# ### * (haver) Housing Starts (Units, SAAR)
# ## Source: Haver
# ## Haver: GM00013
# temp <- indicators_list %>% filter(label == "Housing Starts")
# 
# haver.path("//decimal/DLX/DATA/")        # haver.path("restore")
# data_cmhc <- haver.data(codes = c("GM00013"),
#                         database = "CANADAR",
#                         start = as.Date("2010-01-01", format = "%Y-%m-%d")) %>%
#   data.frame() %>%
#   rownames_to_column(var = "Date") %>%
#   ## create vars needed for app
#   mutate(Year = str_sub(Date, start = 1, end = 5),
#          Month = str_sub(Date, start = 6)) %>%
#   left_join(months, by = "Month") %>%
#   mutate(title = temp$title, #"<b>Housing Starts</b><br>(units, SAAR)",
#          label = temp$label, #"Housing Starts",
#          filter_var = temp$filter_var, #"businesses",
#          ref_date = as.Date(paste0(Year, m, "-01"), "%Y-%m-%d"),
#          value = gm00013*1000) %>%
#   ## re-order/name columns
#   select(title, label, filter_var, ref_date, value)
# 
# 
# 
# 



