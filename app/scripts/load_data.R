## !!!! DON'T NEED TO CHANGE ANYTHING BEYOND THIS POINT !!!!
## Script will output two .rds data files to app/data that app will read in ##

#### load packages ----
if (!require('zoo')) install.packages('zoo'); library(zoo)      ## needed for the Haver interface
if (!require('Haver')) install.packages('Haver', repos='http://www.haver.com/r/'); library(Haver)
library(tidyverse)
library(openxlsx)
if (!require(RODBC)) install.packages("RODBC"); library(RODBC)  ## needed for statbase connection


#### prep ----

## get statbase connection and folder locations
source("app/scripts/private_setup.R")

## months conversion
months <- data.frame(Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     m = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                     stringsAsFactors = FALSE)


#### get exports data ----
### * (trip) Key Exports by Destination and Commodity ----
## Source: Statistics Canada (open-data license by Stats Can)
## TRIP: Pandemic Indicators Report
## \\SFP.IDIR.BCGOV\S152\S52002\Trade\TRIP files\Application
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
  filter(title != "World_All") %>%
  arrange(title) %>%
  select(title, INDICATOR, ref_date, destination, commodity, value)


#### get non-CANSIM data ----
### * (web) International Merchandise Exports ($Thousands, SA) ----
## Source: BC Stats (we're already publishing this on our website)
## https://www2.gov.bc.ca/gov/content/data/statistics/business-industry-trade/trade/trade-data
## monthly sa (seasonally adjusted) data file
data_ime <- openxlsx::readWorkbook(xlsxFile = "https://www2.gov.bc.ca/assets/gov/data/statistics/business-industry-trade/trade/seasonally_adjusted_exports.xlsx",
                                   startRow = 2) %>%
  filter(str_detect(Month, pattern = "Source", negate = TRUE)) %>%
  ## prep for YYYY-MM-DD ref_date
  mutate(Year = case_when(str_detect(Month, "'") ~ str_sub(Month, start = -2), TRUE ~ NA_character_)) %>%
  fill(Year) %>%
  mutate(Year = case_when(Year >= 88 ~ paste0("19", Year), TRUE ~ paste0("20", Year)),
         Month = str_sub(Month, start = 1, end = 3)) %>%
  left_join(months, by = "Month") %>%
  ## create vars needed for app
  mutate(title = "<b>International Merchandise Exports</b><br>($Thousands, SA)",
         label = "International Merchandise Exports",
         filter_var = "overall",
         ref_date = as.Date(paste0(Year, "-", m, "-01"), "%Y-%m-%d")) %>%
  ## re-order/name columns and drop all export columns other than Total
  select(title, label, filter_var, ref_date, value = Total)


### * (statbase) US Housing Starts (Thousands, SAAR) ----
## Source: US Census Bureau (open-data license by US Bureau)
## Statbase: V122103
## J:\PGMS\SQL Statbase\Data Extraction\Statbase.exe

data_ushs <- sqlQuery(cn, "SELECT VectorNumber,ValueDate,Value FROM vwDataPoints WHERE SeriesId IN 
                           (SELECT SeriesId FROM dbo.GetSeriesId
                           ('V122103')
                           ) AND ValueDate>='01-Jan-2010'") %>%
  ## create vars needed for app
  mutate(title = "<b>US Housing Starts</b><br>(Thousands, SAAR)",
         label = "US Housing Starts",
         filter_var = "overall",
         ref_date = as.Date(ValueDate, "%Y-%m-%d")) %>%
  ## re-order/name columns
  select(title, label, filter_var, ref_date, value = Value)


### * (haver) Housing Starts (Units, SAAR) ----
## Source: Haver
## Haver: GM00013

haver.path("//decimal/DLX/DATA/")        # haver.path("restore")
data_cmhc <- haver.data(codes = c("GM00013"),
                        database = "CANADAR",
                        start = as.Date("2010-01-01", format = "%Y-%m-%d")) %>%
  data.frame() %>%
  rownames_to_column(var = "Date") %>%
  ## create vars needed for app
  mutate(Year = str_sub(Date, start = 1, end = 5),
         Month = str_sub(Date, start = 6)) %>%
  left_join(months, by = "Month") %>%
  mutate(title = "<b>Housing Starts</b><br>(units, SAAR)",
         label = "Housing Starts",
         filter_var = "businesses",
         ref_date = as.Date(paste0(Year, m, "-01"), "%Y-%m-%d"),
         value = gm00013*1000) %>%
  ## re-order/name columns
  select(title, label, filter_var, ref_date, value)


### * (statbase) Hotel Occupancy Rate (%, NSA) ----
## Source: CBRE Hotels' Trends (data all available on website noted below)
## Statbase: C40
## J:\PGMS\SQL Statbase\Data Extraction\Statbase.exe
## alternate source: http://www.mtc-currentperformance.com/Hotel.aspx
##      choose: By Month, Geography: BC, Measure Occupancy rate, Refresh, Show Table; = C40
##      can't see a way to scrape the data programmatically

data_hor <- sqlQuery(cn, "SELECT VectorNumber,ValueDate,Value FROM vwDataPoints WHERE SeriesId IN 
                           (SELECT SeriesId FROM dbo.GetSeriesId
                           ('C40')
                           ) AND ValueDate>='01-Jan-2010'") %>%
  ## create vars needed for app
  mutate(title = "<b>Hotel Occupancy Rate</b><br>(%, NSA)",
         label = "Hotel Occupancy Rate",
         filter_var = "businesses",
         ref_date = as.Date(ValueDate, "%Y-%m-%d")) %>%
  ## re-order/name columns
  select(title, label, filter_var, ref_date, value = Value)


### * (ivt) Indigenous & Immigrant Employment data ----
## install: https://www.statcan.gc.ca/eng/public/beyond20-20
## Source: Statistics Canada (open-data license by Stats Can)
## Beyond 2020 Tables (.ivt files)
## this data is manually updated by opening each .ivt file for the corresponding month and year
##   then copying to the appropriate column in Beyond2020data.csv

### ** Indigenous B.C. Employment, Off-Reserve (Thousands, Three-Month Moving Average)
## J:\DATA\StatCan\LABOURFORCESURVEY\Aboriginal LFS\<Year> Monthly\<YYYY-MM>\
##        4ctl_abo_cow_3MMA.ivt


### ** Immigrant Employment, B.C. (Thousands, Three-Month Moving Average) – Recent immigrants
## J:\DATA\StatCan\LABOURFORCESURVEY\Immigration LFS\<Year> Monthly\<YYYY-MM>\
##        IMMIGRANTS_provinces_immigrant_3MMA.ivt


### ** Immigrant Employment, B.C. (Thousands, Three-Month Moving Average) – Very recent immigrants
## J:\DATA\StatCan\LABOURFORCESURVEY\Immigration LFS\<Year> Monthly\<YYYY-MM>\
##        IMMIGRANTS_provinces_immigrant_3MMA.ivt

### ** Indigenous & Immigrant Employment data
data_emp <- read_csv(file = paste0(DRIVE_LOCATION, PROJECT_LOCATION, "/Data/Beyond2020data.csv")) %>%
  ## prep for YYYY-MM-DD ref_date
  mutate(Year = paste0("20", str_sub(Month, start = -2)),
         Month = str_sub(Month, end = 3)) %>%
  ## earlier data is blank for immigrants and reads in as "-"
  filter(Year >= 2010) %>%
  left_join(months, by = "Month") %>%
  mutate(ref_date = as.Date(paste0(Year, "-", m, "-01"), "%Y-%m-%d"),
         across(where(is.numeric), as.character)) %>%
  select(ref_date, Ind = Indigenous, VRI = `Very Recent Immigrants (5 years or less)`,
         RI = `Recent Immigrants (5+ to 10 years)`) %>%
  pivot_longer(-ref_date, names_to = "temp", values_to = "value") %>%
  ## create vars needed for app
  mutate(title = case_when(temp == "Ind" ~ "<b>Indigenous Employment, Off-Reserve</b><br>(Thousands, Three-Month Moving Average)",
                           temp == "VRI" ~ "<b>Immigrant Employment (Very Recent Immigrants)</b><br>(Thousands, Three-Month Moving Average)",
                           temp == "RI" ~ "<b>Immigrant Employment (Recent Immigrants)</b><br>(Thousands, Three-Month Moving Average)"),
         label = case_when(temp == "Ind" ~ "Indigenous Employment, Off-Reserve",
                           TRUE ~ "Immigrant Employment"),
         filter_var = "chart",
         value = as.numeric(value)) %>%
  ## re-order/name columns
  select(title, label, filter_var, ref_date, value)


### * bind non-cansim datasets ----
temp <- read_csv(here::here("/app/indicators_list.csv")) %>% 
  filter(dataset == "non_cansim") %>% select(title) %>% pull()
titles_nc <- c(rep(temp[1], dim(data_ime)[1]),
               rep(temp[2], dim(data_ushs)[1]),
               rep(temp[3], dim(data_cmhc)[1]),
               rep(temp[4], dim(data_hor)[1]),
               rep(c(temp[5:7]), dim(data_emp)[1]/3))
non_cansim_data <- bind_rows(data_ime, data_ushs, data_cmhc, data_hor, data_emp) %>%
  mutate(title = factor(x = titles_nc, levels = temp[1:6]),
         label = factor(label), 
         filter_var = factor(filter_var))

rm(temp, titles_nc, data_ime, data_ushs, data_cmhc, data_hor, data_emp, 
   cn, DRIVE_LOCATION, PROJECT_LOCATION, months)


#### save datasets ----
saveRDS(data_trip, here::here("/app/data/exports_data.rds"))
saveRDS(non_cansim_data, here::here("/app/data/non_cansim_data.rds"))
