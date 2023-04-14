# Script to get historical meteorological data from NOAA
#
# Specify lat/long for site(s)
# Specify date range of interest
#
# Output:
#   individual rds files containing meteorological data for each site and year
#     stored in ./weather folder
# Optional Additional Output:
#   csv & rds files assembled for all sites containing outdoor temp and RH 
# 
# Tom Parkinson & Paul Raftery 2022


#### SETUP ####
library("readxl")
library("tidyverse")
library("magrittr")
library("progress")
library("magrittr")

# load packages using pacman
pacman::p_load(tidyverse, here, lubridate, janitor, # wrangling
               worldmet, # retrieve ISD data
               lutz, # timezones
               pbapply) # progress bars



####  DEFINE FUNCTIONS ####

# function to load all available stations in a given date range
load_isds <- function(begin="2020-01-01",end="2021-12-31"){
  # pull down list of all available weather stations and prune to usable data
  all_isds <- read_csv("https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv") %>%
    janitor::clean_names() %>%
    mutate(begin = ymd(begin), # format as date
           end = ymd(end))
  
  # reduce list to relevant stations
  all_isds%<>% 
    filter(ctry == "US", # keep US stations only
           begin < as.Date("2020-01-01"), # change start / end date to suit
           end > as.Date("2022-12-01"),
           !str_detect(station_name, "BUOY|PLATFORM")) # drop environmental buoys
  
  # make station code for later lookup
  all_isds %<>%
    mutate(station_code = str_c(usaf, wban, sep = "-")) %>%
    relocate(station_code, .after = wban)

  return(all_isds)
}

# function to determine closest station to a given lat/log
closest_station <- function(lat, lon) {
  
  if(!is.na(lat) & !is.na(lon)) {
    # estimate distances and find the closest
    index <- which.min(sqrt((all_isds$lat - lat)^2 + (all_isds$lon - lon)^2))
    
    # pull the station code
    all_isds[index,]$station_code
  }
}

# function to get data for all site in dataframe of lat/long pairs
# optional post processing to assemble into file and save as separate output

get_historical_weather = function(latlongs, 
                                  begin="2020-01-01",
                                  end="2021-12-31", 
                                  post_process=T){
  # create weather folder (in case none exists)
  dir.create("./weather", showWarnings = FALSE)
  
  # determine timezones of each site using lat/long
  latlongs["time_zone"] <- lutz::tz_lookup_coords(lat = latlongs$lat, lon = latlongs$long, 
                                            method = "fast", warn = FALSE)
  # associate closest station with each
  latlongs %<>%
    mutate(station_code = map2_chr(lat, long, closest_station))

  # only need unique stations
  isds_of_interest <- latlongs %>%
    distinct(station_code) %>% # unique list of stations
    arrange(station_code)
  
  print(str_c("Unique station ids:", nrow(isds_of_interest)))
  print(isds_of_interest)
  
  # start progress bar
  pb <- progress_bar$new(total = length(isds_of_interest) - 1,
                         format = " downloading [:bar] :current/:total (:percent) eta: :eta")
  
  # retrieve data and save to hdd at ./weather
  for (station in isds_of_interest) {
    pb$tick()
    worldmet::importNOAA(code = station, year = year(begin):year(end), # change date range to suit
                         path = here("./weather")) # save hourly data as .rds in 'weather' subfolder (make sure it exists)
    
  }

  if(post_process) {
    # get list of weather files in 'weather' subfolder
    weather_files <- fs::dir_ls(path = here("./weather"))
    
    # loop to load weather files into environment
    for (file in weather_files) {
      
      # read rds and select columns
      df_weather_new <- read_rds(file = file) %>%
        select(code, date, air_temp, RH) %>%
        rename("t_out" = "air_temp", "rh_out" = "RH")
      
      # combine into one table
      if (exists("df_weather")) {
        df_weather <- bind_rows(df_weather, df_weather_new)
      } else {
        df_weather <- df_weather_new
      }
    }
    
    # clean and arrange met data table
    df_weather %<>%
      rename(datetime_UTC = date,
             station_code = code)%>%
      arrange(station_code, datetime_UTC) %>%
      droplevels()
    
    # change timezone to local and convert to character
    df_weather%<>%left_join(latlongs)

    # save assembled weather data
    write_rds(df_weather, here("./weather/weather.rds"), compress = "gz")
    write_csv(df_weather, here("./weather/weather.csv"))
  }
  
}


### Example of usage ####

# create a dataframe with two columns representing a single location
# using a pair of latitude and longitude values (decimal, not degrees/seconds)

# examples for New York, Berkeley, and Hawaii

latlongs = data.frame(lat =c(38.5816), 
                      long =c(-121.4944))
# or read in from a csv
# latlongs = read_csv("./XYZ.csv", col_types = "dd")

# state start and end dates for period of interest
begin="2018-01-01"
end="2018-12-31"

# load all station ids 
all_isds  = load_isds(begin, end)

# note, need to define start and end dates for period of interest
get_historical_weather(latlongs, begin, end, post_process=T)

# read in assembled files in RDS format
df = readRDS("./weather/weather.rds")

# optional - clean up
rm(begin, end, all_isds, latlongs)
rm(closest_station, get_historical_weather,load_isds)
invisible(gc())
