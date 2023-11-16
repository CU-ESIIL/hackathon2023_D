# code to examine and plot water quality data from the EPA
# modified from the hackathon code provide at: https://cu-esiil.github.io/hackathon2023_datacube/code_for_building_cube/epa_water_quality/

# Set Up -------------------
install.packages("httr")
install.packages("xml2")
install.packages("tidyverse")
install.packages("ggplot2")

# load packages
library(httr)
library(xml2)
library(tidyverse)
library(ggplot2)



# Query Data --------------------------------

## Parameters for the query ---------------------------------
state_code <- "US:08" # State code for Colorado (I obtained the code from the advanced query tab by examing the URL)
characteristic_name <- "Ammonia" # Water quality characteristic

url <- paste0("https://www.waterqualitydata.us/Result/search?statecode=", state_code, "&characteristicName=", characteristic_name, "&mimeType=csv&zip=yes")

response <- GET(url)

if (status_code(response) == 200) {
  data_file <- "water_quality_data.zip"
  writeBin(content(response, "raw"), data_file)
  cat("Data downloaded successfully.\n")
  
  # Unzip the file and read the CSV
  temp_dir <- tempdir()
  unzip(data_file, exdir = temp_dir)
  csv_file <- list.files(temp_dir, pattern = "*.csv", full.names = TRUE)[1]
  water_quality_data <- read_csv(csv_file)
  
  # Plot the data
  ggplot(water_quality_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
    geom_point() +
    labs(title = "Ammonia Concentration Over Time",
         x = "Date",
         y = "Ammonia Concentration (mg/L)") +
    theme_minimal()
} else {
  cat(paste("Error:", status_code(response)), "\n")
}


# try to pull in the station identifiers to get locations 
state_code <- "US:08" # State code for Colorado (I obtained the code from the advanced query tab by examing the URL)

# bring in the station data 
station_df <- read_csv("data-store/hackathon2023_D/station.csv")

station_sub_df <- station_df %>%
  select(
    MonitoringLocationIdentifier,
    MonitoringLocationName,
    MonitoringLocationTypeName,
    LatitudeMeasure,
    LongitudeMeasure,
    HorizontalCoordinateReferenceSystemDatumName,
    `VerticalMeasure/MeasureValue`,
    `VerticalAccuracyMeasure/MeasureUnitCode`,
    CountyCode
  )

# merge data frames 

skimr::skim(water_quality_data)

water_quality_sub_df <- water_quality_data %>% 
  select(
    OrganizationFormalName,
    ActivityMediaSubdivisionName,
    ActivityStartDate,
    `ActivityStartTime/Time`,
    ActivityEndDate,
    `ActivityEndTime/Time`,
    MonitoringLocationIdentifier,
    HydrologicCondition,
    HydrologicEvent,
    CharacteristicName,
    ResultDetectionConditionText,
    `SampleCollectionMethod/MethodName`,
    ResultMeasureValue,
    `ResultMeasure/MeasureUnitCode`
  )


water_quality_wloc_df <- left_join(water_quality_sub_df, station_sub_df)

# Visualizing Stations -----------

install.packages("sf")
install.packages("terra")

library(sf)

water_quality_sf <- water_quality_wloc_df %>% 
  filter(!is.na(LongitudeMeasure) & !is.na(LatitudeMeasure)) %>%
  st_as_sf(coords = c("LatitudeMeasure", "LongitudeMeasure"), crs = "espg:4269")

ggplot(data = water_quality_sf) + 
  geom_sf(aes(color = ResultMeasureValue))
