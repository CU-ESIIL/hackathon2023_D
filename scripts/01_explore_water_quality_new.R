
# set working directory:



# code to examine and plot water quality data from the EPA
# modified from the hackathon code provide at: https://cu-esiil.github.io/hackathon2023_datacube/code_for_building_cube/epa_water_quality/

# Set Up -------------------
# load packages
library(tidyverse)

# spatial packages
library(sf)
library(httr)
library(xml2)

# read in the water quality data
state_code <- "US:08"
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


# bring in the station data 
# may need to modify the path based on your working directory
station_df <- read_csv("station.csv")

station_sub_df <- station_df %>%
  dplyr::select(
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

water_quality_sub_df <- water_quality_data %>% 
  dplyr::select(
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
write_csv(water_quality_wloc_df, "water_quality_wloc_colorado.csv")

# Visualizing Stations -----------

library(raster)
us <- getData("GADM",country="USA",level=1)

us.states <- us[us$NAME_1 %in% "Colorado",]

library(sf)

water_quality_sf <- water_quality_wloc_df %>% 
  filter(!is.na(LongitudeMeasure) & !is.na(LatitudeMeasure)) %>%
  st_as_sf(coords = c( "LongitudeMeasure", "LatitudeMeasure"), crs = "epsg:4269") %>% 
  mutate(value = as.numeric(ResultMeasureValue))

CO_sf <- us.states %>% 
  st_as_sf(crs = "epsg:4326") %>% 
  st_transform(st_crs(water_quality_sf))

CO_sf_3857 <- st_transform(CO_sf, crs = "epsg:3857")
water_quality_sf_3857 <- st_transform(water_quality_sf, crs = "epsg:3857")

ggplot() + 
  ggspatial::annotation_map_tile(type = "hotstyle", zoom = 7) +
  geom_sf(data = water_quality_sf_3857, aes(shape = MonitoringLocationTypeName, 
                                            fill = MonitoringLocationTypeName)) +
  geom_sf(data = CO_sf_3857, fill = NA) +
  guides(shape = guide_legend(ncol = 2, title = "Location Type"),
         fill = guide_legend(ncol = 2, title = "Location Type")) +
  scale_shape_manual(name = "Location Type", 
                     values = c(2, 3, 1, 15, 21, 25, 13, 8)) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave("colorado_water_quality_overview.png", units = "in", width = 6, height = 6)
# filter based on thresholds
# for Ammonia, acute threshold is 17 mg/L ; 1.9 mg/L chronic (30-day average)


# now make a plot with the max N value for each location

water_quality_3857_sum_sf <- water_quality_sf_3857 %>% 
  group_by(MonitoringLocationIdentifier, MonitoringLocationTypeName) %>%
  summarize(max_N = max(value))

ggplot() + 
  ggspatial::annotation_map_tile(type = "hotstyle", zoom = 7) +
  geom_sf(data = water_quality_3857_sum_sf, aes(fill = max_N, size = max_N), shape = 21) +
  geom_sf(data = CO_sf_3857, fill = NA) +
  scale_size_continuous(name = "Max N") +
  scale_fill_stepsn(name = "Max N", breaks = c(0, 5, 10, 15, 20), colors = RColorBrewer::brewer.pal(5, "Purples")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggspatial::annotation_north_arrow(pad_y = unit(0.75, "cm")) +
  ggspatial::annotation_scale()

ggsave("colorado_ammonia_max.png", units = "in", width = 6, height = 6)
