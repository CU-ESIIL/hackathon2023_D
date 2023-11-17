# Load necessary libraries
library(tidycensus)


library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)

# Set your Census API key
census_api_key("YOUR_API_HERE", install=TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# Load point data from CSV file
point_data <- read.csv("~/Desktop/heat/station.csv")

# Create an sf object for point data and set a CRS for it
library(sf)
st_crs(4326)

point_sf <- st_as_sf(point_data, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)
st_crs(point_sf) <- 4326

#update.packages(ask = FALSE, checkBuilt = TRUE)

# Pull poverty data for Colorado tracts-- if you need to do it again and it is not already in environment
#poverty <- get_acs(
  #state = 'CO',
  #geography = 'tract',
  #year = 2019,
  #geometry = TRUE,
  #variables = c(
   # in_poverty = 'B05010_002E',
   # total_pop_for_poverty_estimates = 'B05010_001E'
 # )
#)

#check the CRS for the two dataframes
st_crs(poverty)
st_crs(point_sf)

#transform them so that they are compatible
point_sf <- st_transform(point_sf, crs = st_crs(poverty))



# Merge the point data with the Census tract data
merged_data <- st_join(poverty, point_sf)

#Just to check....
colnames(merged_data)

# Create the plot
ggplot() +
  geom_sf(data = merged_data, aes(fill = percent_in_poverty), color = "white", size = 0.2) +
  geom_sf(data = point_sf, color = "red", size = 0.5) +
  scale_fill_viridis_c() + # You can use other color scales
  labs(title = "Percent in poverty with Water Quality Station Locations") +
  theme_minimal()


# combining census tract data with water quality data using the MonitoringLocationIdentifier in the merged_data file and joining it with the water data file
# Extract the variables
extracted_data <- select(merged_data, MonitoringLocationIdentifier, percent_in_poverty)

water_quality <- read.csv("~/Desktop/water_quality_wloc_colorado.csv")

merged_df <- merge(water_quality, extracted_data, by = "MonitoringLocationIdentifier", all.x = TRUE)

colnames(merged_df)


#Cleaning the Data
# Remove NAs
cleaned_df <- merged_df[!is.na(merged_df$percent_in_poverty) & !is.na(merged_df$ResultMeasureValue), ]

#If you have character (chr) columns that you want to convert to numeric (num) in a data frame, you can use the mutate function from the dplyr package. Here's an example:

# you want to convert the column to to numeric


cleaned_df <- cleaned_df %>%
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue))





summary(cleaned_df$ResultMeasureValue)

# basic scatterplot
ggplot(cleaned_df, aes(x = percent_in_poverty, y = ResultMeasureValue)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Fit a linear regression line
  labs(
    title = "Boulder Co, Detected Ammonia Level at Monitoring Station by Percent Poverty at that Location ",
    x = "Percent in Poverty",
    y = "Detected Water Ammonia Level mg/L"
  ) 

# If you want to extract statistics, you can use the lm() function
model <- lm(ResultMeasureValue ~ percent_in_poverty, data = cleaned_df)

# Print the summary of the linear regression model
summary(model)