install.packages("tidycensus")
install.packages("tidyverse")

library(tidycensus)
library(tidyverse)

# Set your Census API key
census_api_key("ca98ed282d04f9e94ec09dbcccbf7583fcb703a7", install=TRUE)
readRenviron("~/.Renviron")

# Get the total population for the state of California
ca_pop <- get_acs(
  geography = "state",
  variables = "B01003_001",
  state = "CA"
) %>% 
  rename(total_population = estimate) %>% 
  select(total_population)

# View the result
ca_pop


install.packages("sf")
install.packages("RColorBrewer")
library("mapview")


library(tidycensus)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(mapview)