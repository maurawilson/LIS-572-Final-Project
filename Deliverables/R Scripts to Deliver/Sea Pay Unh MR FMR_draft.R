library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(readr)

##Import df
Data <- read.csv("C:\\Users\\flemi\\OneDrive\\Documents\\MLIS\\572\\final_dataset.csv")

##Create new df with just Seattle area information.  Dataframe uses county_name column name.  This not entirely accurate, as the FMR and Median rent data map to metropolitan aggregate areas that are Seattle + Bellevue.  This is explained in our dataset biography and will be explained in our final project.

Seattle_df <- Data %>% filter(str_detect(county_name, "King County, Washington"))

Seattle_df <- Seattle_df %>%
  pivot_longer(annual_average_pay:unhoused_total, names_to = "Data Type", values_to = "Value") 

library(plotly)
ggplot(data = Seattle_df) +  
  geom_line(aes(x = year, y = Value, color = `Data Type`))