library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(readr)

##Import df
Data <- read.csv("C:\\Users\\flemi\\OneDrive\\Documents\\MLIS\\572\\final_dataset.csv")

##Create new df with just Seattle area information.  Dataframe uses county_name column name.  This not entirely accurate, as the FMR and Median rent data map to metropolitan aggregate areas that are Seattle + Bellevue.  This is explained in our dataset biography and will be explained in our final project.

Seattle_df <- Data %>% filter(str_detect(county_name, "King County, Washington"))

##Filter Seattle_FMR_df to include only the FMR rates for each year.

Seattle_FMR_df <- Seattle_df %>% select(year, county_name, fmr_0br, fmr_1br, fmr_2br, fmr_3br, fmr_4br)

##Because some values are blank, I want to remove them to prevent a weird gap in my plot.

Seattle_FMR_df <- Seattle_FMR_df[-c(1, 2, 3, 4), ]

##Reformat FMR df so that apartment size is a row, not a column, for easier plotting


Seattle_FMR_df <- Seattle_FMR_df %>%
  pivot_longer(fmr_0br:fmr_4br, names_to = "Apartment_Size", values_to = "FMR") 

##Create line plot of FMRs (all sizes) for Seattle area from 2000-2022.
library(plotly)
  ggplot(data = Seattle_FMR_df) +  
    geom_line(aes(x = year, y = FMR, color = Apartment_Size))
