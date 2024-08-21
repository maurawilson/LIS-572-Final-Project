library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(patchwork)
library(plotly)



##Import df
Data <- read.csv("C:\\Users\\flemi\\OneDrive\\Documents\\MLIS\\572\\Final Project\\final_dataset.csv")

##Create new df with just Seattle area information.  Dataframe uses county_name column name.  This not entirely accurate, as the FMR and Median rent data map to metropolitan aggregate areas that are Seattle + Bellevue.  This is explained in our dataset biography and will be explained in our final project.

Seattle_df <- Data %>% filter(str_detect(county_name, "King County, Washington"))

##Filter Seattle_FMR_df to include average wage, total unhoused persons, Median 2 bedroom rent, and Fair Market 2 bedroom rent for each year.

Seattle_multivar_df <- Seattle_df %>% select(year, county_name, annual_average_pay, unhoused_total, med_rent_2br, fmr_2br)

##Because some values are blank, I want to remove them to prevent a weird gap in my plot.

Seattle_multivar_df <- Seattle_multivar_df[-c(1, 2, 3, 4, 5), ]

##Change rents to yearly amounts to match yearly pay data

Seattle_multivar_df <- Seattle_multivar_df %>% 
                  mutate(yearly_Med_rent_2br = med_rent_2br*12)
Seattle_multivar_df <- Seattle_multivar_df %>% 
                  mutate(yearly_FMR_rent_2br = fmr_2br*12)

## Remove monthly rent columns

Seattle_multivar_df <- Seattle_multivar_df %>% 
                    select(-(med_rent_2br : fmr_2br))

##Remove unhoused total column

Seattle_multivar_df <- Seattle_multivar_df %>% 
                        select(-unhoused_total)

##Reformat FMR df so that variables are rows as opposed to columns, for easier plotting


Seattle_multivar_df <- Seattle_multivar_df %>%
  pivot_longer(annual_average_pay:yearly_FMR_rent_2br, ) 

##Create bar plot of annual average pay and median and fair market rents for Seattle area from 2005-2022. Add tooltip.

  ggplot(data = Seattle_multivar_df) +
        geom_col(mapping = aes(x = year, y = value, fill = name))
  

  
## I have tried to add tooltip using multiple different syntax and nothing works.  I can't figure out what's wrong.
