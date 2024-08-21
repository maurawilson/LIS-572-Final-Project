library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)
library(plotly)

##Import df
Data <- read.csv("C:\\Users\\flemi\\OneDrive\\Documents\\MLIS\\572\\Final Project\\final_dataset.csv")

##Create new df with just Seattle- Bellevue area information.  Dataframe uses county_name column name, but note Seattle-Bellevue is not actually all of King County.  

Seattle_df <- Data %>% filter(str_detect(county_name, "King County, Washington"))

##Filter Seattle_df to include only annual average pay and total number of unhoused people per year.

Seattle_wage_unh_df <- Seattle_df %>% select(year, county_name, annual_average_pay, unhoused_total)

##Because some values are blank, I want to remove them to prevent a weird gap in my plot.

Seattle_wage_unh_df <- Seattle_wage_unh_df[-c(1, 2, 3, 4, 5), ]

##Reformat FMR df so that wages and number of unhoused persons are rows rather than columns for easier plotting


Seattle_wage_unh_df <- Seattle_wage_unh_df %>%
  pivot_longer(annual_average_pay:unhoused_total, ) 

##Create line plot of average wage and number of unhoused persons per year 2005-2022.
library(plotly)
  Seattle_wage_unh_plot <- ggplot(data = Seattle_wage_unh_df) +  
    geom_line(aes(x = year, y = value, color = name))
  
  ggplotly(Seattle_wage_unh_plot)
  

