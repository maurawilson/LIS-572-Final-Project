library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)
library(plotly)

##Import df
Data <- read.csv("C:\\Users\\flemi\\OneDrive\\Documents\\MLIS\\572\\Final Project\\final_dataset.csv")

##Create new df with just LA area information.  Dataframe uses county_name column name.  

LA_df <- Data %>% filter(str_detect(county_name, "Los Angeles County, California"))

##Filter LA_df to include only annual average pay and total number of unhoused people per year.

LA_wage_unh_df <- LA_df %>% select(year, county_name, annual_average_pay, unhoused_total)

##Because some values are blank, I want to remove them to prevent a weird gap in my plot.

LA_wage_unh_df <- LA_wage_unh_df[-c(1, 2, 3, 4, 5), ]

##Reformat FMR df so that wages and number of unhoused persons are rows rather than columns for easier plotting


LA_wage_unh_df <- LA_wage_unh_df %>%
  pivot_longer(annual_average_pay:unhoused_total, ) 

##Create line plot of average wage and number of unhoused persons per year 2005-2022.
library(plotly)
  LA_wage_unh_plot <- ggplot(data = LA_wage_unh_df) +  
    geom_line(aes(x = year, y = value, color = name))
  
  ggplotly(LA_wage_unh_plot)
  
 
