# load in environment 
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(plotly)
library(fs)
library(scales)
install.packages("git2r")
library(git2r)
install.packages("git2rdata")
library(git2rdata)

# load in dataset from github
# git link: https://github.com/maurawilson/LIS-572-Final-Project/tree/main/final_dataset
temp_import_dir <- path_temp("githubRepo_finaldf")
repo_url <- "https://github.com/maurawilson/LIS-572-Final-Project"
clone(url = repo_url, local_path = temp_import_dir)

# Getting the actual files
init_df <- dir_ls(
  path = path(temp_import_dir, "final_dataset"),
  glob = "*.csv"
) %>%
  lapply(read.csv) %>%
  bind_rows() 

##### Prepare dataset for line plot ####
# filter initial dataframe for years of interest, change data type to numeric, select columns
percent_plot_df <- init_df %>%
  mutate_at(c("annual_average_pay"),function(x) as.numeric(as.character(gsub(",","",x)))) %>% 
  filter(year >= 2005) %>%
  select("year", "county_name", "annual_average_pay", "unhoused_total","med_rent_2br") 

# change rent to yearly
percent_plot_df <- percent_plot_df %>% 
  mutate(yearly_rent = percent_plot_df$med_rent_2br * 12) 

#### Los Angeles Plot ####
# filter data for los angeles area only, add column for % change in annual pay and % change in homeless total
la_percent_df <- percent_plot_df %>% 
  filter(county_name == "Los Angeles County, California") %>% 
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_change_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_unhoused = unhoused_total - lag(unhoused_total),
    percent_change_in_unhoused = scales::percent(diff_in_unhoused / lag(unhoused_total)),
    diff_in_rent = yearly_rent - lag(yearly_rent),
    percent_change_in_rent = scales::percent(diff_in_rent / lag(yearly_rent))
  ) %>%
  filter(row_number() != 1)

# change the dataframe so pay, rent, and homeless % changes can be plotted together
la_percent_df <- la_percent_df %>%
  select("year", "county_name", "percent_change_in_pay", "percent_change_in_unhoused", "percent_change_in_rent") %>%
  pivot_longer(cols = -c(year, county_name), names_to = "category", values_to = "percent_change") 
# convert percent_change to a numeric for plotting (after taking out the "%")
la_percent_df <- la_percent_df %>% 
  mutate(percent_change = sub("%", "", la_percent_df$percent_change)) %>%
  mutate(percent_change = percent_change <- as.numeric(percent_change))

la_percent_plot <- ggplot(la_percent_df, aes(
  x = year, 
  y = percent_change,
  color = category
)) +
  geom_line() +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(-50, 30)
  ) +
  labs(
    title = "Los Angeles County, California 2005-2022",
    subtitle = "Percent Change in Wages, Rent, & Numbers of Unhoused People",
    x = "Year",
    y = "Percent Change",
    color = "% Change In:"
  ) +
  scale_color_discrete(name = "% Change In:", label = c("Annual Wage", "Annual Rent", "Unhoused Count"))

#### New York Plot ####
# filter data for new york area only, add column for % change in annual pay and % change in homeless total
ny_percent_df <- percent_plot_df %>% 
  filter(county_name == "New York County, New York") %>% 
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_change_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_unhoused = unhoused_total - lag(unhoused_total),
    percent_change_in_unhoused = scales::percent(diff_in_unhoused / lag(unhoused_total)),
    diff_in_rent = yearly_rent - lag(yearly_rent),
    percent_change_in_rent = scales::percent(diff_in_rent / lag(yearly_rent))
  ) %>%
  filter(row_number() != 1)

# change the dataframe so pay, rent, and homeless % changes can be plotted together
ny_percent_df <- ny_percent_df %>%
  select("year", "county_name", "percent_change_in_pay", "percent_change_in_unhoused", "percent_change_in_rent") %>%
  pivot_longer(cols = -c(year, county_name), names_to = "category", values_to = "percent_change") 
# convert percent_change to a numeric for plotting (after taking out the "%")
ny_percent_df <- ny_percent_df %>% 
  mutate(percent_change = sub("%", "", ny_percent_df$percent_change)) %>%
  mutate(percent_change = percent_change <- as.numeric(percent_change))

ny_percent_plot <- ggplot(ny_percent_df, aes(
  x = year, 
  y = percent_change,
  color = category
)) +
  geom_line() +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(-20, 20)
  ) +
  labs(
    title = "New York County, New York 2005-2022",
    subtitle = "Percent Change in Wages, Rent, & Numbers of Unhoused People",
    x = "Year",
    y = "Percent Change",
    color = "% Change In:"
  ) +
  scale_color_discrete(name = "% Change In:", label = c("Annual Wage", "Annual Rent", "Unhoused Count"))

#### Denver Plot ####
# filter data for denver area only, add column for % change in annual pay and % change in homeless total
den_percent_df <- percent_plot_df %>% 
  filter(county_name == "Denver County, Colorado") %>% 
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_change_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_unhoused = unhoused_total - lag(unhoused_total),
    percent_change_in_unhoused = scales::percent(diff_in_unhoused / lag(unhoused_total)),
    diff_in_rent = yearly_rent - lag(yearly_rent),
    percent_change_in_rent = scales::percent(diff_in_rent / lag(yearly_rent))
  ) %>%
  filter(row_number() != 1)

# change the dataframe so pay, rent, and homeless % changes can be plotted together
den_percent_df <- den_percent_df %>%
  select("year", "county_name", "percent_change_in_pay", "percent_change_in_unhoused", "percent_change_in_rent") %>%
  pivot_longer(cols = -c(year, county_name), names_to = "category", values_to = "percent_change") 
# convert percent_change to a numeric for plotting (after taking out the "%")
den_percent_df <- den_percent_df %>% 
  mutate(percent_change = sub("%", "", den_percent_df$percent_change)) %>%
  mutate(percent_change = percent_change <- as.numeric(percent_change))

den_percent_plot <- ggplot(den_percent_df, aes(
  x = year, 
  y = percent_change,
  color = category
)) +
  geom_line() +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(-50, 50)
  ) +
  labs(
    title = "Denver County, Colorado 2005-2022",
    subtitle = "Percent Change in Wages, Rent, & Numbers of Unhoused People",
    x = "Year",
    y = "Percent Change",
    color = "% Change In:"
  ) +
  scale_color_discrete(name = "% Change In:", label = c("Annual Wage", "Annual Rent", "Unhoused Count"))

##### Seattle Plot ####
# filter data for seattle area only, add column for % change in annual pay and % change in homeless total
sea_percent_df <- percent_plot_df %>% 
  filter(county_name == "King County, Washington") %>% 
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_change_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_unhoused = unhoused_total - lag(unhoused_total),
    percent_change_in_unhoused = scales::percent(diff_in_unhoused / lag(unhoused_total)),
    diff_in_rent = yearly_rent - lag(yearly_rent),
    percent_change_in_rent = scales::percent(diff_in_rent / lag(yearly_rent))
  ) %>%
  filter(row_number() != 1)

# change the dataframe so pay, rent, and homeless % changes can be plotted together
sea_percent_df <- sea_percent_df %>%
  select("year", "county_name", "percent_change_in_pay", "percent_change_in_unhoused", "percent_change_in_rent") %>%
  pivot_longer(cols = -c(year, county_name), names_to = "category", values_to = "percent_change") 
# convert percent_change to a numeric for plotting (after taking out the "%")
sea_percent_df <- sea_percent_df %>% 
  mutate(percent_change = sub("%", "", sea_percent_df$percent_change)) %>%
  mutate(percent_change = percent_change <- as.numeric(percent_change))

sea_percent_plot <- ggplot(sea_percent_df, aes(
  x = year, 
  y = percent_change,
  color = category
)) +
  geom_line() +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(-30, 30)
  ) +
  labs(
    title = "King County, Washington 2005-2022",
    subtitle = "Percent Change in Wages, Rent, & Numbers of Unhoused People",
    x = "Year",
    y = "Percent Change",
    color = "% Change In:"
  ) +
  scale_color_discrete(name = "% Change In:", label = c("Annual Wage", "Annual Rent", "Unhoused Count"))

#### Faceted View ####
all_plot <- la_percent_plot + ny_percent_plot + den_percent_plot + sea_percent_plot 

# initialize chart studio
Sys.setenv("plotly_username"="mkwilson")
Sys.setenv("plotly_api_key"="bfkiimNKwHjipA2oTLNA")

# add to chart studio so it can be added to Medium blog post 
api_create(all_plot, filename = "percent_plots")
