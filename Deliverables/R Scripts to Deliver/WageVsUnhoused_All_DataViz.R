# load in environment 
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(plotly)
library(fs)
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

plot_df <- init_df %>% 
  select(year, county_name, annual_average_pay, ) %>% 
  filter(year >= 2005) %>% 
  mutate_at(c("annual_average_pay"),function(x) as.numeric(as.character(gsub(",","",x)))) %>% 
  pivot_longer(annual_average_pay:unhoused_total, names_to = "variable", values_to = "value" )

#### Los Angeles Plot ####
##Create new df with just LA area information for columns of interest 
la_plot_df <- plot_df %>% 
  filter(county_name == "Los Angeles County, California")
  
## Create line plot of average wage and number of unhoused persons per year 2005-2022.
la_plot <- ggplot(la_plot_df, aes(
  x = year,
  y = value,
  color = variable,
  group = 1,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
  )
)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Value",
    color = "Data Type"
  ) +
  scale_color_manual(labels = c("Annual Average Wage", "Unhoused Count"), values = c("darkgoldenrod4", "darksalmon"))

fig1 <- ggplotly(la_plot, tooltip = c("text")) %>% 
  layout(
    yaxis = list(tickfont = list(size = 10))
  )

#### New York Plot ####
##Create new df with just LA area information for columns of interest 
ny_plot_df <- plot_df %>% 
  filter(county_name == "New York County, New York") 

## Create line plot of average wage and number of unhoused persons per year 2005-2022.
ny_plot <- ggplot(ny_plot_df, aes(
  x = year,
  y = value,
  color = variable,
  group = 1,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
  )
)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Value",
    color = "Data Type"
  ) +
  scale_color_manual(labels = c("Annual Average Wage", "Unhoused Count"), values = c("darkgoldenrod4", "darksalmon"))

fig2 <- ggplotly(ny_plot, tooltip = c("text")) %>% 
  layout(
    yaxis = list(tickfont = list(size = 10))
  )

#### Denver Plot ####
##Create new df with just LA area information for columns of interest 
den_plot_df <- plot_df %>% 
  filter(county_name == "Denver County, Colorado")

## Create line plot of average wage and number of unhoused persons per year 2005-2022.
den_plot <- ggplot(den_plot_df, aes(
  x = year,
  y = value,
  color = variable,
  group = 1,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
  )
)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Value",
    color = "Data Type"
  ) +
  scale_color_manual(labels = c("Annual Average Wage", "Unhoused Count"), values = c("darkgoldenrod4", "darksalmon"))

fig3 <- ggplotly(den_plot, tooltip = c("text")) %>% 
  layout(
    yaxis = list(tickfont = list(size = 10))
  )

#### King County Plot ####
##Create new df with just LA area information for columns of interest 
sea_plot_df <- plot_df %>% 
  filter(county_name == "King County, Washington")

## Create line plot of average wage and number of unhoused persons per year 2005-2022.
sea_plot <- ggplot(sea_plot_df, aes(
  x = year,
  y = value,
  color = variable,
  group = 1,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
  )
)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Value",
    color = "Data Type"
  ) +
  scale_color_manual(labels = c("Annual Average Wage", "Unhoused Count"), values = c("darkgoldenrod4", "darksalmon"))

fig4 <- ggplotly(sea_plot, tooltip = c("text")) %>% 
  layout(
    yaxis = list(tickfont = list(size = 10))
  )

#### Create Faceted Interactive Plot ####
figure <- subplot(style(fig1, showlegend = F), style(fig2, showlegend = F), style(fig3, showlegend = F), fig4, nrows = 2) %>%
  layout(
    title = paste("Data on Annual Wage vs. Unhoused Count 2005 - 2022"),
    margin = list(t = 40),
    font = list(size = 12)
  )

annotations <- list(
  list(
    x = 0.2,
    y = 1.0,
    text = "Los Angeles County, California",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "New York County, New York",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.2,
    y = 0.45,
    text = "Denver County, Colorado",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 0.45,
    text = "King County, Washington",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

figure <- figure %>%
  layout(annotations = annotations) 

figure

# add to chart studio so it can be added to Medium blog post 
Sys.setenv("plotly_username"="mkwilson")
Sys.setenv("plotly_api_key"="bfkiimNKwHjipA2oTLNA")

api_create(figure, filename = "wage_vs_unhoused_plot")
