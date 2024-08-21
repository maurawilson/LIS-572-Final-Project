#### This is a script to create visuals for each city based on the final dataset created in the script DataframeAutomation.R

# set up environment
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)
library("scales")
install.packages("patchwork")
library(patchwork)
install.packages("plotly")
library(plotly)

#### Prepare Data for Plotting ####
# load in dataset via csv
init_df <- read.csv("Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/final_dataset.csv", stringsAsFactors = FALSE)

# Need multiple iterations for filtration because otherwise yearly rent throws an error
# filter dataframe for only data from 2005 onward
plot_df <- init_df %>%
  filter(year >= 2005) %>%
  pivot_longer(med_rent_0br:fmr_4br, names_to = "apartment_size", values_to = "rent") %>%
  select("year", "county_name", "apartment_size", "rent", "annual_average_pay", "unhoused_total")

# change rent to be yearly and select only columns of interest
plot_df <- plot_df %>%
  mutate(yearly_rent = plot_df$rent * 12) %>%
  select("year", "county_name", "apartment_size", "yearly_rent", "annual_average_pay", "unhoused_total")

# pivot data so there is only two columns of interest
plot_df <- plot_df %>%
  pivot_wider(names_from = apartment_size, values_from = yearly_rent) %>%
  select("year", "county_name", "fmr_2br", "med_rent_2br", "annual_average_pay","unhoused_total") %>% 
  pivot_longer(fmr_2br:unhoused_total, names_to = "variable", values_to = "value")

#### Los Angeles Plot ####
# Filter out dataframe to only be for Los Angeles
la_df <- plot_df %>%
  filter(county_name == "Los Angeles County, California")

#Turn your 'treatment' column into a character vector
la_df$variable <- as.character(la_df$variable)

#Then turn it back into a factor with the levels in the correct order
la_df$variable <- factor(la_df$variable, levels=unique(la_df$variable))

# plot data as a bar plot
la_plot <- ggplot(la_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
  )
)) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  labs(
    #title = "Los Angeles County, California 2005 - 2022",
    #subtitle = "Data on Annual Wage, Median and Fair Market Rent, and Unhoused Count",
    x = "Year",
    y = "Value",
    fill = "Data Type"
  ) +
  scale_fill_manual(labels = c("Fair Market Rent - 2 Bedroom", "Median Rent - 2 Bedroom", "Annual Average Wage", "Unhoused Count"), values = c("indianred1", "cyan4", "darkgoldenrod4", "darksalmon"))

fig1 <- ggplotly(la_plot, tooltip = c("text"))

#### New York Plot ####
# Filter out dataframe to only be for New York
ny_df <- plot_df %>%
  filter(county_name == "New York County, New York")

# ensure plot orders bars in order of appearance, not alphabeticak
ny_df$variable <- as.character(ny_df$variable)
ny_df$variable <- factor(ny_df$variable, levels=unique(ny_df$variable))

# plot data as a bar plot
ny_plot <- ggplot(ny_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
  )
)) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  labs(
    #title = "New York County, New York 2005 - 2022",
    #subtitle = "Data on Annual Wage, Median and Fair Market Rent, and Unhoused Count",
    x = "Year",
    y = "Value",
    fill = "Data Type"
  ) +
  scale_fill_manual(labels = c("Fair Market Rent - 2 Bedroom", "Median Rent - 2 Bedroom", "Annual Average Wage", "Unhoused Count"), values = c("indianred1", "cyan4", "darkgoldenrod4", "darksalmon"))

fig2 <- ggplotly(ny_plot, tooltip = c("text"))

#### Denver Plot ####
# Filter out dataframe to only be for Denver
den_df <- plot_df %>%
  filter(county_name == "Denver County, Colorado")

# ensure plot orders bars in order of appearance, not alphabeticak
den_df$variable <- as.character(den_df$variable)
den_df$variable <- factor(den_df$variable, levels=unique(den_df$variable))

# plot data as a bar plot
den_plot <- ggplot(den_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
))) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  labs(
    #title = "Denver County, Colorado 2005 - 2022",
    #subtitle = "Data on Annual Wage, Median and Fair Market Rent, and Unhoused Count",
    x = "Year",
    y = "Value",
    fill = "Data Type"
  ) +
  scale_fill_manual(labels = c("Fair Market Rent - 2 Bedroom", "Median Rent - 2 Bedroom", "Annual Average Wage", "Unhoused Count"), values = c("indianred1", "cyan4", "darkgoldenrod4", "darksalmon"))

fig3 <- ggplotly(den_plot, tooltip = c("text")) %>% 
  layout(
    title = list(text = paste0(
      'Denver County, Colorado 2005 - 2022',
      '<br>',
      '<sup>',
      'Data on Annual Wage, Median and Fair Market Rent, and Unhoused Count',
      '</sup>'
    ))
  )

#### Seattle Plot ####
# Filter out dataframe to only be for Seattle
sea_df <- plot_df %>%
  filter(county_name == "King County, Washington")

# ensure plot orders bars in order of appearance, not alphabeticak
sea_df$variable <- as.character(sea_df$variable)
sea_df$variable <- factor(sea_df$variable, levels=unique(sea_df$variable))

# plot data as a bar plot
sea_plot <- ggplot(sea_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Data Type:", variable, "
Year:", year, "
Value:", value
  )
)) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  labs(
    #title = "King County, Washington 2005 - 2022",
    #subtitle = "Data on Annual Wage, Median and Fair Market Rent, and Unhoused Count",
    x = "Year",
    y = "Value",
    fill = "Data Type"
  ) +
  scale_fill_manual(labels = c("Fair Market Rent - 2 Bedroom", "Median Rent - 2 Bedroom", "Annual Average Wage", "Unhoused Count"), values = c("indianred1", "cyan4", "darkgoldenrod4", "darksalmon"))

fig4 <- ggplotly(sea_plot, tooltip = c("text"))

#### Create Faceted Interactive Plot ####
figure <- subplot(style(fig1, showlegend = F), style(fig2, showlegend = F), style(fig3, showlegend = F), fig4, nrows = 2) %>%
  layout(
    title = paste("Data on Annual Wage, Median and Fair Market Rent, and Unhoused Count 2005 - 2022"),
    margin = list(t = 40),
    font = list(size = 10)
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

api_create(figure, filename = "alldata_plot")
