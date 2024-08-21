# load in environment 
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(plotly)
library(fs)
library(scales)
#install.packages("git2r")
library(git2r)
#install.packages("git2rdata")
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

#### Prepare data for bar plot ####
# Need multiple iterations for filtration because otherwise yearly rent throws an error
# filter dataframe for only data from 2005 onward and for only median rent for 2 bedroom 
bar_plot_df <- init_df %>%
  filter(year >= 2005) %>%
  select("year", "county_name", "annual_average_pay", "unhoused_total","med_rent_2br") 

# change rent to be yearly and select only columns of interest
bar_plot_df <- bar_plot_df %>%
  mutate(yearly_rent = bar_plot_df$med_rent_2br * 12) %>%
  select("year", "county_name", "yearly_rent", "annual_average_pay") %>% 
  mutate_at(c("annual_average_pay"),function(x) as.numeric(as.character(gsub(",","",x))))

# combine wage and rent data into one column and add percent column
bar_plot_df <- bar_plot_df %>%
  mutate(percent_rent = round((yearly_rent/annual_average_pay)*100,2)) %>% 
  pivot_longer(yearly_rent:annual_average_pay, names_to = "variable", values_to = "value")

#### Prepare dataset for line plot ####
# filter initial dataframe for years of interest, change data type to numeric, select columns
line_plot_df <- bar_plot_df %>% 
  pivot_wider(names_from = variable, values_from = value)
  
##### Los Angeles Plot #####
# filter for only los angeles area data  
la_bar_df <- bar_plot_df %>%
  filter(county_name == "Los Angeles County, California") 

la_line_df <- line_plot_df %>% 
  filter(county_name == "Los Angeles County, California")

#### plot wage vs rent as a bar plot ####
la_bar_plot <- ggplot(la_bar_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Rent is", percent_rent, "percent of wages")
)) +
  geom_col(position = "stack") +
  labs(
    x = "Year",
    y = "USD",
    fill = "Pay/Rent"
  ) +
  scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage", "Annual Rent"))

# interactive plot
la_bar_fig <- ggplotly(style(la_bar_plot, showlegend = T), tooltip = c("text")) %>% 
layout(
  xaxis = list(title = 'Year'),
  yaxis = list(title = 'USD')
)

#### plot percent changes as a line plot ####
la_line_plot <- ggplot(la_line_df, aes(
  x = year,
  y = percent_rent,
  group = 1,
  text = paste("Year:" , year, "
Rent is", percent_rent, "percent of wages"
  )
)) +
  geom_line() 

# create interactive line plot
la_line_fig <- ggplotly(la_line_plot, tooltip = c("text")) %>% 
  layout(
    xaxis = list(title = 'Year'),
    yaxis = list(tickfont = list(size = 10), title = 'Percent of Wages')
  )

##### create facted interactive view ####
la_interactive <- subplot(la_bar_fig, la_line_fig, nrows = 1, titleY = TRUE, titleX = TRUE, margin = 0.1 ) %>% 
  layout(
    title = paste("Los Angeles County, California 2005 - 2022"),
    margin = list(t = 40),
    font = list(size = 10),
    showlegend = TRUE
  )

annotations <- list(
  list(
    x = 0.2,
    y = 1.0,
    text = "Average Wages vs. Median Rent for 2 Bedroom",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "What % of Wages is Rent",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

la_interactive <- la_interactive %>%
  layout(annotations = annotations) 

# initialize chart studio
Sys.setenv("plotly_username"="mkwilson")
Sys.setenv("plotly_api_key"="bfkiimNKwHjipA2oTLNA")

# add to chart studio so it can be added to Medium blog post 
api_create(la_interactive, filename = "la_interactive_bar_line")

##### New York Plot #####
# filter for only new york area data  
ny_bar_df <- bar_plot_df %>%
  filter(county_name == "New York County, New York") 

ny_line_df <- line_plot_df %>% 
  filter(county_name == "New York County, New York")

#### plot wage vs rent as a bar plot ####
ny_bar_plot <- ggplot(ny_bar_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Rent is", percent_rent, "percent of wages")
)) +
  geom_col(position = "stack") +
  labs(
    x = "Year",
    y = "USD",
    fill = "Pay/Rent"
  ) +
  scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage", "Annual Rent"))

# interactive plot
ny_bar_fig <- ggplotly(style(ny_bar_plot, showlegend = T), tooltip = c("text")) %>% 
  layout(
    xaxis = list(title = 'Year'),
    yaxis = list(title = 'USD')
  )

#### plot percent changes as a line plot ####
ny_line_plot <- ggplot(ny_line_df, aes(
  x = year,
  y = percent_rent,
  group = 1,
  text = paste("Year:" , year, "
Rent is", percent_rent, "percent of wages"
  )
)) +
  geom_line() 

# create interactive line plot
ny_line_fig <- ggplotly(ny_line_plot, tooltip = c("text")) %>% 
  layout(
    xaxis = list(title = 'Year'),
    yaxis = list(tickfont = list(size = 10), title = 'Percent of Wages')
  )

##### create facted interactive view ####
ny_interactive <- subplot(ny_bar_fig, ny_line_fig, nrows = 1, titleY = TRUE, titleX = TRUE, margin = 0.1 ) %>% 
  layout(
    title = paste("New York County, New York 2005 - 2022"),
    margin = list(t = 40),
    font = list(size = 10),
    showlegend = TRUE
  )

annotations <- list(
  list(
    x = 0.2,
    y = 1.0,
    text = "Average Wages vs. Median Rent for 2 Bedroom",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "What % of Wages is Rent",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

ny_interactive <- ny_interactive %>%
  layout(annotations = annotations) 

# add to chart studio so it can be added to Medium blog post 
api_create(ny_interactive, filename = "ny_interactive_bar_line")

##### Denver Plots #####
# filter for only denver area data    
den_bar_df <- bar_plot_df %>%
  filter(county_name == "Denver County, Colorado") 

den_line_df <- line_plot_df %>% 
  filter(county_name == "Denver County, Colorado")

#### plot wage vs rent as a bar plot ####
den_bar_plot <- ggplot(den_bar_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Rent is", percent_rent, "percent of wages")
)) +
  geom_col(position = "stack") +
  labs(
    x = "Year",
    y = "USD",
    fill = "Pay/Rent"
  ) +
  scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage", "Annual Rent"))

# interactive plot
den_bar_fig <- ggplotly(style(den_bar_plot, showlegend = T), tooltip = c("text")) %>% 
  layout(
    xaxis = list(title = 'Year'),
    yaxis = list(title = 'USD')
  )

#### plot percent changes as a line plot ####
den_line_plot <- ggplot(den_line_df, aes(
  x = year,
  y = percent_rent,
  group = 1,
  text = paste("Year:" , year, "
Rent is", percent_rent, "percent of wages"
  )
)) +
  geom_line() 

# create interactive line plot
den_line_fig <- ggplotly(den_line_plot, tooltip = c("text")) %>% 
  layout(
    xaxis = list(title = 'Year'),
    yaxis = list(tickfont = list(size = 10), title = 'Percent of Wages')
  )

##### create facted interactive view ####
den_interactive <- subplot(den_bar_fig, den_line_fig, nrows = 1, titleY = TRUE, titleX = TRUE, margin = 0.1 ) %>% 
  layout(
    title = paste("Denver County, Colorado 2005 - 2022"),
    margin = list(t = 40),
    font = list(size = 10),
    showlegend = TRUE
  )

annotations <- list(
  list(
    x = 0.2,
    y = 1.0,
    text = "Average Wages vs. Median Rent for 2 Bedroom",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "What % of Wages is Rent",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

den_interactive <- den_interactive %>%
  layout(annotations = annotations) 

# add to chart studio so it can be added to Medium blog post 
api_create(den_interactive, filename = "den_interactive_bar_line")

##### Seattle Plots #####
# filter for only seattle area data  
sea_bar_df <- bar_plot_df %>%
  filter(county_name == "King County, Washington") 

sea_line_df <- line_plot_df %>% 
  filter(county_name == "King County, Washington")

#### plot wage vs rent as a bar plot ####
sea_bar_plot <- ggplot(sea_bar_df, aes(
  x = year,
  y = value,
  fill = variable,
  text = paste("Rent is", percent_rent, "percent of wages")
)) + 
  scale_y_continuous(breaks=c(0,15000,30000,45000,60000,75000,90000, 105000, 120000, 135000, 150000)) +
  geom_col(position = "stack") +
  labs(
    x = "Year",
    y = "USD",
    fill = "Pay/Rent"
  ) +
  scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage", "Annual Rent"))

# interactive plot
sea_bar_fig <- ggplotly(style(sea_bar_plot, showlegend = T), tooltip = c("text")) %>% 
  layout(
    xaxis = list(title = 'Year'),
    yaxis = list(title = 'USD')
  )

#### plot percent changes as a line plot ####
sea_line_plot <- ggplot(sea_line_df, aes(
  x = year,
  y = percent_rent,
  group = 1,
  text = paste("Year:" , year, "
Rent is", percent_rent, "percent of wages"
  )
)) +
  geom_line() 

# create interactive line plot
sea_line_fig <- ggplotly(sea_line_plot, tooltip = c("text")) %>% 
  layout(
    xaxis = list(title = 'Year'),
    yaxis = list(tickfont = list(size = 10), title = 'Percent of Wages')
  )

##### create facted interactive view ####
sea_interactive <- subplot(sea_bar_fig, sea_line_fig, nrows = 1, titleY = TRUE, titleX = TRUE, margin = 0.1 ) %>% 
  layout(
    title = paste("King County, Washington 2005 - 2022"),
    margin = list(t = 40),
    font = list(size = 10),
    showlegend = TRUE
  )

annotations <- list(
  list(
    x = 0.2,
    y = 1.0,
    text = "Average Wages vs. Median Rent for 2 Bedroom",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "What % of Wages is Rent",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

sea_interactive <- sea_interactive %>%
  layout(annotations = annotations) 

# add to chart studio so it can be added to Medium blog post 
api_create(sea_interactive, filename = "sea_interactive_bar_line")

###### Combining All Into One Interactive #####
# combine all interactive together 
total_interactive <- subplot(style(la_interactive, showlegend = F), style(ny_interactive, showlegend = F), style(den_interactive, showlegend = F), sea_interactive, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.1 ) %>% 
  layout(
    title = paste("What Percent of Wages Are Rent Costs?"),
    margin = list(t = 50, b = 100),
    font = list(size = 10),
    showlegend = TRUE
  )

# add annotations to plot
annotations_total <- list(
  list(
    x = 0.2,
    y = 0.45,
    text = "Los Angeles County, California 2005 - 2022",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 0.45,
    text = "New York County, New York 2005 - 2022",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.2,
    y = -0.15,
    text = "Denver County, Colorado 2005 - 2022",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = -0.15,
    text = "King County, Washington 2005 - 2022",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
)

total_interactive <- total_interactive %>% 
  layout(annotations = annotations_total)

total_interactive

# add to chart studio so it can be added to Medium blog post 
api_create(total_interactive, filename = "total_interactive_bar_line")

