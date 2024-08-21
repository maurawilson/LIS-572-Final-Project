##### This is a script to automate adding all of the various CSVs and Excel Files that contain the data we need into one dataset to be used for creating visualizations. 

# set up environment
library(dplyr)
library(tidytext)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)
library(fs)
install.packages("git2r")
library(git2r)
install.packages("git2rdata")
library(git2rdata)


#### WAGE DATA AUTOMATION ####
# Read in wage csv files from Github and merge into one dataframe
# Git link: https://github.com/maurawilson/LIS-572-Final-Project/tree/main/wage_data/wage_data_csv
temp_import_dir <- path_temp("githubRepo")
repo_url <- "https://github.com/maurawilson/LIS-572-Final-Project"
clone(url = repo_url, local_path = temp_import_dir)
# Getting the actual files
git_wage_df <- dir_ls(
  path = path(temp_import_dir, "wage_data", "wage_data_csv"),
  glob = "*.csv"
) %>%
  lapply(read.csv) %>%
  bind_rows()

# Locate wage data for the 4 counties we are interested in for 2000 - 2022
merged_wage_df <- git_wage_df %>%
  filter((Industry == "Total, all industries"|Industry == "10 Total, all industries") & Ownership == "Total Covered") %>%
  filter(Area == "Los Angeles County, California" | Area == "New York County, New York" | Area == "Denver County, Colorado" | Area == "King County, Washington") %>% 
  select("Year", "Area", "Annual.Average.Pay") %>% 
  rename(
    annual_average_pay = `Annual.Average.Pay`,
    year = Year,
    county_name = Area
  )

# Save dataframe as CSV to computer
# write.csv(final_df, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/wagedf.csv")

#### MEDIAN RENT DATA ####
# Read in median rent csv files
# git link: https://github.com/maurawilson/LIS-572-Final-Project/tree/main/median_rent_data/csv
temp_import_dir <- path_temp("githubRepo_Rent")
repo_url <- "https://github.com/maurawilson/LIS-572-Final-Project"
clone(url = repo_url, local_path = temp_import_dir)
# Getting the actual files
median_rent_df <- dir_ls(
  path = path(temp_import_dir, "median_rent_data", "csv"),
  glob = "*.csv"
) %>%
  lapply(read.csv) %>%
  bind_rows()

# save dataframe as CSV to computer
# write.csv(median_rent_df, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_df.csv")

# combine wages and rent into one dataframe
combined_df <- merge(median_rent_df, merged_wage_df)

#### FAIR MARKET RENT DATA ####
# Read in FMR csv files from git: 
# git link: https://github.com/maurawilson/LIS-572-Final-Project/tree/main/fmr_data/csv
temp_import_dir <- path_temp("githubRepo_FMR")
repo_url <- "https://github.com/maurawilson/LIS-572-Final-Project"
clone(url = repo_url, local_path = temp_import_dir)
# Getting the actual files
fmr_df <- dir_ls(
  path = path(temp_import_dir, "fmr_data", "csv"),
  glob = "*.csv"
) %>%
  lapply(read.csv) %>%
  bind_rows() %>% 
  select("fmr_0br","fmr_1br","fmr_2br","fmr_3br","fmr_4br", "county_name", "year")

# save dataframe as CSV to computer
# write.csv(fmr_df, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_df.csv")

# combine fmr into previously compiled dataframe
combined_df <- merge(combined_df, fmr_df)

#### DATA ON NUMBER OF UNHOUSED PEOPLE ####
# Read in data on unhoused count from 2005 - 2022 from git
# git link: https://github.com/maurawilson/LIS-572-Final-Project/blob/main/unhoused_populations.csv
temp_import_dir <- path_temp("githubRepo_unhoused")
repo_url <- "https://github.com/maurawilson/LIS-572-Final-Project"
clone(url = repo_url, local_path = temp_import_dir)
# Getting the actual files
unhoused_df <- dir_ls(
  path = path(temp_import_dir),
  glob = "*.csv"
) %>%
  lapply(read.csv) %>%
  bind_rows()

#unhoused_df <- read.csv("https://github.com/maurawilson/LIS-572-Final-Project/main/unhoused_populations.csv")

# combine unhoused data into previously compiled dataframe and save as final_df
final_dataframe <- merge(combined_df, unhoused_df)

#### EXPORT OUT COMPILED DATASET ####
# as written the path is to save the file on maura's computer.
write.csv(final_dataframe, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/final_dataframe.csv")

