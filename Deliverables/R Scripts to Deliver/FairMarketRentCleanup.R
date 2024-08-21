# This is a script to clean up and standardize the yearly fair market rent files from HUD, which were originally located here: https://www.huduser.gov/portal/datasets/fmr.html#data_2024. These cleaned up files will be re-saved on the computer and then pulled in to one dataframe in the script DataframeAutomation.R

# set up environment
library(dplyr)
library(tidytext)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)

#### LOAD IN HUD DATA ON FMR ####
# Load in HUD FMR of rent CSVs for 2004 - 2009
for (n in 4:9) {
  name_fmr <- paste0("fmr_by_county_0", n)
  assign(name_fmr, read_excel(paste0("Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/full_excels/FMR200", n, "_county.xls")))
}

# Load in HUD median cost of rent CSVs for 2010-2015
for (n in 10:15) {
  name_fmr <- paste0("fmr_by_county_", n)
  assign(name_fmr, read_excel(paste0("Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/full_excels/FMR20", n, "_county.xls")))
}

# Load in HUD median cost of rent CSVs for 2016-2022
for (n in 16:22) {
  name_fmr <- paste0("fmr_by_county_", n)
  assign(name_fmr, read_excel(paste0("Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/full_excels/FMR20", n, "_county.xlsx")))
}


#### 2004 ####
# locate fair market rent data for the 4 counties we are interested in for 2003 - 2022
# as is, 2000-2003 do not have county data, need to check HUD website for this
fmr_2004 <- fmr_by_county_04 %>%
  filter(CountyName == "Los Angeles County" | CountyName == "New York County" | CountyName == "Denver County" | CountyName == "King County" & State_Alpha == "WA") %>%
  select("New_FMR0", "New_FMR1", "New_FMR2", "New_FMR3", "New_FMR4", "CountyName") %>%
  mutate(year = "2004") %>%
  rename(
    fmr_0br = New_FMR0,
    fmr_1br = New_FMR1,
    fmr_2br = New_FMR2,
    fmr_3br = New_FMR3,
    fmr_4br = New_FMR4,
    county_name = CountyName
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2004, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2004_county.csv")

#### 2005 ####
# locate fmr data for 2005
fmr_2005 <- fmr_by_county_05 %>%
  filter(CountyName == "Los Angeles County" | CountyName == "New York County" | CountyName == "Denver County" | CountyName == "King County" & State_Alpha == "WA") %>%
  select("FMR_0Bed", "FMR_1Bed", "FMR_2Bed", "FMR_3Bed", "FMR_4Bed", "CountyName") %>%
  mutate(year = "2005") %>%
  rename(
    fmr_0br = FMR_0Bed,
    fmr_1br = FMR_1Bed,
    fmr_2br = FMR_2Bed,
    fmr_3br = FMR_3Bed,
    fmr_4br = FMR_4Bed,
    county_name = CountyName
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2005, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2005_county.csv")

#### 2006 ####
# locate fmr data for 2006
fmr_2006 <- fmr_by_county_06 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2006") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2006, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2006_county.csv")

#### 2007 ####
# locate fmr data for 2007
fmr_2007 <- fmr_by_county_07 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2007") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2007, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2007_county.csv")

#### 2008 ####
# locate fmr data for 2008
fmr_2008 <- fmr_by_county_08 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2008") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2008, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2008_county.csv")

#### 2009 ####
# locate fmr data for 2009
fmr_2009 <- fmr_by_county_09 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2009") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2009, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2009_county.csv")

#### 2010 ####
# locate fmr data for 2010
fmr_2010 <- fmr_by_county_10 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2010") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2010, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2010_county.csv")

#### 2011 ####
# locate fmr data for 2011
fmr_2011 <- fmr_by_county_11 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2011") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2011, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2011_county.csv")

#### 2012 ####
# locate fmr data for 2012
fmr_2012 <- fmr_by_county_12 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2012") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2012, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2012_county.csv")

#### 2013 ####
# locate fmr data for 2013
fmr_2013 <- fmr_by_county_13 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2013") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2013, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2013_county.csv")

#### 2014 ####
# locate fmr data for 2014
fmr_2014 <- fmr_by_county_14 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2014") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2014, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2014_county.csv")

#### 2015 ####
# locate fmr data for 2015
fmr_2015 <- fmr_by_county_15 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2015") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2015, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2015_county.csv")

#### 2016 ####
# locate fmr data for 2016
fmr_2016 <- fmr_by_county_16 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2016") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2016, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2016_county.csv")

#### 2017 ####
# locate fmr data for 2017
fmr_2017 <- fmr_by_county_17 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr0", "fmr1", "fmr2", "fmr3", "fmr4", "countyname") %>%
  mutate(year = "2017") %>%
  rename(
    fmr_0br = fmr0,
    fmr_1br = fmr1,
    fmr_2br = fmr2,
    fmr_3br = fmr3,
    fmr_4br = fmr4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2017, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2017_county.csv")

#### 2018 ####
# locate fmr data for 2018
fmr_2018 <- fmr_by_county_18 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr_0", "fmr_1", "fmr_2", "fmr_3", "fmr_4", "countyname") %>%
  mutate(year = "2018") %>%
  rename(
    fmr_0br = fmr_0,
    fmr_1br = fmr_1,
    fmr_2br = fmr_2,
    fmr_3br = fmr_3,
    fmr_4br = fmr_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2018, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2018_county.csv")

#### 2019 ####
# locate fmr data for 2019
fmr_2019 <- fmr_by_county_19 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr_0", "fmr_1", "fmr_2", "fmr_3", "fmr_4", "countyname") %>%
  mutate(year = "2019") %>%
  rename(
    fmr_0br = fmr_0,
    fmr_1br = fmr_1,
    fmr_2br = fmr_2,
    fmr_3br = fmr_3,
    fmr_4br = fmr_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2019, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2019_county.csv")

#### 2020 ####
# locate fmr data for 2020
fmr_2020 <- fmr_by_county_20 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr_0", "fmr_1", "fmr_2", "fmr_3", "fmr_4", "countyname") %>%
  mutate(year = "2020") %>%
  rename(
    fmr_0br = fmr_0,
    fmr_1br = fmr_1,
    fmr_2br = fmr_2,
    fmr_3br = fmr_3,
    fmr_4br = fmr_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2020, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2020_county.csv")

#### 2021 ####
# locate fmr data for 2021
fmr_2021 <- fmr_by_county_21 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr_0", "fmr_1", "fmr_2", "fmr_3", "fmr_4", "countyname") %>%
  mutate(year = "2021") %>%
  rename(
    fmr_0br = fmr_0,
    fmr_1br = fmr_1,
    fmr_2br = fmr_2,
    fmr_3br = fmr_3,
    fmr_4br = fmr_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2021, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2021_county.csv")

#### 2022 ####
# locate fmr data for 2022
fmr_2022 <- fmr_by_county_22 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("fmr_0", "fmr_1", "fmr_2", "fmr_3", "fmr_4", "countyname") %>%
  mutate(year = "2022") %>%
  rename(
    fmr_0br = fmr_0,
    fmr_1br = fmr_1,
    fmr_2br = fmr_2,
    fmr_3br = fmr_3,
    fmr_4br = fmr_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(fmr_2022, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/fmr_data/csv/FMR2022_county.csv")
