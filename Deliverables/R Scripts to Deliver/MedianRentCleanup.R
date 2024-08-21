# This is a script to clean up and standardize the yearly median rent files from HUD, which were originally located here: https://www.huduser.gov/portal/datasets/50per.html#year2016. These cleaned up files will be re-saved on the computer and then pulled in to one dataframe in the script DataframeAutomation_v1.R

# set up environment
library(dplyr)
library(tidytext)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)

#### Load in HUD Data ####
# Load in HUD median cost of rent CSVs for 2001 - 2009#
for (n in 1:9) {
  name_50_rent <- paste0("med_rent_by_county_0", n)
  assign(name_50_rent, read_excel(paste0("Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY200", n, "_50th_county.xls")))
}

# Load in HUD median cost of rent CSVs for 2010-2015
for (n in 10:15) {
  name_50_rent <- paste0("med_rent_by_county_", n)
  assign(name_50_rent, read_excel(paste0("Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY20", n, "_50th_county.xls")))
}

# Load in HUD median cost of rent CSVs for 2016-2022
for (n in 16:22) {
  name_50_rent <- paste0("med_rent_by_county_", n)
  assign(name_50_rent, read_excel(paste0("Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY20", n, "_50th_county.xlsx")))
}
#### 2003 ####
# locate median rent data for the 4 counties we are interested in for 2003 - 2023
# as is, 2001 and 2002 do not have county data, need to check HUD website for this
med_rent_2003 <- med_rent_by_county_03 %>%
  filter(CountyName == "Los Angeles" | CountyName == "New York" | CountyName == "Denver" | CountyName == "King" & State_Alpha == "WA") %>%
  select("rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4", "CountyName") %>%
  mutate(year = "2003") %>%
  rename(
    med_rent_0br = rent50_0,
    med_rent_1br = rent50_1,
    med_rent_2br = rent50_2,
    med_rent_3br = rent50_3,
    med_rent_4br = rent50_4,
    county_name = CountyName
  ) %>%
  mutate(county_name = recode(county_name,
    "New York" = "New York County, New York",
    "Denver" = "Denver County, Colorado",
    "King" = "King County, Washington",
    "Los Angeles" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2003, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2003_50th_county.csv")

#### 2004 ####
# locate rent data for 2004
med_rent_2004 <- med_rent_by_county_04 %>%
  filter(CountyName == "Los Angeles County" | CountyName == "New York County" | CountyName == "Denver County" | CountyName == "King County" & State_Alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "CountyName") %>%
  mutate(year = "2004") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = CountyName
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2004, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2004_50th_county.csv")

#### 2005 ####
# locate rent data for 2005
med_rent_2005 <- med_rent_by_county_05 %>%
  filter(CountyName == "Los Angeles County" | CountyName == "New York County" | CountyName == "Denver County" | CountyName == "King County" & State_Alpha == "WA") %>%
  select("Rent50_0Bed", "Rent50_1Bed", "Rent50_2Bed", "Rent50_3Bed", "Rent50_4Bed", "CountyName") %>%
  mutate(year = "2005") %>%
  rename(
    med_rent_0br = Rent50_0Bed,
    med_rent_1br = Rent50_1Bed,
    med_rent_2br = Rent50_2Bed,
    med_rent_3br = Rent50_3Bed,
    med_rent_4br = Rent50_4Bed,
    county_name = CountyName
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2005, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2005_50th_county.csv")

#### 2006 ####
# locate rent data for 2006
med_rent_2006 <- med_rent_by_county_06 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2006") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2006, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2006_50th_county.csv")

#### 2007 ####
# locate rent data for 2007
med_rent_2007 <- med_rent_by_county_07 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2007") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2007, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2007_50th_county.csv")

#### 2008 ####
# locate rent data for 2008
med_rent_2008 <- med_rent_by_county_08 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2008") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2008, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2008_50th_county.csv")

#### 2009 ####
# locate rent data for 2009
med_rent_2009 <- med_rent_by_county_09 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2009") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2009, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2009_50th_county.csv")

#### 2010 ####
# locate rent data for 200?
med_rent_2010 <- med_rent_by_county_10 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2010") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2010, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2010_50th_county.csv")

#### 2011 ####
# locate rent data for 200?
med_rent_2011 <- med_rent_by_county_11 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2011") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2011, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2011_50th_county.csv")

#### 2012 ####
# locate rent data for 2012
med_rent_2012 <- med_rent_by_county_12 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2012") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2012, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2012_50th_county.csv")

#### 2013 ####
# locate rent data for 2013
med_rent_2013 <- med_rent_by_county_13 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2013") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2013, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2013_50th_county.csv")

#### 2014 ####
# locate rent data for 2014
med_rent_2014 <- med_rent_by_county_14 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2014") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2014, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2014_50th_county.csv")

#### 2015 ####
# locate rent data for 2015
med_rent_2015 <- med_rent_by_county_15 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2015") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2015, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2015_50th_county.csv")

#### 2016 ####
# locate rent data for 2016
med_rent_2016 <- med_rent_by_county_16 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2016") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2016, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2016_50th_county.csv")

#### 2017 ####
# locate rent data for 2017
med_rent_2017 <- med_rent_by_county_17 %>%
  filter(countyname == "Los Angeles County" | countyname == "New York County" | countyname == "Denver County" | countyname == "King County" & state_alpha == "WA") %>%
  select("Rent50_0", "Rent50_1", "Rent50_2", "Rent50_3", "Rent50_4", "countyname") %>%
  mutate(year = "2017") %>%
  rename(
    med_rent_0br = Rent50_0,
    med_rent_1br = Rent50_1,
    med_rent_2br = Rent50_2,
    med_rent_3br = Rent50_3,
    med_rent_4br = Rent50_4,
    county_name = countyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2017, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2017_50th_county.csv")

#### 2018 ####
# locate rent data for 2018
med_rent_2018 <- med_rent_by_county_18 %>%
  filter(cntyname == "Los Angeles County" | cntyname == "New York County" | cntyname == "Denver County" | cntyname == "King County" & state_alpha == "WA") %>%
  select("rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4", "cntyname") %>%
  mutate(year = "2018") %>%
  rename(
    med_rent_0br = rent50_0,
    med_rent_1br = rent50_1,
    med_rent_2br = rent50_2,
    med_rent_3br = rent50_3,
    med_rent_4br = rent50_4,
    county_name = cntyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2018, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2018_50th_county.csv")

#### 2019 ####
# locate rent data for 20?
med_rent_2019 <- med_rent_by_county_19 %>%
  filter(cntyname == "Los Angeles County" | cntyname == "New York County" | cntyname == "Denver County" | cntyname == "King County" & state_alpha == "WA") %>%
  select("rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4", "cntyname") %>%
  mutate(year = "2019") %>%
  rename(
    med_rent_0br = rent50_0,
    med_rent_1br = rent50_1,
    med_rent_2br = rent50_2,
    med_rent_3br = rent50_3,
    med_rent_4br = rent50_4,
    county_name = cntyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2019, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2019_50th_county.csv")

#### 2020 ####
# locate rent data for 2020
med_rent_2020 <- med_rent_by_county_20 %>%
  filter(cntyname == "Los Angeles County" | cntyname == "New York County" | cntyname == "Denver County" | cntyname == "King County" & state_alpha == "WA") %>%
  select("rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4", "cntyname") %>%
  mutate(year = "2020") %>%
  rename(
    med_rent_0br = rent50_0,
    med_rent_1br = rent50_1,
    med_rent_2br = rent50_2,
    med_rent_3br = rent50_3,
    med_rent_4br = rent50_4,
    county_name = cntyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2020, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2020_50th_county.csv")

#### 2021 ####
# locate rent data for 2021
med_rent_2021 <- med_rent_by_county_21 %>%
  filter(cntyname == "Los Angeles County" | cntyname == "New York County" | cntyname == "Denver County" | cntyname == "King County" & state_alpha == "WA") %>%
  select("rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4", "cntyname") %>%
  mutate(year = "2021") %>%
  rename(
    med_rent_0br = rent50_0,
    med_rent_1br = rent50_1,
    med_rent_2br = rent50_2,
    med_rent_3br = rent50_3,
    med_rent_4br = rent50_4,
    county_name = cntyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2021, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2021_50th_county.csv")

#### 2022 ####
# locate rent data for 2022
med_rent_2022 <- med_rent_by_county_22 %>%
  filter(cntyname == "Los Angeles County" | cntyname == "New York County" | cntyname == "Denver County" | cntyname == "King County" & state_code == 53) %>%
  select("rent_50_0", "rent_50_1", "rent_50_2", "rent_50_3", "rent_50_4", "cntyname") %>%
  mutate(year = "2022") %>%
  rename(
    med_rent_0br = rent_50_0,
    med_rent_1br = rent_50_1,
    med_rent_2br = rent_50_2,
    med_rent_3br = rent_50_3,
    med_rent_4br = rent_50_4,
    county_name = cntyname
  ) %>%
  mutate(county_name = recode(county_name,
    "New York County" = "New York County, New York",
    "Denver County" = "Denver County, Colorado",
    "King County" = "King County, Washington",
    "Los Angeles County" = "Los Angeles County, California"
  ))

# save cleaned up file to my drive so it is easier to load into the main automation file
write.csv(med_rent_2022, "Documents/3_Spring Quarter 2024/LIS 572 Introduction to Data Science/Final Project/median_rent_data/FY2022_50th_county.csv")
