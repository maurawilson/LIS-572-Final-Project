
install.packages("tidytext")
install.packages("tidyverse")
install.packages("plotyly")
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library("plotly")
library("scales")
#
#SEPARATE EACH COUNTY INTO A DATAFRAME
#
#DENVER
#get just DENVER COUNTY data from dataset and only year, rent, and unhoused columns
denver <- final_df %>% filter(county_name=="Denver County, Colorado", year>2004) %>% 
  select(county_name, year, med_rent_0br,med_rent_1br, med_rent_2br, med_rent_3br, med_rent_4br, annual_average_pay, unhoused_total)
#
#KING COUNTY
#get just KING COUNTY data from dataset and only year, rent, and unhoused columns
king_county <- final_df %>% filter(county_name=="King County, Washington", year>2004) %>% 
  select(county_name, year, med_rent_0br,med_rent_1br, med_rent_2br, med_rent_3br, med_rent_4br, annual_average_pay, unhoused_total)
#
#LOS ANGELES
#get just LOS ANGELES COUNTY data from dataset and only year, rent, and unhoused columns
los_angeles <- final_df %>% filter(county_name=="Los Angeles County, California", year>2004) %>% 
  select(county_name, year, med_rent_0br,med_rent_1br, med_rent_2br, med_rent_3br, med_rent_4br, annual_average_pay, unhoused_total)
#
#NEW YORK
#get just NEW YORK data from dataset and only year, rent, and unhoused columns
new_york <- final_df %>% filter(county_name=="New York County, New York", year>2004) %>% 
  select(county_name, year, med_rent_0br,med_rent_1br, med_rent_2br, med_rent_3br, med_rent_4br, annual_average_pay, unhoused_total)
#
#FOR EACH COUNTY
#change annual average pay to a numeric field
denver <- denver %>% 
  mutate_at(c("annual_average_pay"),function(x) 
    as.numeric(as.character(gsub(",","",x))))
king_county <- king_county %>% 
  mutate_at(c("annual_average_pay"),function(x) 
    as.numeric(as.character(gsub(",","",x))))
los_angeles <- los_angeles %>% 
  mutate_at(c("annual_average_pay"),function(x) 
    as.numeric(as.character(gsub(",","",x))))
new_york <- new_york %>% 
  mutate_at(c("annual_average_pay"),function(x) 
    as.numeric(as.character(gsub(",","",x))))
#
#FOR EACH COUNTY
#make a row for each type of rent to calculate annual rent
#used values_transform to keep monthly_rent as numeric
#
#DENVER
denver_annual_rent <- denver %>% pivot_longer(
  cols = -c(county_name, year, unhoused_total, annual_average_pay),
  names_to = "rent_type", values_to = "monthly_rent"
 # values_transform = list(val = as.numeric)
) 
#
#KING COUNTY
king_county_annual_rent <- king_county %>% pivot_longer(
  cols = -c(county_name, year, unhoused_total, annual_average_pay),
  names_to = "rent_type", values_to = "monthly_rent")
#
#LOS ANGELES
los_angeles_annual_rent <- los_angeles %>% pivot_longer(
  cols = -c(county_name, year, unhoused_total, annual_average_pay),
  names_to = "rent_type", values_to = "monthly_rent")
#
#NEW YORK
new_york_annual_rent <- new_york %>% pivot_longer(
  cols = -c(county_name, year, unhoused_total, annual_average_pay),
  names_to = "rent_type", values_to = "monthly_rent")
#
#FOR ALL COUNTIES
#calculate annual rent from monthly rent
#DENVER
denver_annual_rent <-denver_annual_rent %>% mutate(annual_rent = monthly_rent*12)
#use only 2 bedroom rent data
denver_rent_2br <- denver_annual_rent %>% filter(rent_type=="med_rent_2br")
#
#add column for % change in annual pay and % change in homeless total
denver_percent_changes <- denver_rent_2br %>%
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_increase_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_homeless = unhoused_total - lag(unhoused_total),
    percent_increase_homeless = scales::percent(diff_in_homeless / lag(unhoused_total)),
    diff_in_rent = annual_rent - lag(annual_rent),
    percent_increase_rent = scales::percent(diff_in_rent / lag(annual_rent))
  ) %>%
  filter(row_number() != 1)
#
#KING COUNTY
king_county_annual_rent <-king_county_annual_rent %>% mutate(annual_rent = monthly_rent*12)
#use only 2 bedroom rent data
king_county_rent_2br <- king_county_annual_rent %>% filter(rent_type=="med_rent_2br")
#
#add column for % change in annual pay and % change in homeless total
king_county_percent_changes <- king_county_rent_2br %>%
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_increase_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_homeless = unhoused_total - lag(unhoused_total),
    percent_increase_homeless = scales::percent(diff_in_homeless / lag(unhoused_total)),
    diff_in_rent = annual_rent - lag(annual_rent),
    percent_increase_rent = scales::percent(diff_in_rent / lag(annual_rent))
  ) %>%
  filter(row_number() != 1)
#
#LOS ANGELES COUNTY
los_angeles_annual_rent <-los_angeles_annual_rent %>% mutate(annual_rent = monthly_rent*12)
#use only 2 bedroom rent data
los_angeles_rent_2br <- los_angeles_annual_rent %>% filter(rent_type=="med_rent_2br")
#
#add column for % change in annual pay and % change in homeless total
los_angeles_percent_changes <- los_angeles_rent_2br %>%
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_increase_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_homeless = unhoused_total - lag(unhoused_total),
    percent_increase_homeless = scales::percent(diff_in_homeless / lag(unhoused_total)),
    diff_in_rent = annual_rent - lag(annual_rent),
    percent_increase_rent = scales::percent(diff_in_rent / lag(annual_rent))
  ) %>%
  filter(row_number() != 1)
#
#
#NEW YORK COUNTY
new_york_annual_rent <-new_york_annual_rent %>% mutate(annual_rent = monthly_rent*12)
#use only 2 bedroom rent data
new_york_rent_2br <- los_angeles_annual_rent %>% filter(rent_type=="med_rent_2br")
#
#add column for % change in annual pay and % change in homeless total
new_york_percent_changes <- new_york_rent_2br %>%
  mutate(
    diff_in_pay = annual_average_pay - lag(annual_average_pay),
    percent_increase_in_pay = scales::percent(diff_in_pay / lag(annual_average_pay)),
    diff_in_homeless = unhoused_total - lag(unhoused_total),
    percent_increase_homeless = scales::percent(diff_in_homeless / lag(unhoused_total)),
    diff_in_rent = annual_rent - lag(annual_rent),
    percent_increase_rent = scales::percent(diff_in_rent / lag(annual_rent))
  ) %>%
  filter(row_number() != 1)

#
#FOR EACH COUNTY
#change the dataframe so pay, rent, and homeless % changes can be plotted together
#convert percent_change to a numeric for plotting (after taking out the "%")
#
#DENVER
denver_changes <- denver_percent_changes %>%
  select(year, percent_increase_in_pay, percent_increase_homeless, percent_increase_rent) %>%
  pivot_longer(cols = -c(year), names_to = "category", values_to = "percent_change") %>% 
mutate(percent_change=sub("%","",denver_changes$percent_change)) %>% 
  mutate(percent_change=percent_change<-as.numeric(percent_change))
#
#KING COUNTY
king_county_changes <- king_county_percent_changes %>%
  select(year, percent_increase_in_pay, percent_increase_homeless, percent_increase_rent) %>%
  pivot_longer(cols = -c(year), names_to = "category", values_to = "percent_change")
king_county_changes <- king_county_changes %>% 
  mutate(percent_change=sub("%","",king_county_changes$percent_change)) %>% 
  mutate(percent_change=percent_change<-as.numeric(percent_change))
#
#LOS ANGELES COUNTY
los_angeles_changes <- los_angeles_percent_changes %>%
  select(year, percent_increase_in_pay, percent_increase_homeless, percent_increase_rent) %>%
  pivot_longer(cols = -c(year), names_to = "category", values_to = "percent_change")
los_angeles_changes <- los_angeles_changes %>% 
  mutate(percent_change=sub("%","",los_angeles_changes$percent_change)) %>% 
  mutate(percent_change=percent_change<-as.numeric(percent_change))
#
#
#NEW YORK COUNTY
new_york_changes <- new_york_percent_changes %>%
  select(year, percent_increase_in_pay, percent_increase_homeless, percent_increase_rent) %>%
  pivot_longer(cols = -c(year), names_to = "category", values_to = "percent_change")
new_york_changes <- new_york_changes %>% 
  mutate(percent_change=sub("%","",new_york_changes$percent_change)) %>% 
  mutate(percent_change=percent_change<-as.numeric(percent_change))
#
#
#FOR EACH COUNTY
#LINE PLOT WITH % CHANGE FOR ALL VARIABLES
#plot % changes for pay, rent and homeless 
#
#DENVER
ggplot (data=denver_changes)+
  geom_line(aes(x=year, y =percent_change, group=category, color=category))+
  scale_y_continuous(labels=scales::percent)+
  labs(title="Denver County 2005-2023",
       subtitle="Percent Change in Wages, Rent, & Homless Numbers",
       x="Year",
       y="Percent Change",
       color="% Increase/Decrease in:") +
  scale_color_discrete(name = "% Increase/Decrease in:", label = c("Homeless Count", "Annual Wage", "Annual Rent"))
#
#
#KING COUNTY
ggplot (data=king_county_changes)+
  geom_line(aes(x=year, y =percent_change, group=category, color=category))+
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                     limits=c(-20,50))+
 # scale_y_continuous(labels=scales::percent)
#breaks= seq(-10, 25, .5),
#labels=scales::percent,
#limits = c(-50, 25))
  labs(title="King County 2005-2023",
       subtitle="Percent Change in Wages, Rent, & Homless Numbers",
       x="Year",
       y="Percent Change",
       color="% Increase/Decrease in:") +
  scale_color_discrete(name = "% Increase/Decrease in:", label = c("Homeless Count", "Annual Wage", "Annual Rent"))
#
#
#LOS ANGELES COUNTY
ggplot (data=los_angeles_changes)+
  geom_line(aes(x=year, y =percent_change, group=category, color=category))+
  scale_y_continuous(labels=scales::percent)+
  labs(title="Los Angeles County 2005-2023",
       subtitle="Percent Change in Wages, Rent, & Homless Numbers",
       x="Year",
       y="Percent Change",
       color="% Increase/Decrease in:") +
  scale_color_discrete(name = "% Increase/Decrease in:", label = c("Homeless Count", "Annual Wage", "Annual Rent"))
#
#
#NEW YORK COUNTY
ggplot (data=new_york_changes)+
  geom_line(aes(x=year, y =percent_change, group=category, color=category))+
  scale_y_continuous(labels=scales::percent)+
  labs(title="New York County 2005-2023",
       subtitle="Percent Change in Wages, Rent, & Homless Numbers",
       x="Year",
       y="Percent Change",
       color="% Increase/Decrease in:") +
  scale_color_discrete(name = "% Increase/Decrease in:", label = c("Homeless Count", "Annual Wage", "Annual Rent"))
#
#FOR EACH COUNTY
#USE JUST 2 BEDROOM RENT DATA
#calculate rent % of wages
#pivot the data to allow annual pay and annual rent to be plotted together
#using 2 bedroom rent
#
#DENVER
denver_comparison <- denver_annual_rent %>%
  filter(rent_type == "med_rent_2br") %>%
  mutate(rent_percent_of_wages = round((annual_rent * 100) / annual_average_pay)) %>% 
pivot_longer(cols = -c(county_name, year, unhoused_total, rent_type, monthly_rent, rent_percent_of_wages), names_to = "pay_vs_rent", values_to = "USD")
#
#KING COUNTY
king_county_comparison <- king_county_annual_rent %>%
  filter(rent_type == "med_rent_2br") %>%
  mutate(rent_percent_of_wages = round((annual_rent * 100) / annual_average_pay)) %>% 
  pivot_longer(cols = -c(county_name, year, unhoused_total, rent_type, monthly_rent, rent_percent_of_wages), names_to = "pay_vs_rent", values_to = "USD")
#
#LOS ANGELES
los_angeles_comparison <- los_angeles_annual_rent %>%
  filter(rent_type == "med_rent_2br") %>%
  mutate(rent_percent_of_wages = round((annual_rent * 100) / annual_average_pay)) %>% 
  pivot_longer(cols = -c(county_name, year, unhoused_total, rent_type, monthly_rent, rent_percent_of_wages), names_to = "pay_vs_rent", values_to = "USD")
#
#NEW YORK
new_york_comparison <- new_york_annual_rent %>%
  filter(rent_type == "med_rent_2br") %>%
  mutate(rent_percent_of_wages = round((annual_rent * 100) / annual_average_pay)) %>% 
  pivot_longer(cols = -c(county_name, year, unhoused_total, rent_type, monthly_rent, rent_percent_of_wages), names_to = "pay_vs_rent", values_to = "USD")
#
#
#FOR EACH COUNTY
#BAR PLOT OF WAGES VS. RENT
#
#DENVER
#denver_plot <- 
  ggplot(data=denver_comparison)+
  (mapping = aes(fill=pay_vs_rent,y=USD,x=year))+
  geom_col(position="stack")+
  labs(title="Denver County 2005-2023",
       subtitle="Average Wages vs. Average Rent",
       x="Year",
       y="USD",
       fill="Pay/Rent") +
  scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage","Annual Rent"))
 #PERCENT OF WAGES PLOT   
  ggplot(data=denver_comparison)+
    geom_line(mapping = aes(y=rent_percent_of_wages,x=year))+
    scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                       limits=c(15,30, by=5))+
    labs(title="Denver County 2005-2023",
         subtitle="What % of Wages is Rent",
         x="Year",
         y="Percent of Wages")
#
#KING COUNTY
#king_count_plot <- 
  ggplot(data=king_county_comparison)+
    (mapping = aes(fill=pay_vs_rent,y=USD,x=year))+
    geom_col(position="stack")+
    scale_y_continuous(breaks=c(0,15000,30000,45000,60000,75000,90000, 105000, 120000, 135000, 150000))+
    labs(title="King County 2005-2023",
         subtitle="Average Wages vs. Average Rent",
         x="Year",
         y="USD",
         fill="Pay/Rent") +
    scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage","Annual Rent"))
   #PERCENT OF WAGES PLOT   
  ggplot(data=new_york_comparison)+
    geom_line(mapping = aes(y=rent_percent_of_wages,x=year))+
    scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                       limits=c(15,20, by=5))+
    labs(title="King County 2005-2023",
         subtitle="What % of Wages is Rent",
         x="Year",
         y="Percent of Wages")    
#
#LOS ANGELES COUNTY
#los_angeles_plot <- 
  ggplot(data=los_angeles_comparison)+
    (mapping = aes(fill=pay_vs_rent,y=USD,x=year))+
    geom_col(position="stack")+
    # scale_y_continuous(label_number(scale_cut=cut_short_scale()),
    #                       limits=c(0,300000))+
    labs(title="Los Angeles County 2005-2023",
         subtitle="Average Wages vs. Average Rent",
         x="Year",
         y="USD",
         fill="Pay/Rent") +
    scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage","Annual Rent"))
  #PERCENT OF WAGES PLOT   
  ggplot(data=los_angeles_comparison)+
    geom_line(mapping = aes(y=rent_percent_of_wages,x=year))+
    scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                       limits=c(15,50, by=5))+
    labs(title="Los Angeles County 2005-2023",
         subtitle="What % of Wages is Rent",
         x="Year",
         y="Percent of Wages")     
#
#NEW YORK COUNTY
#new_york_plot <- 
  ggplot(data=new_york_comparison)+
    (mapping = aes(fill=pay_vs_rent,y=USD,x=year))+
    geom_col(position="stack")+
    # scale_y_continuous(label_number(scale_cut=cut_short_scale()),
    #                       limits=c(0,300000))+
    labs(title="New York County 2005-2023",
         subtitle="Average Wages vs. Average Rent",
         x="Year",
         y="USD",
         fill="Pay/Rent") +
    scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Wage","Annual Rent"))
  #PERCENT OF WAGES PLOT   
  ggplot(data=new_york_comparison)+
    geom_line(mapping = aes(y=rent_percent_of_wages,x=year))+
    scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                       limits=c(10,30, by=5))+
    labs(title="New York County 2005-2023",
         subtitle="What % of Wages is Rent",
         x="Year",
         y="Percent of Wages") 
  
  
##########
# NOT USED  
#########  
  
  
  
  
#text = paste("% of wages:", rent_percent_of_wages

#ggplot(denver_avg_rent, aes(fill=pay_vs_rent,y=USD,x=year))+
#  geom_col(position="stack")+
#  labs(title="Denver Average Pay vs. Average Rent",
#       x="Year",
#       fill="Pay/Rent") +
# scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Pay","Annual Rent"))



#plot homeless numbers across pay vs. rent plot
#not recommended by the R community
#didn't get all the pieces to work
ggplot ()+
  geom_col(data=denver_comparison, aes(x=year, y=USD, fill=pay_vs_rent)) +
  geom_line(data=denver, aes(x=year, y=unhoused_total))+
  labs(title="Denver Average Pay vs. Average Rent",
       x="Year",
       fill="Pay/Rent") +
  scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Pay","Annual Rent"))
            
#make interactive 
denver_plot<- ggplot ()+
  geom_col(data=denver_comparison, aes(x=year, y=USD, fill=pay_vs_rent)) +
  geom_line(data=denver, aes(x=year, y=unhoused_total,
          text = paste("Unhoused Count:", unhoused_total)))+
  labs(title="Denver Average Pay vs. Average Rent",
       x="Year",
       fill="Pay/Rent") +
  scale_fill_discrete(name = "Pay vs. Rent", label = c("Annual Pay","Annual Rent"))           

ggplotly(denver_plot,tooltip="text")

scale_y_continuous(sec.axis = sec_axis(~./(2*max(denver_comparison$USD)),name="number of people"))+
#
#
#
#HOMELESS DATA
#
#
denver_homeless <- final_df %>%
  filter(county_name == "Denver County, Colorado", year >2004) %>%
  select(county_name, year, unhoused_total, emergency_shelter, transitional_housing, unsheltered)
#
#Line Plot of Denver Total Homeless Numbers

ggplot(data=denver_homeless)+ 
  geom_line(mapping = aes(y = unhoused_total, x = year),
            color="blue") +
  labs(
    title = "Denver Homeless Population Numbers (2005-2023)",
    x="Year",
    y="Number of People")
    

# pivot data for each type to have its own row
denver_homeless_type_comparison <- denver_homeless %>%
  pivot_longer(cols = -c(county_name, year, unhoused_total), names_to = "type", values_to = "count")

#denver_homeless_plot <- use this for interactive
##
#Bar Plot all types of homeless counts
ggplot(denver_homeless_type_comparison, aes(fill = type, y = count, x = year)) +
  geom_col(position = "dodge") +
  labs(
    title = "Denver Homeless Population Numbers (2005-2023)",
    x="Year",
    y="Count",
#    text = paste("Count:", count),
    fill = "Category") +
 scale_fill_discrete(name = "Category", label = c("Emergency Shelter","Transitional Housing", "Unsheltered"))

#Line Plot all types

ggplot(data = denver_homeless_type_comparison, aes(color = type, y = count, x = year)) +
  geom_line(position = "identity") +
  scale_color_hue(name = "Type", label = c(
    "Emergency Shelter",
    "Transitional Housing", "Unsheltered"
  )) +
  labs(
    title = "Denver Homeless Population Numbers (2005-2023)",
    x = "Year",
    y = "Count",
    color = "Type"
  )
 
#OTHER CITIES 
#SEATTLE HOMELESS PLOT
seattle <- final_df %>% filter(county_name=="King County, Washington", year>2004)
ggplot(data=seattle)+
  geom_line(aes(x=year,
                y=unhoused_total,
                color="red")) +
  scale_x_continuous(breaks=seq(from=2005, to=2023, by=2),)+
 # scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
  #                   limits=c(9000,15000))+
  labs(title="Seattle Homeless Population (2005-2023)",
       y="Number of People",
       x="Year")

#LOS ANGELES HOMELESS PLOT
los_angeles <- final_df %>% filter(county_name=="Los Angeles County, California")
los_angeles_homeless <- los_angeles %>% filter(year>2004)

ggplot(data=los_angeles_homeless)+
  geom_line(aes(x=year,
                y=unhoused_total,
                color="red")) +
  scale_x_continuous(breaks=seq(from=2005, to=2023, by=2),)+
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                     limits=c(15000,90000))+
  labs(title="Los Angeles Homeless Population (2005-2023)",
       y="Number of People",
       x="Year")

ggplot(data=los_angeles_homeless)+
  geom_col(aes(x=year,
                y=unhoused_total,
                ),
           color="red") +
 # scale_x_continuous(breaks=seq(from=2005, to=2023, by=2),)+
#  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
 #                    limits=c(15000,90000))+
  labs(title="Los Angeles Homeless Population (2005-2023)",
       y="Number of People",
       x="Year")

#NEW YORK
new_york <- final_df %>% filter(county_name=="New York County, New York")
new_york_homeless <- new_york %>% filter(year>2004)

ggplot(data=new_york_homeless)+
  geom_line(aes(x=year,
    y=unhoused_total,
    color="red")) +

  scale_x_continuous(breaks=seq(from=2005, to=2023, by=2),)+
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                     limits=c(40000,90000))+
  labs(title="New York Homeless Population (2005-2023)",
       y="Number of People",
       x="Year")


#AIRBNB

#Extract NYC data from Airbnb data
#Separate the Year from the date of Host Since column
#Get total number of hosts in each year
library(lubridate, warn.conflicts=FALSE)
airbnb_df <- read.csv("/Users/patty/Downloads/Airbnb_Data.csv", stringsAsFactors=FALSE)
nyc_airbnb <- airbnb_df %>% filter(city=="NYC", room_type=="Entire home/apt") 
nyc_airbnb <- nyc_airbnb %>% mutate (host_since_year = year(mdy(host_since)))
nyc_airbnb_count_by_year <- nyc_airbnb %>% group_by(host_since_year) %>% 
                       summarize(total_airbnbs=n())


ggplot()+
  geom_line(data=nyc_airbnb_count_by_year, aes(
    x=host_since_year,
    y=total_airbnbs))+

  labs(title="Airbnbs in New York 2008-2017",
       y="Total Airbnbs",
       x="Year Host Started Hosting")

#LOS ANGELESE AIRBNB
la_airbnb <- airbnb_df %>% filter(city=="LA", room_type=="Entire home/apt") 
la_airbnb <- la_airbnb %>% mutate (host_since_year = year(mdy(host_since)))
la_airbnb_count_by_year <- la_airbnb %>% group_by(host_since_year) %>% 
  summarize(total_airbnbs=n())


ggplot()+
  geom_line(data=la_airbnb_count_by_year, aes(
    x=host_since_year,
    y=total_airbnbs))+
  
  labs(title="Airbnbs in Los Angeles 2008-2017",
       y="Total Airbnbs",
       x="Year Host Started Hosting")
 