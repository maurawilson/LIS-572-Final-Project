#FOR EACH COUNTY
#LINE PLOT WITH % CHANGE FOR ALL VARIABLES
#plot % changes for pay, rent and homeless 
#
#DENVER
ggplot (data=denver_changes)+
  geom_line(aes(x=year, y =percent_change, group=category, color=category))+
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                     limits=c(-50,50))+
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
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                     limits=c(-50,30))+
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
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()),
                     limits=c(-45,30))+
  labs(title="New York County 2005-2023",
       subtitle="Percent Change in Wages, Rent, & Homless Numbers",
       x="Year",
       y="Percent Change",
       color="% Increase/Decrease in:") +
  scale_color_discrete(name = "% Increase/Decrease in:", label = c("Homeless Count", "Annual Wage", "Annual Rent"))