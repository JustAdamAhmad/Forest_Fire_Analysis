---
  title: "Exploratory Visualization of Forest Fire Data"
author: "Adam Ahmad"
output: html_document
---
  
  # Exploring Data Through Visualizations: Independent Investigations
  
  Loading the packages and data needed for this project

install.packages("tidyverse")
library(readr)
library(tidyverse)
library(dplyr)
getwd()
forest_fires <- forestfires %>% mutate(month = factor(month, levels = c('jan','feb','mar','apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')))
forest_fires %>% mutate(day = factor(day, levels = c('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun')))
forest_fires <- forestfires %>% mutate(month = factor(month, levels = month_order),day = factor(day, levels = dow_order)

# When Do Most Forest Fires Occur?                                     
monthly_fires <- forest_fires %>% group_by(month) %>% summarise(total_fire= n())
monthly_fires %>% ggplot(aes(x=month, y= total_fire)) + geom_col() + labs(
title = "Number of forest fires in data by month",y ="Fire count",x = "Month")

# what day do most forest fires occur?
daily_fires <- forest_fires %>% group_by(day) %>% summarise(total_fires = n())
daily_fires %>% ggplot(aes(x=day, y= total_fires)) + geom_col() + labs(
  title = "Number of forest fires in data by day",y ="Fire count",x = "day")


# From our graphs, we saw that August and September see more forest fires than other months. 
# It also looks as though the weekend days (Friday, Saturday, and Sunday) have more forest fires than days in the middle of the week.


# To explore the temporal patterns of forest fire occurrence the bar charts reveal
# we should look more closely at how the variables that relate to forest fires vary by month and by day of the week. 

#We should see how each of the other variables in the dataset relates to month

monthly_fires_NEW <- forest_fires %>% group_by(month) %>% summarise(avg_dc = mean(DC),
                                                                avg_dmc = mean(DMC),
                                                                avg_temp = mean(temp),
                                                                avg_FFMC = mean (FFMC),
                                                                avg_ISI = mean(ISI),
                                                                avg_RH = mean (RH),
                                                                avg_wind = mean(wind),
                                                                avg_rain = mean(rain),
                                                                avg_area = mean(area))


#We see a massive spike in fires in August and September, as well as a smaller spike in March. Fires seem to be more frequent on the weekend.

# Plotting Other Variables Against Time (month) - To Investigate the variation around month
forest_fires_long <- forest_fires %>% pivot_longer(cols = c('FFMC', 'DC', 'DMC', 'ISI', 'RH', 'temp', 'wind', 'rain'), names_to = 'Data_col', values_to = 'Value')
forest_fires_long %>% ggplot(aes(x=month, y= Value)) + geom_line() + facet_wrap(vars(Data_col), scales = "free_y")

#Exploring forest fire severity
#We are trying to see how each of the variables in the dataset relate to `area`. We can leverage the long format version of the data we created to use with `facet_wrap()`.

forest_fires_long %>% ggplot(aes(x= Value, y = area )) + geom_point() + facet_wrap(vars(Data_col), scales = 'free_x') + labs( 
  title = "Relationships between other variables and area burned",x = "Value of column",y = "Area burned (hectare)")


# Outlier Problems
# It seems that there are two rows where `area` that still hurt the scale of the visualization. Let's make a similar visualization that excludes these observations so that we can better see how each variable relates to `area`.

forest_fires_long_outlier <- forest_fires_long %>% filter(area < 300) %>% ggplot(aes(x= Value, y= area)) + geom_point() + facet_wrap(vars(Data_col), scales = 'free_x') +
  labs( title = "Relationships between other variables and area burned",x = "Value of column",y = "Area burned (hectare)")

