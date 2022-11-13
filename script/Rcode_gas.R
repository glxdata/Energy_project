##################
# LOAD LIBRARIES #
#################

library(tidyverse)
library(rio)
library(lubridate)
library(patchwork)
library(viridis)

################
# IMPORT DATA #
###############

# import data (specify your own path) and store in a tibble format
gas <- tibble(import("~/R_projects/EnergyConsumption/data/gas.xlsx"))

# show tibble
gas
View(gas)

########################
# START DATA WRANGLING #
#######################

# add columns for kwh, month, year 
gas <- gas |>
  mutate(kwh = (m3*10.55)) |>
  mutate(month = month(date)) |>
  mutate(year = as.factor(year(date)))
  
# create tibble with totals grouped by month
gasmonth <- gas|>
    group_by(year) |>
    group_by(month)|>
    summarise(sum = sum(m3, na.rm = TRUE)) 

# check data
gas 
gasmonth

######################
# END DATA WRANGLING #
#####################

#############
# PLOTTING #
############

# plot daily gas consumption
p1 <- gas |>
  ggplot(aes(x = date, y = m3)) +
  geom_line(color = 'firebrick3') +
  geom_point(color = 'steelblue4', alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 10.55, name = "Gas Consumption (kwh)")) +
  ggtitle("Daily Gas Consumption") +
  labs(x = "", y = "Gas Consumption (m3)") +
  geom_smooth(color = "steelblue4") +
  theme_classic()

# plot monthly gas consumption
p2 <- gasmonth |>
  ggplot(aes(x = month, y = sum)) +
  geom_line(color = 'firebrick3') +
  geom_point(color = 'steelblue4', alpha = 0.5) +
  scale_x_continuous(n.breaks = 12)+
  scale_y_continuous(sec.axis = sec_axis(~ . * 10.55, name = "Gas Consumption (kwh)")) +
  ggtitle("Monthly Gas Consumption") +
  labs(x = "", y = "Gas Consumption (m3)") +
  geom_smooth(color = "steelblue4") +
  theme_classic()

# print plots
p1/p2 # combined horizontal
p1+p2 # combined vertical
p1 # single
p2 # single

# plot colorblind friendly and, factor by year
gas |>
  ggplot(aes(x = date, y = m3, color = year)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d(option = "D") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 10.55, name = "Gas Consumption (kwh)")) +
  ggtitle("Daily Gas Consumption") +
  labs(x = "", y = "Gas Consumption (m3)") +
  geom_smooth(fill = viridis(1)) +
  theme_classic()

gasmonth |>
  ggplot(aes(x = month, y = sum)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  geom_smooth(color = viridis(1), fill = viridis(1)) +
  scale_color_viridis_c(option = "D") + 
  scale_x_continuous(n.breaks = 12)+
  scale_y_continuous(sec.axis = sec_axis(~ . * 10.55, name = "Gas Consumption (kwh)")) +
  ggtitle("Monthly Gas Consumption") +
  labs(x = "", y = "Gas Consumption (m3)") +
  theme_classic()


  

