##################
# LOAD LIBRARIES #
#################

library(tidyverse)
library(rio)
library(lubridate)
library(ggstatsplot)
library(patchwork)

################
# IMPORT DATA #
###############

# import data (specify your own path) and store in a tibble format
kwh <- tibble(import("~/R_projects/EnergyConsumption/data/kwh.xlsx"))
# show tibble
kwh 
View(kwh)

########################
# START DATA WRANGLING #
#######################

# pivot longer
kwh <- kwh |>
  pivot_longer(cols = everything(), # transform all column names that have date values as a name
               names_to = 'dates', # into 1 column named 'dates' 
               values_to ='kwh') |> # and create a column named 'kwh' with all corresponding kwh values per date
               arrange(dates) # order the new 'dates' column by date

# transform serial date numbers into dates
# first transform from character into numeric class
kwh$dates <- as.numeric(kwh$dates) 
# compute the numbers into dates; 
# start date differs across operating systems!
# For Excel on Windows, the origin is December 30, 1899 for dates after 1900.
# For Excel on Mac, the origin is January 1, 1904.
# For Matlab the origin is 1970-01-01 
# Use US notation yyyy-mm-dd
kwh$dates <- as.Date(kwh$dates, origin = '1899-12-30') 
# create a month column as factor
kwh$month <- as.factor(month(kwh$dates)) 
# add hour column 'h' with values from 1 - 24
# add column for night, evening and day time hours, and quartals as factor
kwh <- kwh |>
  mutate(h = rep(1:24,365))  |>
  mutate(timezone = as.factor(case_when(h >= 1 & h <= 5  ~ 'Night',
                              h >= 22 & h <= 24 ~ 'Night',
                              h >= 18 & h <= 21 ~ 'Evening', 
                              h >= 6 & h <= 17 ~ 'Day'))) |>
  mutate(quarter = as.factor(quarter(dates)))

# create tibble with totals grouped by month and timezone
kwhmonth <- kwh |>
  group_by(month, timezone)|>
  summarise(sum = sum(kwh, na.rm = TRUE))

# create tibble with mean kwh, and median kwh grouped by quarter by timezone
kwhQuart <- kwh |>
  group_by(quarter, timezone)|>
  summarise(meanKWh = mean(kwh, na.rm = TRUE),
            medianKwh = median(kwh, na.rm = TRUE))

# check data
kwh
kwhmonth
kwhQuart

######################
# END DATA WRANGLING #
#####################

#############
# PLOTTING #
############

# plot hourly consumption plot by timezone

ph <- kwh |>
  ggplot(aes(x = dates, y = kwh)) +
  geom_line(aes(group = timezone, color = timezone)) +
  # geom_point(alpha = 0.5) +
  scale_color_viridis_d(option = "D") +
  ggtitle("Hourly Energy Consumption") +
  labs(x = "", y = "Energy Consumption (KWh)") +
  geom_smooth() +
  facet_wrap(~timezone) +
  theme_classic() +
  theme(legend.position = "")

# plot monthly consumption by timezone
pm <- kwhmonth |>
  ggplot(aes(x = month, y = sum)) +
  geom_line(aes(group = timezone, color = timezone)) +
  geom_point(aes(color = timezone),alpha = 0.5) +
  scale_color_viridis_d(option = "D") +
  stat_summary(fun = sum, na.rm = TRUE, group = 1, geom ='line') +
  ggtitle("Household Electricity Load Profile") +
  labs(x = "Months", y = "Energy Consumption (KWh)") +
  theme_classic() +
  theme(legend.position = "top")

# anova to examine significance mean differences between time zones in hourly consumption
pth <- kwh |>
  ggbetweenstats(x = timezone,
                 y = kwh)

# anova to examine significance mean differences between time zones in monthly consumption
ptm <- kwhmonth |>
  ggbetweenstats(x = timezone,
                 y = sum)

# print plots
pm # month
ptm # anova month
ph # hour
pth # anova hour
ph / pth # combine plots horizontal
pm / ptm  # combine plots horizontal

# Hourly Energy Consumption By Quarter Grouped by Timezone
kwh |>
  ggplot(aes(x = quarter, y = kwh, color = quarter, fill = timezone)) +
  geom_boxplot() +
  stat_summary(fun ="mean") +
  scale_color_viridis_d(option = "D") +
  ggtitle("Hourly Energy Consumption By Quarter Grouped By Timezone") +
  labs(x = "", y = "Energy Consumption (KWh)") +
  facet_wrap(~timezone) +
  theme_classic() +
  theme(legend.position = "bottom")

 



