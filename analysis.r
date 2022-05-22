library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)

incarcerations <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Introduction
#
#
# 1) highest prison admissions county for each state in 2015
prison_admin_df <- incarcerations %>% 
  filter(year == 2015) %>% 
  group_by(state) %>%
  filter(!is.na(total_prison_pop)) %>%
  filter(total_prison_pop == max(total_prison_pop))
highest_prison_admin <- prison_admin_df[["county_name"]]

# 2) total jailed population for each state
state_df <- incarcerations %>% 
  group_by(state) %>% 
  summarize(state_total = sum(total_jail_pop, na.rm = TRUE))
state_jail_pop <- state_df[["state_total"]]

# 3) proportion of juvenile jail for several years
#
# add column with proportion of juvenile jail
incarcerations <- mutate(incarcerations, total_juv_jail_pop = female_juvenile_jail_pop + male_juvenile_jail_pop)
incarcerations <- mutate(incarcerations, juv_jail_prop = total_juv_jail_pop / total_jail_pop)
# proportion for 1990
juv1990_df <- incarcerations %>%
  filter(!is.na(juv_jail_prop)) %>%
  filter(year == 1990)
juv1990 <- mean(juv1990_df[["juv_jail_prop"]])
# proportion for 2000
juv2000_df <- incarcerations %>%
  filter(!is.na(juv_jail_prop)) %>%
  filter(year == 2000)
juv2000 <- mean(juv2000_df[["juv_jail_prop"]])
# proportion for 2015
juv2015_df <- incarcerations %>%
  filter(!is.na(juv_jail_prop)) %>%
  filter(year == 2015)
juv2015 <- mean(juv2015_df[["juv_jail_prop"]])

# 4) total prison population for each racial group
black_pop <- sum(select(incarcerations, black_prison_pop), na.rm = TRUE)
white_pop <- sum(select(incarcerations, white_prison_pop), na.rm = TRUE)
latinx_pop <- sum(select(incarcerations, latinx_prison_pop), na.rm = TRUE)
native_pop <- sum(select(incarcerations, native_prison_pop), na.rm = TRUE)
aapi_pop <- sum(select(incarcerations, aapi_prison_pop), na.rm = TRUE)

# 5) proportion of each race in prison
#
# add column with proportion of each race
incarcerations <- mutate(incarcerations, white_proportion = white_prison_pop / total_prison_pop)
incarcerations <- mutate(incarcerations, black_proportion = black_prison_pop / total_prison_pop)
incarcerations <- mutate(incarcerations, latinx_proportion = latinx_prison_pop / total_prison_pop)
incarcerations <- mutate(incarcerations, native_proportion = native_prison_pop / total_prison_pop)
incarcerations <- mutate(incarcerations, aapi_proportion = aapi_prison_pop / total_prison_pop)
# proportion of each race in 1980
race_proportion1985_df <- incarcerations %>%
  filter(year == 1985)
white_prop_1985 <- mean(race_proportion1985_df[["white_proportion"]], na.rm = TRUE)
black_prop_1985 <- mean(race_proportion1985_df[["black_proportion"]], na.rm = TRUE)
latinx_prop_1985 <- mean(race_proportion1985_df[["latinx_proportion"]], na.rm = TRUE)
native_prop_1985 <- mean(race_proportion1985_df[["native_proportion"]], na.rm = TRUE)
aapi_prop_1985 <- mean(race_proportion1985_df[["aapi_proportion"]], na.rm = TRUE)
# proportion of each race in 2000
race_proportion2000_df <- incarcerations %>%
  filter(year == 2000)
white_prop_2000 <- mean(race_proportion2000_df[["white_proportion"]], na.rm = TRUE)
black_prop_2000 <- mean(race_proportion2000_df[["black_proportion"]], na.rm = TRUE)
latinx_prop_2000 <- mean(race_proportion2000_df[["latinx_proportion"]], na.rm = TRUE)
native_prop_2000 <- mean(race_proportion2000_df[["native_proportion"]], na.rm = TRUE)
aapi_prop_2000 <- mean(race_proportion2000_df[["aapi_proportion"]], na.rm = TRUE)
# proportion of each race in 2015
race_proportion2015_df <- incarcerations %>%
  filter(year == 2015)
white_prop_2015 <- mean(race_proportion2015_df[["white_proportion"]], na.rm = TRUE)
black_prop_2015 <- mean(race_proportion2015_df[["black_proportion"]], na.rm = TRUE)
latinx_prop_2015 <- mean(race_proportion2015_df[["latinx_proportion"]], na.rm = TRUE)
native_prop_2015 <- mean(race_proportion2015_df[["native_proportion"]], na.rm = TRUE)
aapi_prop_2015 <- mean(race_proportion2015_df[["aapi_proportion"]], na.rm = TRUE)


# Trends over time chart
race_prison_df <- incarcerations %>% 
  group_by(year) %>%
  filter(year > 1977 & year < 2016) %>%
  summarize(white_prison_prop = mean(white_proportion, na.rm = TRUE), 
            black_prison_prop = mean(black_proportion, na.rm = TRUE),
            latinx_prison_prop = mean(latinx_proportion, na.rm = TRUE),
            native_prison_prop = mean(native_proportion, na.rm = TRUE),
            aapi_prison_prop = mean(aapi_proportion, na.rm = TRUE)) %>%
  select(year, white_prison_prop, black_prison_prop, latinx_prison_prop, 
         native_prison_prop, aapi_prison_prop)
race_prison_df <- race_prison_df[!is.infinite(rowSums(race_prison_df)), ]

line_colors <- c("White" = "gray40", "Black" = "steelblue", "Latinx" = "palegreen",
                  "Native" = "skyblue", "AAPI" = "lightgoldenrod")
ggplot(data = race_prison_df, aes(x = year)) +
  geom_line(aes(y = white_prison_prop, color = "White")) +
  geom_line(aes(y = black_prison_prop, color = "Black")) +
  geom_line(aes(y = latinx_prison_prop, color = "Latinx")) +
  geom_line(aes(y = native_prison_prop, color = "Native")) +
  geom_line(aes(y = aapi_prison_prop, color = "AAPI")) +
  ggtitle("Proportion of Race in Prison vs Year") +
  labs(x = "Year", y = "Proportion of Prison", color = "Race") +
  scale_color_manual(values = line_colors)

# Variable Comparison Chart
juv_jail_df <- incarcerations %>% 
  filter(total_pop > 500000) %>%
  group_by(year) %>%
  filter(year > 1977 & year != 1993) %>%
  summarize(male_juv_jail_pop = mean(male_juvenile_jail_pop, na.rm = TRUE),
            total_jail_popu = mean(total_jail_pop, na.rm = TRUE)) %>%
  select(year, total_jail_popu, male_juv_jail_pop)
juv_jail_df <- juv_jail_df[!is.infinite(rowSums(juv_jail_df)), ]

ggplot(data = juv_jail_df, aes(x = total_jail_popu)) +
  geom_point(aes(y = male_juv_jail_pop)) +
  ggtitle("Population of Males in Juvenile Jail vs Total Jail Population") +
  labs(x = "Total Jail Population", y = "Population of Males in Juvenile Jail")


# Map
prison_df <- incarcerations %>%
  filter(!is.na(total_prison_pop)) %>%
  filter(year == 2000) %>%
  select(fips, total_prison_pop)

US <- map_data("world") %>% filter(region == "US")

dfips <- maps::county.fips %>%
  as.tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$")
long_lat_values <- map_data("county") %>% 
  left_join(dfips)

long_lat_values <- long_lat_values %>% distinct(fips, .keep_all = TRUE)

prison_map_df <- merge(prison_df, long_lat_values, by = "fips")

ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.3) +
  geom_point(data=prison_map_df, aes(x=long, y=lat, size=total_prison_pop),
             color = "steelblue", alpha = 0.8) +
  scale_size_continuous(range=c(0.1,3), name = "Prison Population") +
  ggtitle("Prison Population of Each County") +
  labs(x = "Longitude", y = "Latitude")



