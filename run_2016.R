# Graph race results from 2016
# 2016 New Year Resolution: One road race a month

# Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

# Set working directory
setwd("C:/Users/fishe/Documents/running_2016")

# Read in file
r <- read_csv("results.csv")

# Tidy up the data into long form
# place_overall, place_gender, total_overall, total_gender should be one column
# values of those columns should be another column

r2 <- select(r, -c(`Results 1`, `Results 2`, location)) %>% gather(place_type, place, place_overall:place_gender) %>%
  gather(total_type, total_runners, total_overall:total_gender)

# place_type and total_type need to match
gen <- filter(r2, place_type == "place_gender", total_type == "total_gender")
ov <- filter(r2, place_type == "place_overall", total_type == "total_overall" )

r3 <- rbind(gen, ov)

# Add percentiles to results
# Formula: R = P/100 * (N+1)
# N = total_runners, 
# Formula: P = 100R/(N+1)

r4 <- r3 %>% mutate(percentile = (100-(as.integer(place)/total_runners) * 100))

# Plot the results
r4 %>% 
  filter(place_type == "place_overall", total_type == "total_overall") %>% 
  ggplot(aes(race_number, percentile, color = racer)) + 
    geom_point(size = 6) +
    scale_color_manual(values = c("Dark Grey", "Blue")) +
    scale_x_continuous(breaks = 1:14) +
    scale_y_continuous(breaks = c(25, 50, 75, 100)) +
    labs(title = "2016 Race Results by Percentile",
         x = "Race Number",
         y = "Overall Place Percentile") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_blank())
