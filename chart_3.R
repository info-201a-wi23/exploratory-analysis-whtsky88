# # load necessary libraries
library(dplyr)
library(ggplot2)
library("scales")
library(stringr)
library(tidyverse)
library(RColorBrewer)

# # load layoff dataframe into variable `layoffs`
layoffs <- read.csv("tech_layoffs.csv", stringsAsFactors = FALSE)

# # Subset the layoffs dataframe to:
# - convert impacted_workforce_percentage to numeric data
# - convert reported_date to date data instead of character
# - select impacted_workforce_percentage, reported_date, and status
# - sort the dates in ascending order
layoff_impact <- layoffs %>% filter(impacted_workforce_percentage != "Unclear") %>% 
  mutate(impacted_workforce_percentage = as.numeric(impacted_workforce_percentage)) %>%
  mutate(reported_date = as.Date(reported_date, format = "%m/%d/%Y")) %>%
  select(impacted_workforce_percentage, reported_date, status) %>%
  arrange(reported_date)

# # plot a scatterplot that marks points by the company's IPO
ggplot(data=layoff_impact, aes(x=reported_date, y=impacted_workforce_percentage, color = status)) +
  geom_point() +
  labs(title = "Impacted Workforce Percentage by Reported Date", x = "Reported Date", y = "Impacted Workforce Percentage", color = "IPO")
