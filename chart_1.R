# # load necessary libraries
library(dplyr)
library(ggplot2)
library("scales")
library(stringr)
library(tidyverse)
library(RColorBrewer)
 
# # load layoff dataframe into variable `layoffs`
layoffs <- read.csv("~/Documents/Info201Code/exploratory-analysis-whtsky88/tech_layoffs.csv", stringsAsFactors = FALSE)
View(layoffs)

# # establish new dataframe with the sources and the amount of times they appear
source_count <- table(Sources = layoffs$sources)
source_count <- as.data.frame(source_count)

# # record total count of every instance of a CEO
CEO_count <- source_count %>%
  filter(grepl("CEO", Sources)) %>%
  nrow()

# # summarize every source with less than 5 appearances
other_count <- source_count %>%
  filter(Freq < 5) %>%
  summarise(sum(Freq, na.rm = TRUE))
View(other_count)

# # use newly calculated data to simplify source list
source_count <- source_count %>%
  filter(Freq >= 5) %>%
  add_row(Sources = "Company CEO", Freq = CEO_count) %>%
  add_row(Sources = "Other", Freq = as.numeric(other_count))

# # plot a stacked bar chart, and then modify for pie chart
ggplot(as.data.frame(source_count), aes(x="", y=Freq, fill=Sources)) +
  geom_bar(stat="identity", width=1, color="aliceblue") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels