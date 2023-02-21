# # load necessary libraries
library(dplyr)
library(ggplot2)
library("scales")
library(stringr)
library(tidyverse)
library(RColorBrewer)

# # load layoff dataframe into variable `layoffs`
layoffs <- read.csv("tech_layoffs.csv", stringsAsFactors = FALSE)

# # mutating a new column that only takes the main industry of each company
layoffs <- layoffs %>%
  mutate(simplify_industry = gsub(",.*$", "", industry))

# # create a new dataframe that is just the broad industry and the total layoffs in each
industry_layoffs <- layoffs %>%
  group_by(simplify_industry) %>%
  summarize(mean_percent_layoffs = mean(suppressWarnings(as.numeric(impacted_workforce_percentage)), rm.na = TRUE))

# # remove all the NA values in dataframe
industry_layoffs <- industry_layoffs[complete.cases(industry_layoffs), ] 

# # limit dataframe to only the ten biggest impacted industries for graph clarity
top_ten <- industry_layoffs %>%
  filter(row_number() <= 10)

# # plot a bar graph showing the ten most-impacted tech industries
ggplot(data=top_ten, aes(x=simplify_industry, y=mean_percent_layoffs, fill = simplify_industry)) +
  geom_bar(stat="identity", width=0.5) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "10 Most Impacted Industries by Mean Layoff Percentage", x = "Industry", y = "Average Percent Laid Off", fill = "Industry") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
