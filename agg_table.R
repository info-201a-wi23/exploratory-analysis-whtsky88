# AGGREGATE TABLE
# use dplyr to produce a table of aggregate information about the data. It must
# perform a group_by() and summarize(), creating a concise yet informative
# summary table.
# ------------------------------------------------------------------

# # load necessary library: dplyr, ggplot2
library(dplyr)
library(ggplot2)
library("scales")
# 
# # load layoff dataframe into variable `layoffs`
layoffs <- read.csv("tech_layoffs.csv", stringsAsFactors = FALSE)

# Subset the `covid` dataframe for our table. What we want to show:
# - select the total_layoffs, company, industry
# - sort in descending order by total_layoffs
# - slice the top 5 rows
agg_table <- layoffs %>% filter(total_layoffs != "Unclear") %>% 
  mutate(total_layoffs = as.numeric(total_layoffs)) %>% arrange(-total_layoffs) %>% 
  select(company, total_layoffs, industry) %>% slice_head(n=5)

agg_table
