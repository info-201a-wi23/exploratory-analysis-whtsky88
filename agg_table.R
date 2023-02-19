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
# # load states dataframe into variable `covid` and filter for most recent data
layoffs <- read.csv("tech_layoffs.csv", stringsAsFactors = FALSE)

# Subset the `covid` dataframe for our table. What we want to show:
# - select the state, cases, and deaths columns
# - sort in descending order by cases
# - slice the top 5 rows
agg_table <- layoffs %>% filter(total_layoffs != "Unclear") %>% 
  mutate(total_layoffs = as.numeric(total_layoffs)) %>% arrange(-total_layoffs) %>% 
  select(company, total_layoffs, industry) %>% slice_head(n=5)

agg_table
