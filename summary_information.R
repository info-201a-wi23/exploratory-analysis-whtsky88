library(dplyr)

layoffs <- read.csv("tech_layoffs.csv", stringsAsFactors = FALSE)

# A function that takes in a dataset and returns a list of info about it:
summary_information <- function(layoffs) {
  # The list to return
  summary_info <- list()

  # Number of companies reported
  summary_info$unique_companies <- length(unique(layoffs$company))

  # Total number of layoffs in the dataset
  layoffs$total_layoffs <- suppressWarnings(as.numeric(layoffs$total_layoffs))
  summary_info$total_layoff <- sum(layoffs$total_layoffs, na.rm = TRUE)

  # Company layoffs the most and its number
  max_layoffs <- layoffs %>%
    group_by(company) %>%
    summarize(total_layoff_num = sum(total_layoffs)) %>%
    filter(! is.na(total_layoff_num)) %>%
    filter(total_layoff_num == max(total_layoff_num))
  summary_info$company_name <- max_layoffs$company
  summary_info$max_company_layoffs <- max_layoffs$total_layoff_num

  # Most location of the headquarter of the layoff company
  summary_info$max_headquarter_location <- layoffs %>%
    group_by(headquarter_location) %>%
    summarize(headquarter_num = n()) %>%
    filter(headquarter_num == max(headquarter_num)) %>%
    pull(headquarter_location)

  # Percentage of the company status
  company_status <- layoffs %>%
    group_by(status) %>%
    summarize(comp_status = n())
  summary_info$private_percent <- round(
    company_status$comp_status[1] / sum(company_status$comp_status) * 100,
    digits = 2)

  # Industry impact the most by layoffs
  # clean the industry column
  ind <- list()
  for (i in as.list(layoffs$industry)) {
    industries <- unlist(strsplit(i, ", "))
    ind <- append(ind, industries[length(industries)])
  }
  ind <- lapply(ind, tolower)
  ind <- lapply(X = ind, FUN = function(t) gsub("-", "", t))

  max_industry <- layoffs %>%
    mutate(industry_cleaned = ind) %>%
    group_by(industry_cleaned) %>%
    summarize(industry_count = n()) %>%
    filter(industry_count == max(industry_count)) %>%
    pull(industry_cleaned)
  summary_info$max_industry <- unlist(max_industry)

  # return the summary_info
  return(summary_info)
}

# In summary of all the total technology company layoffs, we found that there
# are `477` number of companies reported in the total dataset. Adding them all
# together, we found that there are a total of `13156` people that are laid off.
# Given such information, we dug deeper into the dataset and found out that
# `Amazon` company has been affected the most and had the greatest number of
# layoffs with a total of `18000` people. Most company’s headquarter are located
# at `San Francisco`. To sum up all the given values, the top layoff industry is
# `fintech`. From all the companies listed in this dataset, `75.7`% of them are
# identified as private, indicating that most laid off companies are private
# companies, who took the larger burden.
