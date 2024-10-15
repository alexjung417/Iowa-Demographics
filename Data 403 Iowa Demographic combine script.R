library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

base_url <- "https://raw.githubusercontent.com/alexjung417/Iowa-Demographics/refs/heads/main/Iowa%20Demographics%20"
years1 <- 2012
original_names <- NULL

# Function to process a single year's data
process_data <- function(years1) {
  url <- paste0(base_url, year, ".csv")
  demo <- read_csv(url, show_col_types = FALSE)
  
  # Strip non-breaking spaces and trim whitespace from the first column
  first_column_list <- demo %>%
    mutate(across(1, ~str_replace_all(.x, "\u00a0", ""), .names = "cleaned")) %>% # Strip non-breaking spaces
    mutate(across(starts_with("cleaned"), trimws)) %>%  # Strip whitespace
    pull(cleaned) %>%  # Extract the cleaned first column
    as.list()          # Convert to list
  
  return(first_column_list)
}

# Example usage
first_column_values <- process_data(years1)


base_url <- "https://raw.githubusercontent.com/alexjung417/Iowa-Demographics/refs/heads/main/Iowa%20Demographics%20"
years <- 2012:2022
# Function to process a single year's data
process_data <- function(year) {
  url <- paste0(base_url, year, ".csv")
  demo <- read_csv(url, show_col_types = FALSE)

  # Remove columns ending with 'percent'
  demo <- demo %>%
    select(-matches("(?i)percent$"))
  
  # Keep only county names (up to first comma) and transpose
  colnames(demo) <- toupper(sapply(strsplit(names(demo), ","), `[`, 1))
  demo <- t(demo) %>%
    as.data.frame()

  # Remove duplicate columns
  demo <- demo[, !duplicated(names(demo))]
  
  # Clean and process city names
  demo <- demo %>%
    rownames_to_column(var = "city") %>%
    rename_with(~str_replace_all(.x, "\u00a0", "")) %>%
    mutate(across(where(is.character), ~str_trim(.))) %>%
    mutate(year = year)
  
  # Convert relevant columns to numeric
  demo[, 3:ncol(demo)] <- lapply(demo[, 3:ncol(demo)], as.numeric)
  
  # Set 'CITY' and 'year' as index (equivalent to setting key in R)
  demo <- demo %>%
    arrange(city, year) %>%
    relocate(city, year)
  
  return(demo)
}

# Apply the function to each year and combine the data
dataframes <- map(years, process_data)

combined_df <- bind_rows(dataframes) %>%
  drop_na()  # Drop columns with NA values
new_colnames <- c("city", "year", first_column_values)


combined_df <- combined_df %>%
  mutate(city = str_replace_all(city, " city", "") %>% 
           str_replace_all(., " CDP", "") %>% 
           str_trim())
colnames(combined_df) <- new_colnames


