library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

base_url <- "https://raw.githubusercontent.com/alexjung417/Iowa-Demographics/refs/heads/main/Iowa%20Demographics%20"
years <- 2012:2022

process_data <- function(year) {
  url <- paste0(base_url, year, ".csv")
  demo <- read_csv(url, show_col_types = FALSE)
  

  demo <- demo %>%
    select(-matches("(?i)percent$"))
  
  print(paste("Dimensions after removing percent columns:", dim(demo)))
  
  # Keep only county names (up to first comma) and transpose
  original_names <- names(demo)
  colnames(demo) <- toupper(sapply(strsplit(names(demo), ","), `[`, 1))
  

  demo <- t(demo) %>%
    as.data.frame()
  
  # Print dimensions after transposition
  #print(paste("Dimensions after transposition:", dim(demo)))
  
  # Remove duplicate columns
  demo <- demo[, !duplicated(names(demo))]
  
  # Print dimensions after removing duplicate columns
  #print(paste("Dimensions after removing duplicate columns:", dim(demo)))
  
  # Clean and process city names
  demo <- demo %>%
    rownames_to_column(var = "city") %>%
    rename_with(~str_replace_all(.x, "\u00a0", "")) %>%
    mutate(across(where(is.character), ~str_trim(.))) %>%
    mutate(year = year)
  
  # Print dimensions after cleaning city names
  #print(paste("Dimensions after cleaning city names:", dim(demo)))
  
  # Check for any non-numeric values in columns that should be numeric
  non_numeric <- demo %>%
    select(-city, -year) %>%
    mutate(across(everything(), ~!is.na(as.numeric(.))))
  
  
  # Convert relevant columns to numeric
  demo[, 3:ncol(demo)] <- lapply(demo[, 3:ncol(demo)], as.numeric)
  
  # Set 'CITY' and 'year' as index (equivalent to setting key in R)
  demo <- demo %>%
    arrange(city, year) %>%
    relocate(city, year)
  
  return(demo)
}

original_names <- NULL
years1 = 2012
# Function to process a single year's data
process_data1 <- function(year) {
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


first_column_values <- process_data1(years1)
new_colnames <- c("city", "year", first_column_values)
dataframes <- map(years, process_data)
combined_df <- bind_rows(dataframes)
na <- combined_df[apply(is.na(combined_df),1,any),]


combined_df <- combined_df %>%
  mutate(city = str_replace_all(city, " city", "") %>% 
           str_replace_all(., " CDP", "") %>% 
           str_trim())
colnames(combined_df) <- new_colnames

write_csv(combined_df, "Iowa Demographics.csv")

