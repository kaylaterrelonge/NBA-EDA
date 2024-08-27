# Final Project Data Cleaning ----

## Load Packages ----

library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)
library(readr)

### data importing ----

# gathering all .csv files from folder
nba_files <- dir("data/raw_data_players/", pattern = "\\.csv$", full.names = TRUE)
nba_files

# reading in files via for loop
nba_files_imported <-list()

for(i in seq_along(nba_files)){
  nba_files_imported[[i]] <- read_csv(nba_files[[i]])
}

# combining all list into one data frame
nba_data <- bind_rows(nba_files_imported)

### data cleaning ----

tidy_nba_data <- 
  nba_data %>%
  rowwise() %>% 
  # separating team codes into own col
  mutate(team = str_extract_all(name, "[A-Z]{2,4}$") %>%
           unlist() %>%
           paste0(collapse = ""), 
         # removing the team codes from names
         name = str_split(name,"[A-Z]{2,4}$") %>% 
           unlist() %>%
           paste0(collapse = ""), 
         # separating the team codes for players who played for more than one team
         sec_team = str_extract_all(name, "[A-Z]{2,4}/$") %>% 
           unlist() %>%
           paste0(collapse = ""),
         # removing those team codes from the names
         name = str_split(name,"[A-Z]{2,4}/$") %>% 
           unlist() %>%
           paste0(collapse = "")) %>% 
  # removing `/`
  mutate(sec_team = str_split(sec_team, "/") %>% 
           unlist() %>%
           paste0(collapse = "")) %>% 
  # tidying data to get teams into one col
  pivot_longer(cols = ends_with("team"), names_to = "all_teams") %>% 
  # filtering out rows with no team entry, meaning player only played for one team
  filter(value != "") %>% 
  # changing col names 
  mutate(team = value,
         team_class = all_teams) %>% 
  select(-c(value, all_teams))

### data export ----
write_csv(tidy_nba_data, "data/processed_data/nba_data_processed.csv")

## importing coaching data ----

# gathering all files
coaching_files <- dir("data/raw_data_coaches/", pattern = "\\.csv$", full.names = TRUE)
coaching_files

# reading in files via for loop
coaching_files_imported <-list()

for(i in seq_along(coaching_files)){
  coaching_files_imported[[i]] <- read_csv(coaching_files[[i]])
}

# combining all list into one data frame
nba_coaching_data <- bind_rows(coaching_files_imported)

### data cleaning ----

tidy_coaching_data <-
  # selecting out important rows
  nba_coaching_data %>% 
  select(...1, ...2, Seasons, ...27) %>% 
  # renaming cols
  mutate(coach = ...1,
         team = ...2,
         year_w_team = Seasons,
         season = ...27) %>% 
  select(coach, team, year_w_team, season) %>% 
  # getting rid of empty cells/unimportant headers
  filter(coach != 'NA') %>% 
  filter(coach != 'Coach') %>% 
  filter(team != 'Tm') %>% 
  filter(year_w_team != '#') %>% 
  filter(season != 'season') 
 
### data export ----
write_csv(tidy_coaching_data, "data/processed_data/coaching_data_processed.csv")
