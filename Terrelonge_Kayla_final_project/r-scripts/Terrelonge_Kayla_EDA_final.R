# Long Form EDA ----

## Load Packages ----

library(tidyverse)
library(dplyr)
library(lvplot)
library(stringr)
library(forcats)
library(skimr)
library(lattice)
library(gridExtra)

### Data Import ----
nba_player_data <- read_csv("data/processed_data/nba_data_processed.csv")
nba_coaching_data <-  read_csv("data/processed_data/coaching_data_processed.csv")

### viewing and skimming data ----
skim_without_charts(nba_player_data)
skim_without_charts(nba_coaching_data)

### data manipulation  ----
pandemic_classification_data <- nba_player_data %>% 
  mutate(pandemic_status = fct_collapse(season, 
                                         `pre-pandemic` = c("2001-2002","2002-2003", "2003-2004",
                                                          "2004-2005","2005-2006","2006-2007",
                                                          "2007-2008","2008-2009","2009-2010",
                                                          "2010-2011","2011-2012", 
                                                          "2012-2013","2013-2014",
                                                          "2014-2015","2015-2016",
                                                          "2016-2017", "2017-2018","2018-2019"),
                                        `pandemic` = c("2019-2020", "2020-2021"),
                                        `post-pandemic` = c("2021-2022")
                                         )) 
  
## Pandemic Data ----

# subsetting for appropriate data 
pre_pandemic_nba_data <-
  pandemic_classification_data %>% 
  filter(pandemic_status == "pre-pandemic")

# looking at external factors that could affect performance
pre_pandemic_nba_data %>% 
  select(GP, MIN) %>% 
  summarise(avg_mins_played = mean(MIN, na.rm = TRUE),
            avg_games_played = mean(GP, na.rm = TRUE))

# breaking data up by year to observe any major changes over time
pre_pandemic_nba_data %>% 
  group_by(season) %>% 
  summarise(mean_field_goal = mean(`FG%`, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = season, y = mean_field_goal, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Average Field Goal Percentages per Season Pre-Pandemic",
       x = "Season",
       y = "Average Field Goal Percentage")
# evaluating the top 100 players 
pre_pandemic_nba_data %>% 
  filter(rank <= 100) %>% 
  group_by(rank) %>% 
  mutate(avg_field_goal = mean(`FG%`, na.rm = TRUE)) %>% 
  ggplot(aes(x = rank , y = avg_field_goal)) +
  geom_point() +
  geom_smooth(se = FALSE) 

# fixing labeling error in data 
corrected_pandemic_data <- pandemic_classification_data%>% 
  mutate(team = fct_collapse(team, 
                                        `ATL` = c("IATL","ATL"),
                                        `BKN` = c("IBKN", "BKN"),
                                        `BOS` = c("IBOS","BOS"),
                                        `CHA` = c("ICHA", "CHA"),
                                        `CHI` = c("ICHI", "CHI"),
                             `CLE` = c("ICLE", "CLE"),
                             `DEN` = c("IDEN", "DEN"),
                             `DET` = c("IDET", "DET"),
                             `IND` = c("IIND", "IND"),
                             `NO` = c("IINO", "NO"),
                             `NY` = c("IINY", "NY"),
                             `LAL` = c("ILAL", "LAL"),
                             `MEM` = c("IMEM", "MEM", "VMEM"),
                             `MIA` = c("IMIA", "MIA"),
                             `MIL` = c("IMIL", "MIL"),
                             `MIN` = c("IMIN", "MIN","VMIN"),
                             `PHI` = c("IPHI", "PHI"),
                             `SAC` = c("ISAC", "SAC"),
                             `SA` = c("IVSA", "SA"),
                             `WSH` = c("IWSH", "WSH"),
                             `POR` = c("VPOR", "POR", "IPOR"),
                             `GS` = c("IIGS", "GS"),
                             `HOU` = c("IHOU", "HOU"),
                             `OKC` = c("IOKC", "OKC"),
                             `ORL` = c("IORL", "ORL"),
                             `TOR` = c("ITOR", "TOR"),
  )) 

# refiltering out pre-pandemic data

pre_pandemic_corrected_data <-
  corrected_pandemic_data%>% 
  filter(pandemic_status == "pre-pandemic")

# looking at the distribution of FG% per season
pre_pandemic_corrected_data %>% 
  group_by(team) %>% 
  ggplot(mapping = aes(x = (season), y = `FG%`)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Field Goal Percentage Distribution per Season Pre-Pandemic",
       x = "Season",
       y = "Field Goal Percentage")

pre_pandemic_corrected_data %>% 
  group_by(season, team) %>% 
  summarise(mean_fg_percent = mean(`FG%`)) %>% 
  ggplot(aes(x = season, y = team, fill = mean_fg_percent)) +
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Average Field Goal Percentage per Team per Season Pre-Pandemic",
       x = "Season",
       y = "Team")
# creating heat map
pre_pandemic_corrected_data %>% 
  ggplot(mapping = aes(x = FGA, y = `FG%`)) +
  geom_hex() +
  labs(title = "Attempted Field Goals vs Field Goal Percentages",
       x = "Number of Attempted Field Goals",
       y = "Field Goal Percentage")

# correcting coaching data labs
corrected_coaching_data <-
  nba_coaching_data %>% 
  mutate(team = fct_collapse(team, 
                             `CHA` = c("CHO", "CHA", "CHH"),
                             `BKN` = c("BRK"),
                             `GS` = c("GSW"),
                             `UTAH` = c("UTA"),
                             `NJ` = c("NJN"),
                             `NO` = c("NOH", "NOK", "NOP"),
                             `NY` = c("NYK"),
                             `PHX` = c("PHO"),
                             `WSH` = c("WAS"),
                             `SA` = c("SAS")
                             
  )) 

pandemic_coaching_data <-
  corrected_coaching_data %>% 
  mutate(pandemic_status = fct_collapse(season, 
                                        `pre-pandemic` = c("2001-2002","2002-2003", "2003-2004",
                                                           "2004-2005","2005-2006","2006-2007",
                                                           "2007-2008","2008-2009","2009-2010",
                                                           "2010-2011","2011-2012", 
                                                           "2012-2013","2013-2014",
                                                           "2014-2015","2015-2016",
                                                           "2016-2017", "2017-2018","2018-2019"),
                                        `pandemic` = c("2019-2020", "2020-2021"),
                                        `post-pandemic` = c("2021-2022")
  )) 
# filtering and joining datasets via keys
pre_pandemic_coaching_data <-
  pandemic_coaching_data %>% 
  filter(pandemic_status == "pre-pandemic")
  
merged_pre_pandemic_data <-
  full_join(pre_pandemic_corrected_data, pre_pandemic_coaching_data)

# looking at relationship between coaching experience and team performance
merged_pre_pandemic_data %>% 
  group_by(year_w_team) %>% 
  summarise(avg_field_goal = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = year_w_team, y = avg_field_goal)) +
  geom_line() +
  labs(title = "Average Field Goal Percentage v Coaching Experience Pre-Pandemic",
       x = "Coaching Experience (Years)",
       y = "Average Field Goal Percentage")
  
  
## Pandemic Data ----  

# subsetting for appropriate data
pandemic_nba_data <-
  corrected_pandemic_data %>% 
  filter(pandemic_status == "pandemic")

# average game conditions
pandemic_nba_data %>% 
  select(GP, MIN) %>% 
  summarise(avg_mins_played = mean(MIN, na.rm = TRUE),
            avg_games_played = mean(GP, na.rm = TRUE))

# looking at the distribution of FG%
pandemic_nba_data %>% 
  ggplot(mapping = aes(x = `FG%`)) +
  geom_freqpoly(bins = 100) +
  labs(x = "Field Goal Percentage",
       y = "Count",
       title = "The Distribution of Field Goal Percentages During the Pandemic")

# analyzing distribution by season
pandemic_nba_data %>% 
ggplot(mapping = aes(x = season, y = `FG%`)) +
  geom_boxplot() +
  labs(x = "Season",
       y = "Field Goal Percentage",
       title = "The Distribution of Field Goal Percentages per Season During the Pandemic")

# comparing `rank` and `FG%`
pandemic_nba_data %>% 
  group_by(rank) %>% 
  filter(rank <= 100) %>% 
  summarise(mean_field_goal = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = rank, y = mean_field_goal)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Rank",
       y = "Average Field Goal Percentage",
       title = "The Average Field Goal Percentage for Players of a Certain Rank During the Pandemic")

# comparing team performance to FG%
pandemic_nba_data %>% 
  group_by(team, season) %>% 
  summarise(mean_field_goal = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = season, y = team, fill = mean_field_goal)) +
geom_tile() +
  labs(title = "Average Field Goal Percentage per Team per Season During the Pandemic",
                   x = "Season",
                   y = "Team")

# filtering out pandemic coaches
pandemic_coaches <-
  pandemic_coaching_data %>% 
  filter(pandemic_status == "pandemic")

# merging data
merged_pandemic_data <-
  full_join(pandemic_nba_data, pandemic_coaches)

# comparing years of experience to `FG%`
merged_pandemic_data %>% 
  group_by(year_w_team) %>% 
  summarise(avg_field_goal = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = year_w_team, y = avg_field_goal)) +
  geom_line() +
  labs(title = "Average Field Goal Percentage v Coaching Experience During the Pandemic",
       x = "Coaching Experience (Years)",
       y = "Average Field Goal Percentage")

## Post-Pandemic Data ----

# subsetting for appropriate data
post_pandemic_nba_data <-
  corrected_pandemic_data %>% 
  filter(pandemic_status == "post-pandemic")

# game conditions
post_pandemic_nba_data %>% 
  select(GP, MIN) %>% 
  summarise(avg_mins_played = mean(MIN, na.rm = TRUE),
            avg_games_played = mean(GP, na.rm = TRUE))

# filtering coaching data 
post_pandemic_coaches <-
  pandemic_coaching_data %>% 
  filter(pandemic_status == "post-pandemic")

# distribution of FG%
post_pandemic_nba_data %>% 
  ggplot(mapping = aes(x = `FG%`)) +
  geom_freqpoly(bins = 100) +
  labs(title = "The Distribution of the Field Goal Percentages Post-Pandemic",
       x = "Field Goal Percentage")

# relationship between `rank` and `FG%`
post_pandemic_nba_data %>% 
  group_by(rank) %>% 
  filter(rank <= 100) %>% 
  summarise(mean_field_goal = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = rank, y = mean_field_goal)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Rank",
       y = "Average Field Goal Percentage",
       title = "The Average Field Goal Percentage for Players of a Certain Rank Post-Pandemic")

# merging data 
merged_post_pandemic_data <- 
  full_join(post_pandemic_nba_data, post_pandemic_coaches)

# comparing relationship between years of experience versus avg `FG%`
merged_post_pandemic_data %>% 
  group_by(year_w_team) %>% 
  summarise(avg_field_goal = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = year_w_team, y = avg_field_goal)) +
  geom_line() +
  labs(title = "Average Field Goal Percentage v Coaching Experience Post-Pandemic",
       x = "Coaching Experience (Years)",
       y = "Average Field Goal Percentage")

## Conclusions ----
# all game conditions
  pandemic_classification_data %>% 
  group_by(pandemic_status) %>% 
  filter(pandemic_status != "NA") %>% 
  select(GP, MIN, PTS) %>% 
  summarise(avg_gp = mean(GP, na.rm = TRUE),
            avg_mins = mean(MIN, na.rm = TRUE),
            avg_points = mean(PTS, na.rm = TRUE))

# comparing FG% across all data
# looking at FG% distribution
pandemic_classification_data %>% 
  group_by(pandemic_status) %>% 
  filter(pandemic_status != "NA") %>% 
  ggplot(mapping = aes(`FG%`)) +
  geom_density(aes(fill=factor(pandemic_status)), alpha = 0.7) +
  labs(title = "The Distribution of the Field Goal Percentages Over the Past 22 Years",
       x = "Field Goal Percentage")

# comparing the distributions to the average
pandemic_classification_data %>% 
  group_by(pandemic_status) %>% 
  filter(pandemic_status != "NA") %>% 
  summarise(avg_field_goal = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = pandemic_status, 
                       y =avg_field_goal,
                       fill = pandemic_status)) +
  geom_col() +
  labs(title = "The Average Field Goal Percentages In Each Pandemic Stage",
       x = "Pandemic Status",
       y = "Average Field Goal Percentage")

# breaking down the numerical
numerical_stats <- pandemic_classification_data %>% 
  group_by(pandemic_status) %>% 
  filter(pandemic_status != "NA") %>% 
summarise(min = min(`FG%`),
            max = max(`FG%`),
            median = median(`FG%`),
            Q1 = quantile(`FG%`, 0.25),
            Q3 = quantile(`FG%`, 0.75),
            IQR = IQR(`FG%`),
            count = n())
