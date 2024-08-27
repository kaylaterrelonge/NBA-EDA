# EDA Exec Summary ----

## Load Packages ----
library(tidyverse)
library(dplyr)
library(lvplot)
library(stringr)
library(forcats)
library(skimr)
library(lattice)
library(gridExtra)

## Data Import and Manipulation ----
nba_player_data <- read_csv("data/processed_data/nba_data_processed.csv")
nba_coaching_data <-  read_csv("data/processed_data/coaching_data_processed.csv")

# adding pandemic status as a variable
final_nba_player_data<- nba_player_data %>% 
  mutate(pandemic_status = fct_collapse(season, 
                                        `pre-pandemic` = c("2001-2002","2002-2003", 
                                                           "2003-2004",
                                                           "2004-2005","2005-2006",
                                                           "2006-2007",
                                                           "2007-2008","2008-2009",
                                                           "2009-2010",
                                                           "2010-2011","2011-2012", 
                                                           "2012-2013","2013-2014",
                                                           "2014-2015","2015-2016",
                                                           "2016-2017", "2017-2018",
                                                           "2018-2019"),
                                        `pandemic` = c("2019-2020", "2020-2021"),
                                        `post-pandemic` = c("2021-2022")
  ),
  # fixing labeling error in data 
  team = fct_collapse(team, 
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

# repeating process with coaching data set
final_nba_coaching_data <-
  nba_coaching_data %>% 
  # fixing labeling error
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
                             
  ),
  # adding pandemic status
pandemic_status = fct_collapse(season, 
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

# merging data for multiple data set analysis
merged_nba_data <-
  full_join(final_nba_data, final_nba_coaching_data)

## Plots ----
# the distribution of field goal percentages divided by pandemic status
p1 <- final_nba_player_data %>% 
  group_by(pandemic_status) %>% 
  filter(pandemic_status != "NA") %>% 
  ggplot(mapping = aes(`FG%`)) +
  geom_density(aes(fill=factor(pandemic_status)), alpha = 0.7) +
  labs(title = "The Distribution of the Field Goal Percentages Over the Past 22 Years",
       x = "Field Goal Percentage")

p2 <- merged_nba_data %>% 
  group_by(pandemic_status, year_w_team) %>% 
  filter(pandemic_status != "NA") %>% 
  summarise(mean = mean(`FG%`)) %>% 
  ggplot(mapping = aes(x = year_w_team, y = mean)) +
  geom_line()+
  facet_wrap(~pandemic_status) +
  labs(title = "Coaching Experience v. Average Field Goal Percentages ",
       x = "Years With Team",
       y = "Average Field Goal Percentage")







