#install.packages("tidyverse")
library(tidyverse)
library(nflreadr)
library(ggrepel)


#load plays from one season
#group by season and play_type, count types, save as dataframe
#filter to plays with injuries in description
#group injuries by play_type and count play_types, save as df
#left_join to play_count df, mutate to pct of injuries by play type
#do again for next year. rjoin 
#graph year_by_year to see changes

all_inj <- data.frame() #only run this one time, to create an empty dataframe

yr <- 2001 #change this for each year (data goes from 1999 through 2022)

plays <- load_pbp(seasons = yr) %>%
  filter(!is.na(play_type), 
         play_type != "qb_kneel",
         play_type != "qb_spike") %>% 
  group_by(season, play_type) %>% summarise(n = n())

plays_inj <- load_pbp(seasons = yr) %>% 
  filter(grepl('injured', desc), 
         !is.na(play_type), 
         play_type != "qb_kneel",
         play_type != "qb_spike") %>% 
  group_by(season, play_type) %>% summarise(n = n())

plays_all <- left_join(plays, plays_inj, 
                        by = c("season" = "season", 
                               "play_type" = "play_type"))

plays_all <- plays_all %>% mutate(pct_inj = (n.y / n.x)*100) #injuries per 100 plays

all_inj <- rbind(all_inj, plays_all)
#go back up, change year, run again

max <- max(all_inj$season)

all_inj %>% 
  ggplot(aes(x = season, y = pct_inj, color = play_type, legend = F)) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  geom_line() +
  geom_label_repel(data = filter(all_inj, season == max), 
                   aes(x = season, y = pct_inj, label = play_type)) +
  theme_light() +
  labs(x = "Season",
       y = "Injuries per 100 plays",
       title = "Injury rates by Play Type",
       subtitle = "From xx to xx")


