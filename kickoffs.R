#install.packages("tidyverse")
library(tidyverse)
library(nflreadr)
library(ggrepel)

plays %>% 
  group_by(specialTeamsPlayType) %>% 
  summarise(n = n())

names(tracking2018)

tracking2018 %>% filter(gameId == 2018123000 & playId == 36 & event == "tackle")
ko18 <- tracking2018 %>% filter(event == "kickoff")
kickoffs18 <- load_pbp(seasons = 2018) %>% filter(play_type == "kickoff")

kos_inj18 <- kickoffs18 %>% filter(grepl('injured', desc))

inj18 <- load_pbp(seasons = 2018) %>% 
  filter(grepl('injured', desc)) %>% 
  group_by(play_type) %>% 
  summarise(n = n())

plays18 <- load_pbp(seasons = 2018) %>% group_by(play_type) %>% summarise(n = n())

plays18 <- left_join(plays18, inj18, by = "play_type")

plays18 <- plays18 %>% mutate(inj_rt = n.y / n.x)


#load plays from many seasons
#group by season and play_type, count types, save as dataframe
#filter to plays with injuries in description
#group injuries by play_type and count play_types, save as df
#left_join to play_count df, mutate to pct of injuries by play type
#graph year_by_year to see changes

plays10_22 <- load_pbp(seasons = 2010:2022) %>% 
  group_by(season, play_type) %>% summarise(n = n())

plays10_22_inj <- load_pbp(seasons = 2010:2022) %>% 
  filter(grepl('injured', desc)) %>% 
  group_by(season, play_type) %>% summarise(n = n())

plays10_22_inj %>% filter(play_type == "kickoff" | play_type == "punt") %>% 
  ggplot(aes(x = season, y = n, color = play_type)) +
  geom_line()

plays10_22 <- left_join(plays10_22, plays10_22_inj, 
                        by = c("season" = "season", 
                               "play_type" = "play_type"))

plays10_22 <- plays10_22 %>% mutate(pct_inj = (n.y / n.x)*100)

plays10_22 %>% 
  ggplot(aes(x = season, y = pct_inj, color = play_type)) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  geom_line() +
  geom_label_repel(data = filter(plays10_22, season == "2022"), 
                   aes(x = season, y = pct_inj, label = play_type),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme_light() +
  labs(x = "Season",
       y = "Injuries per 100 plays",
       title = "Injury rates by Play Type")
