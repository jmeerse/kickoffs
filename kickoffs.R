#install.packages("tidyverse")
library(tidyverse)

plays %>% 
  group_by(specialTeamsPlayType) %>% 
  summarise(n = n())


