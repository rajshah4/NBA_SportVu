##Three Second Violations
library(readr)
library(dplyr)

##my machine couldn't load all the games at once, so I hard coded this loop

for (i in 1:3) {
  if (i ==1) { games <- read_csv("data/threepointplays/threepointers_1_200.csv.zip") }
  if (i ==2) { games <- read_csv("data/threepointplays/threepointers_201_400.csv.zip") }
  if (i ==3) { games <- read_csv("data/threepointplays/threepointers_401_631.csv.zip") } 
  
  games <- games  %>% 
    filter(player_id != -1) 
  
  games <- games %>%  group_by(gameid,playid) %>% 
    mutate (Oteam = ifelse (player_id==PLAYER1_ID,team_id,0)) %>% 
    mutate (Oteam2 = ifelse (team_id==max(Oteam),0,1))##0 is offense
  
  allplayers <- games  %>% 
    filter(player_id != -1) %>% 
    filter (((x_loc > 0 & x_loc < 19)) & (y_loc > 17 & y_loc < 33)) %>% 
    group_by(gameid,playid,team_id,player_id,Oteam2,shooter = PLAYER1_ID) %>% 
    mutate(dt = max(game_clock) - min(game_clock)) %>% 
    mutate(x = round(lag(game_clock) - game_clock),digits=2) %>% 
    mutate (diffs =  max(diff(c(0,  c(which(x[-1L] != x[-length(x)]), length(x)))))) %>% 
    summarize (eventid = max(event.id),dt = max(game_clock) - min(game_clock), three= max(diffs)*.04,clock = max(game_clock),quarter=max(quarter)) %>% 
    filter (Oteam2 == 0)   %>% 
    filter (three > 3)
  
  if (i == 1) {total <- allplayers} else {total <- bind_rows(allplayers,total)}
}

write.csv(total,"threesecondviolations.csv",row.names = FALSE)
