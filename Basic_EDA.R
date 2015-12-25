###NBA_SportVU EDA

source("_functions.R")
library(plotly)

#########LOAD DATA

## Grab the data using: wget https://github.com/neilmj/BasketballData/blob/master/2016.NBA.Raw.SportVU.Game.Logs/12.18.2015.DET.at.CHI.7z?raw=true
## The munge for this file takes 7 minutes on my computer and results in a dataframe 3258100 obs. of 13 variables
# See NBA stats for this game at http://stats.nba.com/game/#!/0021500391/playbyplay/

all.movements <- sportvu_convert_json("data/0021500391.json")
write.csv(all.movements,"all.movements_0021500391.csv", row.names = FALSE)

########Extract Movement for One Player
## Code from http://tcbanalytics.com/blog/nba-movement-data-R.html#.VnX8d4RiOCQ
## Play 6 by Rose - http://on.nba.com/1OoYi7e
all.movements <- all.movements %>% dplyr::arrange(quarter,desc(game_clock),x_loc)
id6 <- all.movements[which(all.movements$event.id == 6),]
rose <- all.movements[which(all.movements$lastname == "Rose" & all.movements$event.id == 6),]
gasol <- all.movements[which(all.movements$lastname == "Gasol" & all.movements$event.id == 6),]


plot_ly(data = rose, x = x_loc, y = y_loc, mode = "markers", color=cut(rose$game_clock, breaks=3)) %>% 
    layout(xaxis = list(range = c(0, 100)), 
           yaxis = list(range = c(0, 50))) 

plot_ly(data = gasol, x = x_loc, y = y_loc, mode = "markers") %>% 
  layout(xaxis = list(range = c(0, 100)), 
         yaxis = list(range = c(0, 50))) 

########Extract Distance for One Player
## Play 6 by Rose - http://on.nba.com/1OoYi7e

travelDist(rose$x_loc, rose$y_loc)
# 76.39166 

travelDist(gasol$x_loc, gasol$y_loc)
# 87.84398

########Extract Distance for Group of Players for one event
seconds = max(gasol$game_clock) - min(gasol$game_clock)
speed = travelDist(gasol$x_loc, gasol$y_loc)/seconds  #in feet per second

########Extract Distance for Group of Players for one event
player.groups <- group_by(id6, lastname)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc),playerid = max(player_id))
arrange(dist.traveled.players, desc(totalDist))


########Extract Distance for Group of Players for the entire game
#Need to dedup data because multiple events for different event.ids
deduped.data <- unique( all.movements[ , 1:12 ] )
player.groups <- group_by(deduped.data, lastname)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc),playerid = max(player_id))
total <- arrange(dist.traveled.players, desc(totalDist))

########Extract Distance for one player for the entire game
gasol <- deduped.data[which(deduped.data$lastname == "Gasol"),]
travelDist(gasol$x_loc, gasol$y_loc)
# 18561.93 - Seems a little high, but this was a very long game



