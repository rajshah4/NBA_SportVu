###NBA_SportVU FUNCTIONS

library(RCurl)
library(jsonlite)
library(dplyr)

factorconvert <- function(f){as.numeric(levels(f))[f]}

sportvu_convert_json <- function (file.name)
{
  # Much of the process is from http://tcbanalytics.com/blog/nba-movement-data-R.html#.VnX8d4RiOCQ
  the.data.file<-fromJSON(file.name)
  ##Get the sports vu data
  moments <- the.data.file$events$moments
  
  ##Function for extracting infomration from JSON
  extractbb <- function (listbb)
  {#df <- unlist(listbb,recursive = FALSE)
    df <- listbb
    # str(df)
    quarters <- unlist(lapply(df, function(x) x[[1]]))
    game.clock <- unlist(lapply(df, function(x) x[[3]]))
    shot.clock <- unlist(lapply(df, function(x) ifelse(is.null(x[[4]]), 'NA', x[[4]])))
    moment.details <- (lapply(df, function(x) x[[6]]))
    x3 <-  mapply(cbind, moment.details, game.clock, shot.clock,quarters, SIMPLIFY=F)
    x4 <- do.call('rbind', x3)
    return (x4)
  }
  
  test2 <- lapply(moments, function (x) {extractbb(x)})
  lengthmm <- the.data.file$events$eventId
  test2 <- mapply(cbind, test2, "event.id"=lengthmm, SIMPLIFY=F)
  
  #Remove events that are NAs
  final <- (lapply(test2, function(x) {
    if ((length(unlist(x)))<=1) {x <- NA} 
    return(x)
  }))
  
  ###Merge the file
  test2 <- do.call('rbind', final)
  test2 <- as.data.frame(test2)
  test2[test2 == "NA" ] = NA
  all.movement <- test2
 #all.movement<-test2[order(test2$game.clock),]
  
  ##Lets join the movement to player id
  headers = c("team_id", "player_id", "x_loc", "y_loc", "radius", "game_clock", "shot_clock", "quarter","event.id")
  colnames(all.movement) <- headers
  all.movement<-data.frame(all.movement)
  all.movement<-all.movement[order(all.movement$game_clock),]
  
  home.players <- the.data.file$events$home$players[[1]]
  away.players <- the.data.file$events$visitor$players[[1]]
  colnames(home.players)[3] <- "player_id"
  colnames(away.players)[3] <- "player_id"
  
  ## Add the player name information to each movement moment
  home.movements<-merge(home.players, all.movement, by="player_id")
  away.movements<-merge(away.players, all.movement, by="player_id")
  ball.movement<-all.movement %>% filter(player_id == -1)
  ball.movement$jersey <- NA
  ball.movement$position <- NA
  ball.movement$team_id <- NA
  ball.movement$lastname <- "ball"
  ball.movement$firstname <- NA
  all.movements <- rbind(home.movements, away.movements,ball.movement)
  all.movements[, 6:13] <- lapply(all.movements[, 6:13], factorconvert)
  all.movements <- as.data.frame(all.movements) %>% dplyr::arrange(quarter,desc(game_clock),x_loc,y_loc)
  return(all.movements)
}

## Function to calculate player distance traveled
travelDist <- function(xloc, yloc){
  diffx <- diff(xloc)
  diffy <- diff(yloc)
  ##Removes big jumps - Limiting to changes of less than 1 foot per .04 seconds means 
  # anything over 17 mph will be excluded, this seems reasonable
  diff <- as.data.frame(cbind(diffx,diffy))
  diff  <- subset(diff, abs(diffx) < 1 & abs(diffy) < 1)
  ##Back to code
  diffx <- as.vector(diff$diffx)
  diffy <- as.vector(diff$diffy)
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)
  (sum(b))  
}

player_dist <- function(lastnameA,lastnameB, eventID) {
  df <- all.movements[which((all.movements$lastname == lastnameA | all.movements$lastname == lastnameB) & all.movements$event.id == eventID),]
  dfA <- df %>% filter (lastname==lastnameA) %>% select (x_loc,y_loc) 
  dfB <- df %>% filter (lastname==lastnameB) %>% select (x_loc,y_loc) 
  df.l <- 1:nrow(dfA)
  distsdf <- unlist(lapply(df.l,function(x) {dist(rbind(dfA[x,], dfB[x,]))}))
  return(distsdf)
}

get_game_clock <- function(lastnameA,eventID){
  alldf <- all.movements[which((all.movements$lastname == lastnameA) & all.movements$event.id == eventID),]
  game_clock <- alldf$game_clock
  return(as.data.frame(game_clock))
}

player_dist_matrix <- function(eventID) {
  
  players <- all.movements %>% filter(event.id==pickeventID) %>% select(lastname) %>% distinct(lastname)
  players2 <- players
  bigdistance <-unlist(lapply(list(players$lastname)[[1]], function(X) {
    lapply(list(players2$lastname)[[1]], function(Y) {test=
      player_dist(X, Y,pickeventID)
    })
  }), recursive=FALSE)
  bigdistance_names <-unlist(lapply(list(players$lastname)[[1]], function(X) {
    lapply(list(players2$lastname)[[1]], function(Y) {
      paste(X, Y,sep = "_")
    })
  }), recursive=FALSE)
  bigdistancedf <- as.matrix(do.call('cbind',bigdistance))
  colnames(bigdistancedf) <- bigdistance_names
  bigdistancedf <- bigdistancedf[,colSums(bigdistancedf^2) !=0]
  bigdistancedf <- as.data.frame(bigdistancedf)
  clockinfo <- get_game_clock("ball",eventID)
  bigdistancedf$game_clock <- clockinfo$game_clock
  return (bigdistancedf)
}

get_pbp <- function(gameid){
  URL1 <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=",gameid,"&RangeType=2&StartPeriod=1&StartRange=0",sep = "")
  the.data.file<-fromJSON(URL1)
  test <-the.data.file$resultSets$rowSet
  test2 <- test[[1]]
  test3 <- data.frame(test2)
  coltest <- the.data.file$resultSets$headers
  colnames(test3) <- coltest[[1]]
  return (test3)}