###NBA_SportVU FUNCTIONS

library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)

factorconvert <- function(f){as.numeric(levels(f))[f]}

sportvu_convert_json <- function (file.name)
{
  # Much of the process is from http://tcbanalytics.com/blog/nba-movement-data-R.html#.VnX8d4RiOCQ
  # Takes a json and converts it into a dataframe
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
  #Functions finds the distance of the player, assumes you have a dataframe all.movements with player info
  df <- all.movements[which((all.movements$lastname == lastnameA | all.movements$lastname == lastnameB) & all.movements$event.id == eventID),]
  dfA <- df %>% filter (lastname==lastnameA) %>% select (x_loc,y_loc) 
  dfB <- df %>% filter (lastname==lastnameB) %>% select (x_loc,y_loc) 
  df.l <- 1:nrow(dfA)
  distsdf <- unlist(lapply(df.l,function(x) {dist(rbind(dfA[x,], dfB[x,]))}))
  return(distsdf)
}

get_game_clock <- function(lastnameA,eventID){
  #Function gets the glame clock, assumes there is a dataframe all.movements with player info
  alldf <- all.movements[which((all.movements$lastname == lastnameA) & all.movements$event.id == eventID),]
  game_clock <- alldf$game_clock
  return(as.data.frame(game_clock))
}

player_dist_matrix <- function(eventID) {
  #Function creates a matrix of all player/ball distances with each other
  #assumes there a dataframe all.movements with player info
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
  #Grabs the play by play data from the NBA site
  URL1 <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=",gameid,"&RangeType=2&StartPeriod=1&StartRange=0",sep = "")
  the.data.file<-fromJSON(URL1)
  test <-the.data.file$resultSets$rowSet
  test2 <- test[[1]]
  test3 <- data.frame(test2)
  coltest <- the.data.file$resultSets$headers
  colnames(test3) <- coltest[[1]]
  return (test3)}

chull_area <- function(X,Y){
  #Calculates the convex hull area
  df_hull <- data.frame(X = X, Y = Y)
  c.hull <- chull(df_hull)
  #You need five points to draw four line segments, so we add the first set of points at the end
  c.hull <- c(c.hull, c.hull[1])
  chull.coords <- df_hull[c.hull ,]
  chull.poly <- Polygon(chull.coords, hole=F)
  chull.area <- chull.poly@area
  return (chull.area)}

chull_areabyteam <- function (total,balltime) {
  #Function returns a dataframe with event id and convex hull area for each team
  #Function requires an input of a dataframe with the rotated plays and a dataframe indicating event/time
  #for calculating convex hull area
  allsum <- NULL
  teams <- as.list((unique(total$team_id)))
  teams <- teams[!is.na(teams)]
  for(i in 1:(nrow(balltime))) 
  {temp <- total %>% filter(event.id == balltime$event.id[i] & game_clock == balltime$clock28[i])  %>% filter(lastname!="ball")
  if (nrow(temp) == 10) {
    dfall <- lapply(teams,function (x) { df <- temp %>% filter(team_id == x)
    if (nrow(df) == 5) {area <- (chull_area(df$x_loc_r,df$y_loc_r))
    area}
    })
    df <- cbind(balltime$event.id[i],teams[[1]],dfall[[1]],teams[[2]],dfall[[2]])  
    allsum <- rbind(df,allsum)
  }
  }
  allsum <- as.data.frame(allsum)
  colnames(allsum)<-c("event.id","team1","team1_area","team2","team2_area")
  return(allsum)
}        

player_position <- function(eventid,gameclock){
  ##Returns positions of all players at a time
  ##Requires data in total and balltime
    dfall <- total %>% filter(game_clock == gameclock,event.id=eventid)  %>% 
      filter(lastname!="ball") %>% select (team_id,x_loc_r,y_loc_r)
    colnames(dfall) <- c('ID','X','Y')
    return(dfall)
  }

chull_plot <- function(event.id,game_clock) {
  ##Returns a data frame with the coordinates of a convex hull
  ##Requires player_position for info
    df2 <- player_position(event.id,game_clock)
    df_hull2 <- df2 %>% filter(ID == min(ID)) %>% select(X,Y)
    df_hull3 <- df2 %>% filter(ID == max(ID)) %>% select(X,Y)
    c.hull2 <- chull(df_hull2)
    c.hull3 <- chull(df_hull3)
    #You need five points to draw four line segments, so we add the fist set of points at the end
    c.hull2 <- c(c.hull2, c.hull2[1])
    c.hull3 <- c(c.hull3, c.hull3[1])
    df2 <- as.data.frame(cbind(1,df_hull2[c.hull2 ,]$X,df_hull2[c.hull2 ,]$Y))
    df3 <- as.data.frame(cbind(2,df_hull3[c.hull3 ,]$X,df_hull3[c.hull3 ,]$Y))
    dfall <- rbind(df2,df3)
    colnames(dfall) <- c('ID','X','Y')
    return(dfall)
  }

chull_plot_centroid <- function(event.id,game_clock) {
  ##Returns a data frame with the centroid of a convex hull
  ##Requires player_position for info
      df2 <- player_position(event.id,game_clock)
      df_hull2 <- df2 %>% filter(ID==min(ID)) %>% select(X,Y)
      df_hull3 <- df2 %>% filter(ID==max(ID)) %>% select(X,Y)
      c.hull2 <- chull(df_hull2)
      c.hull3 <- chull(df_hull3)
      df2centroid <- c(1,mean(df_hull2[c.hull2 ,]$X),mean(df_hull2[c.hull2 ,]$Y))
      df3centroid <- c(2,mean(df_hull3[c.hull3 ,]$X),mean(df_hull3[c.hull3 ,]$Y))
      dfall <- as.data.frame(rbind(df2centroid,df3centroid))
      colnames(dfall) <- c('ID','X','Y')
      return(dfall)
}

chull_plot_area <- function(event.id,game_clock) {
  ##Returns a data frame with the area of each convex hull by team ID
  ##Requires player_position for info
  df2 <- player_position(event.id,game_clock)
  df2area <- df2 %>% group_by(ID) %>% summarise (area = chull_area(X,Y)) %>% select (ID,area)
  return (df2area)
}

velocity <- function(xloc, yloc){
  diffx <- as.vector((diff(xloc)))
  diffy <- as.vector((diff(yloc)))
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)*25 #(distance in feet per second)
  b
}

acceleration <- function(xloc, yloc){
  diffx <- as.vector((diff(xloc,differences = 2)))
  diffy <- as.vector((diff(yloc,differences = 2)))
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)*25*25 #(distance in feet per second)
  b
}

jerk <- function(xloc, yloc){
  diffx <- as.vector((diff(xloc,differences = 3)))
  diffy <- as.vector((diff(yloc,differences = 3)))
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)*25/32.173*25*25 #(distance in feet per second)
  b
}

