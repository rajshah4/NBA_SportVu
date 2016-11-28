## Get Three POint Shots

library(dplyr)
# setwd("/media/ladmin/Data1/Code/basketball/Sportvu")
library(foreach)
library(doParallel)
library(parallel)
source("_functions.R")
library(iterators)
library(stringr)
library(lubridate)

numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

pathfl = "data/"
pathflpbp = "data/pbp/"  # Path to where you have play by play data stored (pbp)

allFiles =
    list.files(path = pathfl, pattern = "*.gz")  # Assuming files are in
                                                 # data.frame format, but
                                                 # compressed as *.gz files

# Simple Function needed during script
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r + 1, nrow(existingDF) + 1), ] <- existingDF[
      seq(r, nrow(existingDF)), ]
  existingDF[r, ] <- newrow
  existingDF
}

##Select which files to run  . . . with 16GB of memory, I was not able to do all 631 games at once
allFiles <- allFiles[1:200]
#allFiles <- allFiles[201:400]
#allFiles <- allFiles[401:631]
#allFiles <- allFiles[1:1]  ##This is for testing


rowCounts_chull <- foreach(
                       filename = allFiles, .packages = c(
                                                'dplyr', 'readr', 'lubridate',
                                                'stringr')) %dopar%
    {
      # filename = "0021500001" #This is for testing

      #Load game data
      gameid <- gsub(".csv.gz", "", filename)
      filen <- paste0(pathfl, gameid, ".csv.gz", sep = "")
      filep <- paste0(pathflpbp, gameid, "_pbp.txt", sep = "")
      all.movements <- read.csv(filen)
      pbp <- read.csv(filep)
      df_total <- NULL

      ##Filter down data to ball in the air
      df_game <- all.movements %>%
          select(-X) %>% filter(player_id == "-1") %>% filter(radius > 8) %>%
          #XY distance to the basket
          mutate(threedist = ifelse(
                     x_loc < 47,
                     {
                       sqrt((x_loc - 5.25) ^ 2 + (y_loc - 25) ^ 2)
                     }, {
                          sqrt((x_loc - 88.75) ^ 2 + (y_loc - 25) ^ 2)
                        })) %>%
          #XYZ distance to the basket
          mutate(threedistz = ifelse(
                     x_loc < 47,
                     {
                       sqrt((x_loc - 5.25) ^ 2 + (y_loc - 25) ^ 2 +
                            (radius - 10) ^ 2)
                     }, {
                          sqrt((x_loc - 88.75) ^ 2 +
                               (y_loc - 25) ^ 2 + (radius - 10) ^ 2)
                        })) %>% arrange(quarter, desc(game_clock)) %>%
          distinct(game_clock, .keep_all = TRUE)


      ## Find the start and end of plays when ball is in the air
      shot_break_end <- df_game %>%
          mutate(lead_game_clock = lead(game_clock, n = 1)) %>%
          filter(game_clock - lead_game_clock > 1) %>%
          distinct(game_clock, quarter) %>%
          select(game_clock_end = game_clock, quarter)

      shot_break_start <- df_game %>%
          mutate(lag_game_clock = lag(game_clock, n = 1)) %>%
          filter(lag_game_clock - game_clock > 1) %>%
          distinct(game_clock, quarter) %>%
          select(game_clock_start = game_clock, quarter)

      ##Creates dataframe with start and end times of ball in the air
      r <- 1
      newrow <- c(df_game$game_clock[1], df_game$quarter[1])  # Start with first
                                                              # time
      length <- nrow(shot_break_start)
      shot_row <- shot_break_start[length, ]
      shot_break_start <- insertRow(shot_break_start, newrow, r)
      shot_break_end <- bind_rows(shot_break_end, shot_row)  # Add the last time
      shot_break <- cbind(shot_break_start, shot_break_end)

      ##Now that we have the start/end times, lets start by filtering out our dataset to these times
      ##Also, lets get rid of any plays that are less than 22 feet
      ##Assign a new id to these plays - shot_id
      sumtotal <- NULL
      for (i in 1:nrow(shot_break)) {
        df_event <- df_game %>%
            filter(quarter == shot_break$quarter[i] &
                   game_clock <= shot_break$game_clock_start[i] &
                   game_clock > shot_break$game_clock_end[i]) %>%
            filter(max(threedist) - min(threedist) > 22) %>% mutate(shot_id = i)
        sumtotal <- bind_rows(df_event, sumtotal)
      }
      ##This gives us a dataframe of the ball in air, on plays, where it goes greater than 22 feet

      ##The next step is matching this data to the play by play data:

      ##This brings in all the 3 points shots in the play by play data
      ##This is one way to bring in additional informaton in
      pbp_shot <- pbp %>%
          select(EVENTNUM, EVENTMSGTYPE, EVENTMSGACTIONTYPE, HOMEDESCRIPTION,
                 VISITORDESCRIPTION, PCTIMESTRING, PERIOD, PLAYER1_ID)
      pbp_shot$HOMEDESCRIPTION <- as.character(pbp_shot$HOMEDESCRIPTION)
      pbp_shot$VISITORDESCRIPTION <- as.character(pbp_shot$VISITORDESCRIPTION)
      pbp_shot$threepoint <- ifelse(
          grepl("3PT", pbp_shot$VISITORDESCRIPTION) |
          grepl("3PT", pbp_shot$HOMEDESCRIPTION), 1, 0)
      pbp_shot <- pbp_shot %>% filter(threepoint == 1)
      pbp_shot$game_clock <- period_to_seconds(
          ms(as.character(pbp_shot$PCTIMESTRING)))

      #unique(sumtotal2$shot_id)
      sumtotal3 <- NULL
      for (q in 1:4) {
        df_merge <- sumtotal %>% filter(quarter == q)
        if (nrow(df_merge) > 0) {
          events <- unique(df_merge$shot_id)
          pbp_q <- pbp_shot %>% filter(PERIOD == q)
          for (i in 1:length(events)) {
            df_merge2 <- df_merge %>% filter(shot_id == events[i])
            merge_time <- min(df_merge2$game_clock)
            timeb <- ifelse(abs(pbp_q$game_clock - merge_time) < 5, 1,
                            0)  # merges if the pbp time is within 5 seconds
            indexc <- match(1, timeb)
            if (Reduce("+", timeb) > 0) {
              df_merge2$EVENTNUM <- pbp_q$EVENTNUM[indexc]
              df_merge2$EVENTMSGTYPE <- pbp_q$EVENTMSGTYPE[indexc]
              df_merge2$PLAYER1_ID <- pbp_q$PLAYER1_ID[indexc]
            } else {
              df_merge2$EVENTNUM <- 999  # 999 indicates no match
              df_merge2$EVENTMSGTYPE <- 999
              df_merge2$PLAYER1_ID <- 999
            }
            sumtotal3 <- bind_rows(df_merge2, sumtotal3)
          }
        }
      }
      sumtotal3 <- sumtotal3 %>% filter(EVENTMSGTYPE != '999')  # Remove any no
                                                                # match plays

      ##Now we have a dataframe of 3 point plays from when the ball leaves the shooters hand to when it reaches the basket
      
      ##Finds the point where the ball leaves the shooters hand
      df_startshot <- sumtotal3 %>%
          group_by(shot_id) %>% filter(row_number() == 1) %>% ungroup() %>%
          select(shot_id, EVENTMSGTYPE, game_clock, quarter, PLAYER1_ID,
                 shot_clock) %>% arrange(quarter, desc(game_clock))


      ##loops through each three point play
      for (i in 1:nrow(df_startshot)) {

        ##Get start of the play
        df_startplay <- all.movements %>%
            filter(quarter == df_startshot$quarter[i] &
                   game_clock >= df_startshot$game_clock[i]) %>%
            filter(player_id == "-1") %>%
            distinct(quarter, game_clock, .keep_all = TRUE) %>%
            arrange(quarter, game_clock) %>% filter(!is.na(shot_clock)) %>%
            mutate(lead_shot_clock = lead(shot_clock, n = 1)) %>%
            filter(shot_clock - lead_shot_clock > 1) %>% head(1)
        ##Get the ball/player data now that we have the start/end time
        if (nrow(df_startplay) > 0) {
          ##Subset down to just data for this play based on length of play
          df_play <- all.movements %>%
              filter(quarter == df_startshot$quarter[i] &
                     game_clock <= (df_startplay$game_clock) &
                     game_clock >= df_startshot$game_clock[i]) %>%
              # df_play <- all.movements %>% filter (quarter==df_startshot$quarter[i] & game_clock <= (df_startshot$game_clock[i]+length_of_play) & game_clock >= df_startshot$game_clock[i]) %>%
              mutate(playid = i) %>%
              distinct(player_id, quarter, game_clock, .keep_all = TRUE) %>%
              arrange(desc(game_clock), player_id)
          #Rotate plays depending upon location of the shot
          if (tail(df_play$x_loc, 1) > 47) {
            df_play <- df_play %>%
                mutate(x_loc = 94 - x_loc) %>% mutate(y_loc = 50 - y_loc)
          }
          df_play$gameid <- gameid
          df_play$EVENTMSGTYPE <- df_startshot$EVENTMSGTYPE[i]  # Adding in some
                                                                # of the pbp
                                                                # data
          df_play$PLAYER1_ID <- df_startshot$PLAYER1_ID[i]
          df_total <- bind_rows(df_total, df_play)
        }
      }
      df_total

    }


final <- bind_rows(rowCounts_chull) %>%
    arrange(gameid, playid, desc(game_clock)) %>%
    select(-X, --a_score, -h_score) %>%
    arrange(gameid, playid, desc(game_clock))

write.csv(final, "threepointers_201_400.csv", row.names = FALSE)

##Validate findings
test <- final %>% group_by(gameid, playid) %>% summarize(count = n())
summary(test$count)

##Get specific plays
testplay <- final %>% filter(gameid == '0021500418' & playid == '15')
testplay <- final %>% filter(playid == '21')
testplayball <- testplay %>% filter(player_id == '-1')

