library(animation)
library(ggplot2)
library(gganimate)
library(readr)
source("_function_fullcourt.R")

##Command to create video, it can take several minutes to create a video
make_video(final, "0021500001", "3")



get  # Function to create video
make_video <- function(df = df, gameID, playID) {
  toy_play <- df %>% filter(gameid == gameID, playid == playID)
  if (nrow(toy_play) > 0) {
    g <- fullcourt() +
        geom_point(data = toy_play,
                   aes(x = x_loc, y = y_loc, frame = 720 - game_clock,
                       color = factor(team_id)), size = 3)
    filen <- paste("anim_plays/", gameID, "_", playID, "_play.mp4", sep = "")
    gg_animate(g, filename = filen, saver = "mp4", convert = "gm convert",
               interval = .05, title_frame = F)
  } else {
    print("no data")
  }

}


