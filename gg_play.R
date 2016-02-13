library(shiny)
library(miniUI)
library(ggplot2)
source("_function_fullcourt.R")

gg_play <- function(data) {
  
  ui <- miniPage(
    gadgetTitleBar("See Play"),
    miniContentPanel(
      sliderInput("inSlider2", "GameClock:",
                  min = 1, max = 500, value = 1,step = 8,animate=TRUE),
      plotOutput("plot_hull", height = "100%")
    )
  )
  
  server <- function(input, output, session) {
    
    output$plot_hull <- renderPlot({
      step <- length(unique(data$game_clock))
      players <- length(unique(data$player_id))
      clock <- unique(data$game_clock)
      data <- data %>% 
              filter(game_clock == clock[[input$inSlider2]])
      data$team_id <- ifelse(data$player_id=="-1",1,data$team_id)
      
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
       g<- fullcourt() +
        geom_point(data=data,aes(x=x_loc,y=y_loc),group=data$team_id,color=dense_rank(data$team_id),size=3,shape=8)+
        theme_bw()+theme(panel.grid=element_blank(), legend.title=element_blank(), panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
      
      g 
    })
  }
  
  runGadget(ui, server)
}
