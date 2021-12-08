# title: "R Shiny application for interactive, user-modified recurrence quantification analysis plots"
# author: "Joshua Sheldon, Dr. Erin N. McCormick, Dr. Leslie M. Blaha"
# date: "December 06, 2021"


# LICENSE ##############################
#
# R Shiny application for interactive, user-modified recurrence quantification analysis plots.
# Copyright (C) 2021 Joshua Sheldon, Erin N. McCormick, Leslie M. Blaha
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# README ##############################
#
# This file provides a R Shiny application that allows you to 
# interactively edit a single recurrence plot.
#
# We recommend using this tool to help develop you visual expertise 
# in reading and understanding recurrence plots. We feel that interactively 
# creating and modifying a variety of recurrence plots can help researchers 
# make the connection between a given plot and the event sequence producing it.
#
# Original code written by Joshua Sheldon, during an United States Air Force 
# Academy internship with Dr. Blaha and Dr. McCormick.


# Required Packages  ##############################

library(shiny)
library(tidyverse)
library(shinyjs)
library(colourpicker)
library(crqa)
library(DT)


# User Interface  ##############################

ui <- fluidPage(
  
  useShinyjs(),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tags$head(tags$style('h4 {color:gray;}')),
  tags$head(tags$style('h3 {color:#4B68B8;}')),
  
  titlePanel("Recurrence Quanitification Analysis plot design"),
  
  h3("User Designed RQA Plot", align = "center"),
  plotOutput(outputId = "rqa"),
  h3("Recurrence Plot Design", align = "center"),
  
  fluidRow(
    column(4,
           align = "center",
           h4("Color Customization", align = "center"),
           actionButton("stateBut", "Event State"),
           actionButton("recBut", "Recurrence"),
           colourInput("colLoi", "Color for line of incidence", value = "#5b6770"),
           hidden(colourInput("colRec", "Color for recurrence", value = "#000000")),
           hidden(colourInput("col1", "Color for choice 1", value = "#b0e472")),
           hidden(colourInput("col2", "Color for choice 2", value = "#5e904e")),
           colourInput("colNon", "Color for non-recurrence", value = "#ffffff"),
    ),
    
    column(4,
           align = "center",
      h4("Strategy", align = "center"),
      actionButton("randomBut", "Random Choice"),
      actionButton("altBut", "Alternating"),
      actionButton("textBut", "User Input Sequence"),
      hidden(textInput("text", "Enter sequence (A or B)", "A,B,A,B,B")),
      hidden(sliderInput(inputId = "rand", "Probability of choice '1' (0% to 100%) ", value = 50, min = 0, max = 100, step = 5)),
      hidden(numericInput(inputId = "alt", "Alternate every # of trials", value = 1, min = 1, step = 1)),
      hidden(sliderInput(inputId = "noise", "Choose desired noise % (0 to 100)", value = 0, min = 0, max = 100, step = 5)),
      
    ),
    column(4,
           align = "center",
           h4("Length of Sequence", align = "center"),     
           actionButton("slideBut", "Slider Input"),
           actionButton("numBut", "Numeric Input"), 
           hidden(sliderInput("seqSlide", "", min = 0, max = 400, value = 100, step = 10)),
           hidden(numericInput("seqNum", "", value = 100, min = 1)),
           h4("Show/Hide Recurrence Statistics", align = "center"),
           actionButton("statsBut", "Toggle Statistics"),
    ),
    
  ),
  fluidRow(
   column(6,
      align = "center", 
      offset = 3,
     h3("Recurrence Statistics", align = "center", id = "stats"),
         verbatimTextOutput("data"),
   )
  ),
  # fluidRow(
  # 
  #   column(4,
  #          align = "left",
  # 
  #          div(img(src="cmu.png", width = 70, height = 70)),
  #   ),
  #   column(4,
  # 
  #          align = "center",
  #          div(img(src="afrl.jpg", width = 150, height = 70)),
  #   ),
  #   column(4,
  # 
  #          align = "right",
  #          div(img(src="usafa.png", width = 50, height = 50)),
  #   ),
  # )
)


# Server functions  ##############################

server <- function(input, output, session) {
  
  observe({
  updateSliderInput(session, "seqSlide")
  })
  observe({
    updateTextInput(session, "text")
  })
  observe({
    updateNumericInput(session, "seqNum")
  })

 
  
   seq <- reactiveValues(update = 100, type = "start")
     
   
  newSeq <- function()
  {
    # cat(paste0("reached 107: seq$type is ",seq$type))
    if(seq$type == "slider"){
      # cat("passed if statement slider")
      if(!is.na(input$seqSlide)){
        # cat("seq slider is not missing")
        seq$update <- input$seqSlide
     }
   }
    else if(seq$type == "numeric"){
      # cat("passed if statement numeric")
      if(!is.na(input$seqNum)){
        # cat("seq num is not missing")
         seq$update <- input$seqNum
      }
    }
    else{
      # cat("remaining else")
    }
  }

   
  
  strategy <- reactiveValues(sequence = c("A","B","A","B","B"),
                             type = "start")
  
  sequence.update <- function() {
    
    if(strategy$type == "random") {
      if(!is.na(input$rand)){
        strategy$sequence <- sample(c(1,2), size = seq$update, replace = TRUE, prob = c(input$rand/100, 1 - (input$rand/100)))
      }
    } else if (strategy$type == "alternating") {
      if(!is.na(input$alt) & !is.na(input$noise)){
        strategyAlt  <- rep(c(1,2), each = input$alt , times = (seq$update/(input$alt*2)))
        
        strategynoise <- (input$noise/100)
        
        for (i in c(1:length(strategyAlt))) {
          if(strategyAlt[i] == 1) {
            strategyAlt[i] <- sample(c(1,2), size = 1, prob = c(1-strategynoise, strategynoise))
          } else if(strategyAlt[i] == 2){
            strategyAlt[i] <- sample(c(2,1), size = 1, prob = c(1-strategynoise, strategynoise))
          } else {
            strategyAlt[i] <- NA_integer_
          }
        }
        
        strategy$sequence <- strategyAlt
      }
    }
      
      else if (strategy$type == "textoutput"){
          if(!is.na(input$text)){
          strategy$sequence <- strsplit(input$text, split = "")[[1]]
          cat(paste0("text output is", strategy$sequence))
          }
      }
      
      
    else {
      
      # don't change the sequence
      
    }
  }

  # Button and input behavior  ##############################
  
  observeEvent(input$statsBut, {
    toggle("stats", anim = TRUE, animType = "slide", time = 0.5)
    toggle("data", anim = TRUE, animType = "slide", time = 0.5)
  })
  
  
  observeEvent(input$slideBut, {

      seq$type <- "slider"
    })

    observeEvent(input$numBut, {
      seq$type <- "numeric"
    })


    observeEvent(input$textBut, {
      show("text", anim = TRUE, animType = "slide", time = 0.5)
      hide("alt", anim = TRUE, animType = "slide", time = 0.5)
      hide("noise", anim = TRUE, animType = "slide", time = 0.5)
      hide("rand", anim = TRUE, animType = "slide", time = 0.5)
      strategy$type <- "textoutput"
    })
  observeEvent(input$slideBut, {
    show("seqSlide", anim = TRUE, animType = "slide", time = 0.5)
    hide("seqNum", anim = TRUE, animType = "slide", time = 0.5)
  })
  observeEvent(input$numBut, {
    hide("seqSlide", anim = TRUE, animType = "slide", time = 0.5)
    show("seqNum", anim = TRUE, animType = "slide", time = 0.5)
  })
  
  observeEvent(input$stateBut, {
    show("col1", anim = TRUE, animType = "slide", time = 0.5)
    show("col2", anim = TRUE, animType = "slide", time = 0.5)
    hide("colRec", anim = TRUE, animType = "slide", time = 0.5)
    reset("colRec")
    reset("colLoi")
    reset("colNon")
    
  })
  observeEvent(input$recBut, {
    show("colRec", anim = TRUE, animType = "slide", time = 0.5)
    hide("col1", anim = TRUE, animType = "slide", time = 0.5)
    hide("col2", anim = TRUE, animType = "slide", time = 0.5)
    reset("col1")
    reset("colLoi")
    reset("col2")
    reset("colNon")
  })
  observeEvent(input$randomBut, {
    show("rand", anim = TRUE, animType = "slide", time = 0.5)
    hide("alt", anim = TRUE, animType = "slide", time = 0.5)
    hide("noise", anim = TRUE, animType = "slide", time = 0.5)
    hide("text", anim = TRUE, animType = "slide", time = 0.5)
    
    strategy$type <- "random"
  })
  observeEvent(input$altBut, {
    show("alt", anim = TRUE, animType = "slide", time = 0.5)
    show("noise", anim = TRUE, animType = "slide", time = 0.5)
    hide("rand", anim = TRUE, animType = "slide", time = 0.5)
    hide("text", anim = TRUE, animType = "slide", time = 0.5)
    strategy$type <- "alternating"
  })
  
  observeEvent(input$seqNum, {
    sequence.update()
    newSeq()
  })
  observeEvent(input$seqSlide, {
    sequence.update()
    newSeq()
  })
  
  observeEvent(input$rand, {
    sequence.update()
    newSeq()
  })
  
  observeEvent(input$alt, {
    sequence.update()
    newSeq()
  })

  observeEvent(input$noise, {
    sequence.update()
    newSeq()
  })
  observeEvent(input$text, {
    sequence.update()
    newSeq()
  })
  

   
  observeEvent(input$recBut,{
  output$rqa <- renderPlot({

    sequence.df <- tibble(value = c(strategy$sequence),
                          trial = c(1:length(value)))

    trial.tick.labels <- sequence.df$value

    names(trial.tick.labels) <- sequence.df$trial

    tibble(trial.x = c(1:nrow(sequence.df)),
           trial.y = c(1:nrow(sequence.df))) %>%
      complete(trial.x, trial.y) %>%
      left_join(sequence.df,
                by = c("trial.x" = "trial")) %>%
      left_join(sequence.df,
                by = c("trial.y" = "trial")) %>%



      mutate(r = case_when(value.x == value.y & trial.x != trial.y ~ "recurrent",
                           trial.x == trial.y & trial.x != 8 ~ "loi",
                           TRUE ~ "nonrecurrent")
          )  -> recurrence.example.df


    recurrence.example.df %>%
      mutate(trial.x.fac = factor(trial.x),
             trial.y.fac = factor(trial.y)) %>%
      ggplot(aes(x = trial.x.fac, y = trial.y.fac, fill = r)) +
      geom_tile() +
      labs(x = "Trial Sequence", y = "Trial Sequence") +
      theme_classic() +
      theme(legend.position = "bottom",
            panel.grid = element_blank(),
            strip.background = element_blank()) +
      scale_fill_manual(name = NULL,
                        labels = c("loi","recurrent", "nonrecurrent"),
                        values=c(input$colLoi,input$colRec, input$colNon)) +
      scale_x_discrete(labels = trial.tick.labels) +
      scale_y_discrete(labels = trial.tick.labels)
    })

  })


  # Rendering details  ##############################


  observeEvent(input$stateBut, {


   output$rqa <- renderPlot({

    sequence.df <- tibble(value = c(strategy$sequence),
                        trial = c(1:length(value)))

  trial.tick.labels <- sequence.df$value

  names(trial.tick.labels) <- sequence.df$trial

  tibble(trial.x = c(1:nrow(sequence.df)),
         trial.y = c(1:nrow(sequence.df))) %>%
    complete(trial.x, trial.y) %>%
    left_join(sequence.df,
              by = c("trial.x" = "trial")) %>%
    left_join(sequence.df,
              by = c("trial.y" = "trial")) %>%


  mutate(r = case_when(value.x == value.y & trial.x != trial.y ~ "recurrent",
                       trial.x == trial.y  ~ "loi",
                       TRUE ~ "nonrecurrent"),
         event.state = case_when(value.x == value.y & trial.x != trial.y ~ as.character(value.x),
                                 trial.x == trial.y  ~ "loi",
                                 TRUE ~ "nonrecurrent"))  -> recurrence.example.df
  recurrence.example.df %>%
    mutate(trial.x.fac = factor(trial.x),
           trial.y.fac = factor(trial.y)) %>%
    ggplot(aes(x = trial.x.fac, y = trial.y.fac, fill = event.state)) +
    geom_tile() +
    labs(x = "Trial Sequence", y = "Trial Sequence") +
    theme_classic() +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          strip.background = element_blank()) +
    scale_fill_manual(name = NULL,
                      labels = c("1","2", "loi", "nonrecurrent"),
                      values=c(input$col1,input$col2, input$colLoi, input$colNon)) +
    scale_x_discrete(labels = trial.tick.labels) +
    scale_y_discrete(labels = trial.tick.labels)
})


  })
  
  
output$data <- renderPrint({
  sequence_levels <- data.frame(event.state = unique(strategy$sequence),
                                numeric.state = c(1:length(unique(strategy$sequence))))
  
  sequence_transformation <-data.frame(thesequence = strategy$sequence) %>%
    left_join(sequence_levels, by = c("thesequence" = "event.state")) %>%
    rename(transformed_sequence = numeric.state)
  
  crqa_stats <- crqa(ts1 = sequence_transformation$transformed_sequence,
                     ts2 = sequence_transformation$transformed_sequence,
                     delay = 1, embed = 1, rescale = 1, radius = 1e-04, normalize = 0, mindiagline = 2, minvertline = 2, tw = 1) %>%
    list_modify(RP = NULL) %>%
    as.data.frame()
  print(crqa_stats)
})


  
  
}

# shinyApp() call  ##############################

shinyApp(ui = ui, server = server)

