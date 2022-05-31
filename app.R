library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(RCurl)

x <- "https://raw.githubusercontent.com/AlexMcLaughlin1/Movie-Browser-Shiny-Application/main/movies.csv"
movies <- readr::read_csv(x)
codebook <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies_codebook.csv")

ui <- fluidPage(
  titlePanel("Movie browser, 1970 - 2014"),
  theme = shinytheme("united"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel("input.tab == 'plot'",
                       h3("Plotting"),
                       selectInput("y", "Y-axis:", choices = c("IMDB Rating" = "imdb_rating",
                                                               "IMDB number of votes" = "imdb_num_votes",
                                                               "Critics Score" = "critics_score", 
                                                               "Audience Score" = "audience_score", 
                                                               "Runtime" = "runtime"), 
                                   selected = "audience_score"),
                       selectInput("x", "X-axis:", choices = c("IMDB Rating" = "imdb_rating",
                                                               "IMDB number of votes" = "imdb_num_votes",
                                                               "Critics Score" = "critics_score", 
                                                               "Audience Score" = "audience_score", 
                                                               "Runtime" = "runtime"), 
                                   selected = "critics_score"),
                       selectInput("colour", "Colour by:", choices = c("Title Type" = "title_type", 
                                                                       "Genre" = "genre", 
                                                                       "MPAA Rating" = "mpaa_rating", 
                                                                       "Critics Rating" = "critics_rating", 
                                                                       "Audience Rating" = "audience_rating"), 
                                   selected = "mpaa_rating"), 
                       hr(),
                       sliderInput("alpha", "Alpha:", min = 0, max = 1, step = 0.01, value = 0.5), 
                       sliderInput("size", "Size:", min = 0, max = 5, step = 1, value = 2), 
                       textInput("title", "Plot title", placeholder = "Enter text to be used as plot title"), 
                       hr()),
      h3("Subsetting and sampling"), 
      checkboxGroupInput("type", "Select movie type(s):", choices = c("Documentary", "Feature Film"), selected = "Feature Film"),
      numericInput("ssize", "Sample size:", min = 1, max = nrow(movies), value = 50),
      conditionalPanel("input.tab == 'data'",
                       hr(),
                       checkboxInput("show", "Show data table", value = TRUE)),
      br(), 
      h5("Built with", img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"), ".")
    ),
    mainPanel(
      tabsetPanel(
        id = "tab",
        tabPanel("Plot",
                 br(),
                 plotOutput("plot"),
                 h6(uiOutput("ui")),
                 value = "plot"),
        tabPanel("Data",
                 br(),
                 conditionalPanel("input.show == true",
                                  dataTableOutput("table")),
                 value = "data"), 
        tabPanel("Codebook",
                 br(),
                 dataTableOutput("table2"))
      )
    )
  )
)



server <- function(input, output) {
  
  data <- reactive({
    a <- movies %>% 
      filter(title_type %in% input$type) %>% 
      sample_n(input$ssize)
    a
  })
  
  
  
  
  output$plot <- renderPlot({
    p <- ggplot(data(), aes_string(input$x, input$y, color = input$colour)) +
      geom_point(size = input$size, alpha = input$alpha) +
      labs(title = input$title)
    p
  })
  
  output$table <- DT::renderDataTable({data()[,1:7]})
  
  output$table2 <- DT::renderDataTable({codebook})
  
  output$ui <- renderUI({
    if (length(input$type) == 0) {
      text <- p("There are no film types selected")
    }
    if (length(input$type) == 1) {
      text <- p("There are", input$ssize, input$type, "movies in this dataset")
    }
    if (length(input$type) > 1) {
      text <- p("There are", input$ssize, "movies in this dataset of different types!")
    }
    text
  })
  
}



shinyApp(ui = ui, server = server)
