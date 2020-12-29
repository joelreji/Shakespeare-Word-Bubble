library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(shinythemes)

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Shakespeare's Plays Word Frequencies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("bk", "Choose a book", choices = books),
      checkboxInput("stopwords", "Stop words", value=TRUE),
      actionButton("run", "Run App"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("maxwords", "Max # of words:", min = 10,  max = 200, value = 100, step = 10),
      sliderInput("largestwords", "Size of largest words:", min = 1, max = 8, value = 4),
      sliderInput("smallestwords", "Size of smallest words:", min = 0.1, max = 4, value = 0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput("minwordcount", "Minimum words for Counts Chart:", min = 10, max = 100, value = 25),
      sliderInput("fontsize", "Word size for Counts Chart:", min = 8, max = 30, value = 14)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Word Cloud", plotOutput("cloud", height = "600px")),
                  tabPanel("Word Counts", plotOutput("freq", height = "600px"))
      )
    )
  )
)

server <- function(input, output) {
  
  freq <- eventReactive(input$run, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$bk, input$stopwords) 
    })
  })
  
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$largestwords, input$smallestwords),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })

  output$freq <- renderPlot({
    v <- freq()
    vf <- v%>%
      filter(n>input$minwordcount)
    
    ggplot(vf, aes(reorder(word, n),n)) + 
    geom_col() + 
    coord_flip()+
      theme(text = element_text(size=input$fontsize),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
}

shinyApp(ui = ui, server = server)
