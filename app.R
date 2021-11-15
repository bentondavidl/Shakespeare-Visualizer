# Deployed at https://bentondavidl.shinyapps.io/word-frequency/

library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
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

# task6: add in shinythemes function

ui <- fluidPage(theme=shinytheme("cerulean"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "plays", label = "Select a Play", choices = books
      ),
      checkboxInput(
        inputId = "stopWords", label = "Keep stop words", value = T
      ),
      actionButton(inputId = "update", label = "Update View"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput(inputId="maxwords",label="Max # of Words", min=10, max=200, value=100, step=10),
      sliderInput(inputId="largeSize",label="Max Word Size", min=1, max=8, value=4),
      sliderInput(inputId="smallSize",label="Min Word Size", min=0.1, max=4, value=0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput(inputId="minCount",label="Min Occurances to Include", min=10, max=100, value=25),
      sliderInput(inputId="fontSize",label="Font Size", min=8, max=30, value=14)
      
    ),
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("cloud", height="600px")),
        tabPanel("Word Counts", plotOutput("freq", height="600px"))
      )
    )
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
  )
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$update, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$plays, input$stopWords)
    })
  })
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8, "Dark2")
    
    v %>%
      with(
        wordcloud(
          word, 
          n, 
          scale=c(input$largeSize,input$smallSize),
          random.order = F,
          max.words = input$maxwords,
          colors=pal
        )
      )
  })
  output$freq <- renderPlot({
    v <- freq()
    
    v %>%
      filter(n > input$minCount) %>%
      ggplot(aes(x=n,y=reorder(word, n))) +
      geom_bar(stat="identity") +
      theme(text=
              element_text(size=input$fontSize), 
              axis.title.x = element_blank(), 
              axis.title.y=element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)
