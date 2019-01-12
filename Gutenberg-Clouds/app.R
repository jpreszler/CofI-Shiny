
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

library(gutenbergr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

cloud_from_book <- function(gutenberg_id){
  #download book using gutenbergr
  book <- gutenberg_download(gutenberg_id)
  #turn line per row into word per row
  Words <- unnest_tokens(book, word, text)
  #remove stop_words using the standard english stop_words
  WordsReduced <- anti_join(Words, stop_words)
  #turn word list into frequency table
  WRCount <- WordsReduced %>% count(word) %>% ungroup()
  #build wordcloud
  wordcloud(WRCount$word, WRCount$n, random.order = FALSE, max.words = 50, colors = brewer.pal(8, "Dark2"))
}

ui <- fluidPage(
   titlePanel("Random Book Word Cloud"),
   sidebarLayout(
      sidebarPanel(
        actionButton("new", "New Book"),
        br(),
        actionButton("reveal", "Show Book Info")
      ),
      
      mainPanel(
         plotOutput("distPlot"),
         tableOutput("BookInfo"))
   )
)

server <- function(input, output) {
  
   v<- reactiveValues(random_id=NULL, metaTab=NULL)
   
   observeEvent(input$new,{
     v$random_id<-sample(gutenberg_metadata$gutenberg_id, 1)
     v$metaTab <- NULL
   })
   observeEvent(input$reveal, {
     v$metaTab<-filter(gutenberg_metadata, gutenberg_id==v$random_id)[,2:3]
   })
   
   output$distPlot <- renderPlot({
     if(!is.null(v$random_id)){
       cloud_from_book(v$random_id)
     }
   })

   output$BookInfo <- renderTable({
     if(!is.null(v$metaTab)){v$metaTab}
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

