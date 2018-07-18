library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(markdown)

ui <- fluidPage(
   
   titlePanel("Canvas to WebWork Roster Converter"),
   
   sidebarLayout(
      sidebarPanel(
        div(id = "description", includeMarkdown("instructions.md"))
        
      ),
      
      mainPanel(
        fileInput("canvas", "Choose Canvas csv roster", 
                  multiple=FALSE, accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
        downloadButton('download_data', 'Download WW')
      )
   )
)

server <- function(input, output) {
  
  output$download_data <- downloadHandler(
    filename = function() {
        req(input$canvas)
        str_replace(basename(input$canvas$name),'.csv','.lst')
    },
    content = function(file) {
      req(input$canvas)
      inDF <- read.csv(input$canvas$datapath, header=TRUE) %>% 
        select(Student, SIS.User.ID, SIS.Login.ID, Section) %>% 
        filter(!is.na(SIS.User.ID))
      #clean and manipulate
      inDF$First <- str_split(inDF$Student, " ", n=2, simplify = TRUE)[,1]
      inDF$Last <- str_split(inDF$Student, " ", n=2, simplify = TRUE)[,2]
      inDF$blank1 <- " "
      inDF$blank2 <- " "
      inDF$C <- "C"
      inDF$username <- str_split(inDF$SIS.Login.ID, "@", n=2, simplify=TRUE)[,1]
      inDF <- select(inDF,SIS.User.ID, Last, First, C, blank1, Section, blank2, SIS.Login.ID, username )
      
      #write inDF to file
      write.table(inDF,file, row.names = FALSE, col.names = FALSE, quote = FALSE, sep=",")
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

