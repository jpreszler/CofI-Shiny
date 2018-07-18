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
        paste('WW-',input$canvas$name,sep='')
    },
    content = function(file) {
      req(input$canvas)
      inDF <- read.csv(input$canvas$datapath, header=TRUE) %>% 
        select(Student, ID, SIS.User.ID, SIS.Login.ID, Section)
      #clean and manipulate
      
      
      #write inDF to file
      write.csv(inDF,file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

