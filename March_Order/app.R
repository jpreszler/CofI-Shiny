library(shiny)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(markdown)

ui <- fluidPage(
   
   titlePanel("the College of Idaho March Order"),
   
   sidebarLayout(
      sidebarPanel(
        div(id = "description", includeMarkdown("instructions.md"))
        
      ),
      
      mainPanel(
        fileInput("Full_Roster", "The full Faculty/Staff list", 
                  multiple=FALSE, accept = c(".xls",".xlsx")),
        fileInput("RSVP_Roster", "The list of Faculty/Staff participating",
                  multiple=FALSE, accept = c(".xls",".xlsx")),
        downloadButton('download_order', 'Download March Order')
      )
   )
)

server <- function(input, output) {
  
  output$download_order <- downloadHandler(
    filename = paste0("March_Order-", Sys.Date(),".pdf"),
    content = function(file) {
      req(input$Full_Roster)
      req(input$RSVP_Roster)
      FullDF <- read_excel(input$Full_Roster$datapath, col_names =TRUE, skip=1) %>% 
        select(LN=`Last Name`, FN=`First Name`,Yrs=`Decimal Years Since Hire Date`) 
      RSVP <- read_excel(input$RSVP_Roster$datapath, col_names = TRUE, skip=1) %>%
        select(LN=`Last Name`, FN=`First Name`)
      
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

