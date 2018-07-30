library(shiny)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(markdown)

ui <- fluidPage(
   
   titlePanel(img(src='logo.jpg', align="center")),
   
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
    filename = "March_Order.pdf",
    content = function(file) {
      req(input$Full_Roster)
      req(input$RSVP_Roster)
      
      tempReport <- file.path(tempdir(), "March_Order.Rmd")
      file.copy("March_Order.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      param_list <- list(full = input$Full_Roster$datapath, rsvp = input$RSVP_Roster$datapath)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      out <- rmarkdown::render(tempReport, 
                        params = param_list,
                        envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

