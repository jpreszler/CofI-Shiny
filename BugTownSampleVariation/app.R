#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

#get data
bugtownDF <- read.csv("data.org", sep="|", header=TRUE, strip.white = TRUE)
#clean
bugtownDF <- filter(bugtownDF, !is.na(apartNumber))

#bugSampDF <- read.csv("example.csv", header=TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("BugTown Apartment Sample Variation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("term", "Semester to view:", 
                    choices = c(example, f18="Fall 2018"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("apartPlot"),
         plotOutput("apartHist")
      )
   )
)

# Define server logic
server <- function(input, output) {
   bugSampTermDF <- bugSampDF#filter(bugSampDF, term=input$term)
   
   output$apartPlot <- renderPlot({
     students <- sum(bugSampTermDF$method3)/5
     left_join(bugtownDF, bugSampTermDF) %>%
     ggplot(aes(x=Xcoord, y=Ycoord, alpha=(method2+method3)/students))+
       geom_tile(col="black", fill="white", size=1)+
       geom_text(aes(label=apartNumber))+theme_void()
   })
   output$scatter <- rednerPlot({
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

