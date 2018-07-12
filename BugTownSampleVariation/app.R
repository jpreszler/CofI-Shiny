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
library(tidyr)

#get data
bugtownDF <- read.csv("data.org", sep="|", header=TRUE, strip.white = TRUE)
#clean
bugtownDF <- filter(bugtownDF, !is.na(apartNumber))

#read, split, and reshape data
bugSampDF <- read.csv("example.csv", header=TRUE) %>% 
    separate(samp, into = paste0("n",1:5), sep=",", remove=TRUE) %>%
    gather(key="n",value="Apt", -c(id,method) ) %>%
  select(-n)
bugSampDF$Apt <- as.numeric(bugSampDF$Apt)

ui <- fluidPage(
   
   titlePanel("BugTown Apartment Sample Variation"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("term", "Semester to view:", 
                    choices = c(example = "Example", f18="Fall 2018"))
      ),
      
      mainPanel(
        tabsetPanel(type="tabs",
                    tabPanel("Apartment Plot", plotOutput("apartPlot")),
                    tabPanel("Histogram", plotOutput("apartHist")),
                    tabPanel("Table", tableOutput("table"))
        )
      )
   )
)

# Define server logic
server <- function(input, output) {
#df <- reactive({
#     bugSampTermDF <- bugSampDF#filter(bugSampDF, term=input$term)
#})
   output$apartPlot <- renderPlot({
     students <- length(unique(bugSampDF$id))
     sampCntDF <- bugSampDF %>% group_by(Apt) %>%
       summarise(method2 = sum(method=="area2"), method3 = sum(method=="area3"))
     left_join(bugtownDF, sampCntDF, by=c("apartNumber" = "Apt")) %>%
     ggplot(aes(x=Xcoord, y=Ycoord)+
       geom_tile(aes(col=ifelse(method2>method3, "red","blue"), alpha=(method2+method3)/students)), fill="white", size=1)+
       geom_text(aes(label=apartNumber))+theme_void()
   })
   output$scatter <- renderPlot({
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

