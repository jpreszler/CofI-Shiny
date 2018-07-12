#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

#get data
bugtownDF <- read.csv("data.org", sep="|", header=TRUE, strip.white = TRUE)
#clean
bugtownDF <- filter(bugtownDF, !is.na(apartNumber))


ui <- fluidPage(
   
   # Application title
   titlePanel("Random Rectangles"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId="apartmentSample",
                     label="Number of Apartments to sample:",
                       choices=c("All"=-1,"5"=5,"10"=10, "15"=15),selected = "All"),
         h4("Area Information"),
         dataTableOutput("sampArea")
        
      ),
     
       # Show a plot of the generated distribution
      mainPanel(
        h3("BugTown Appartments"),
         plotOutput("apartPlot", height = "700px")#,
        #h4("Area Information"),
#        dataTableOutput("sampArea")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  reactBTsamp<- reactive({
    req(input$apartmentSample)
    #get sample
    apts<-unique(bugtownDF$apartNumber)
    if(input$apartmentSample==-1){
      btSample<- bugtownDF
    }
    else{
      btSample <- bugtownDF[bugtownDF$apartNumber %in% sample(apts, input$apartmentSample),]
    }
    
  })  
   output$apartPlot <- renderPlot({
     #plot sampled appartments
     ggplot(reactBTsamp(), aes(x=Xcoord, y=Ycoord))+geom_tile(col="black", fill="white", size=1)+geom_text(aes(label=apartNumber))+theme_void()
   })
   
   
   output$sampArea <- renderDataTable({
          reactBTsamp() %>% dplyr::select(apartNumber, area) %>% transmute(Apartment=apartNumber, area=area) %>% group_by(Apartment) %>% summarise(Area=mean(area))
   }, options = list(searching=FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)

