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
library(stringr)

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
        tabsetPanel(type="tabs",
            tabPanel("plot",h3("BugTown Appartments"),
                plotOutput("apartPlot", height = "700px")),
            tabPanel("area submission", 
                     textInput("area2", label = h3("Apts. For Method 2:"), value = "Enter Apt. numbers sep. by commas..."),
                     
                     hr(),
                     fluidRow(column(4, verbatimTextOutput("value2"))),
                     hr(),
                     textInput("area3", label = h3("Apts. For Method 3:"), value = "Enter Apt. numbers sep. by commas..."),
                     
                     hr(),
                     fluidRow(column(4, verbatimTextOutput("value3"))),
                     actionButton("submit", "Submit", class = "btn-primary")
            )
            )         #,
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
   
   output$value2 <- renderPrint({ input$area2 })
   output$value3 <- renderPrint({ input$area3})
   
   output$sampArea <- renderDataTable({
          reactBTsamp() %>% dplyr::select(apartNumber, area) %>% transmute(Apartment=apartNumber, area=area) %>% group_by(Apartment) %>% summarise(Area=mean(area))
   }, options = list(searching=FALSE))

#to only allow submit with valid input, needs work
#   observe({
     # check if all mandatory fields have a value
#     mandatoryFilled <-
#       vapply(c(input$area2,input$area3),
#              function(x) {
#                str_detect(x, "[:digit:]+,[:digit:]+,[:digit:]+,[:digit:]+,[:digit:]+")
#                },
#              logical(1))
#     mandatoryFilled <- all(mandatoryFilled)
     
     # enable/disable the submit button
#     shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
#   })
}

# Run the application 
shinyApp(ui = ui, server = server)

