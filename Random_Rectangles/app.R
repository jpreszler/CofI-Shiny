#Random Rectangles Shiny App
# 
# Created By Jason Preszler, 2018
# 
# License: GPLv3
#
#

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(markdown)
library(googlesheets)

#######
#UPDATE EACH TERM?
#######
term<- "SU18"

#get data
bugtownDF <- read.csv("data.org", sep="|", header=TRUE, strip.white = TRUE)
#clean
bugtownDF <- filter(bugtownDF, !is.na(apartNumber))

gs_auth(token="shiny_token.rds")
subSheet <- gs_key("1Q_DsqB1roB2OJyAhlqaEMOJ6Qa_lYKZa3QUNn2BHUno")

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
            tabPanel("Instrustions",includeMarkdown("instructions.md")),
            tabPanel("BugTown Apartments",h3("BugTown Appartments"),
                plotOutput("apartPlot", height = "700px")),
            tabPanel("area submission", 
                     
                     textInput("method2", label = h3("Apts. For Method 2:"), value = "Enter Apt. numbers sep. by commas..."),
                     hr(),
                     textInput("method3", label = h3("Apts. For Method 3:"), value = "Enter Apt. numbers sep. by commas..."),
                     #tags$head(tags$script(src = "message-handler.js")),
                    hr(),
                     actionButton("submit", "Submit", class = "btn-primary"),
                    hr(),
                    h3("Data Submitted:"),
                    fluidRow(column(4, verbatimTextOutput("value2"))),
                    fluidRow(column(4, verbatimTextOutput("value3")))
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

   #using eventReactive instead of observeEvent to only return
   #submitted data for each press of submit
   vals <- eventReactive(
     input$submit,{
       gs_add_row(subSheet, input = c(input$method2,input$method3,term))
       data.frame(method2=input$method2, method3=input$method3)
     }
   )
   output$value2 <- renderPrint({ vals()$method2})
   output$value3 <- renderPrint({ vals()$method3})

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

