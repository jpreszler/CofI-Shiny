library(shiny)
library(dplyr)

titTR <- read.csv("titanic-train.csv", header=TRUE)
titTST<- read.csv("titanic-test-real.csv", header=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Test Titanic Logistic Regression Prediction"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(textInput("mdl", label= "Model Formula", value=""),
                   numericInput("k", label="Survival Threshold", value="0.5"),
                   actionButton("Enter", "Test")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("confMatrix"),
         textOutput("acc")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$Enter, {
    in.glm <- glm(input$mdl, data=titTR, family=binomial())
    titTST$pred <- predict(in.glm, titTST[,-2], type="response")
    titTST$pSurvived <- ifelse(titTST$pred>input$k, 1, 0)
    output$confMatrix <- renderTable({xtabs(~Survived+pSurvived, data=titTST)})
    output$acc <- renderPrint({sum(titTST$Survived==titTST$pSurvived)/nrow(titTST)})
    
  })
     
#   output$confMatrix <- renderTable({
#     return(data.frame(TN = sum(titTST$Survived==0 && titTST$pSurvived==0), 
#                TP = sum(titTST$Survived==1 & titTST$pSurvived==1), 
#                FP = sum(titTST$Survived==0 & titTST$pSurvived==1), 
#                FN = sum(titTST$Survived==1 & titTST$pSurvived==0),
#                Acc = sum(titTST$Survived==titTST$pSurvived)/nrow(titTST))
#   )})
}

# Run the application 
shinyApp(ui = ui, server = server)

