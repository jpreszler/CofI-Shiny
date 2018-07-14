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
library(googlesheets)

#get data
bugtownDF <- read.csv("data.org", sep="|", header=TRUE, strip.white = TRUE)
#clean
bugtownDF <- filter(bugtownDF, !is.na(apartNumber))

gs_auth(token="shiny_token.rds")
samples_gs <- gs_key("1Q_DsqB1roB2OJyAhlqaEMOJ6Qa_lYKZa3QUNn2BHUno")
#read submitted data
samples <- gs_read(samples_gs) %>% mutate(id = 1:length(area2)) %>% 
  gather(key="method", value="samp", -c(id,term))

#clean submitted data
samples$samp <- str_remove_all(samples$samp, "[:space:]+")
samples <- filter(samples, 
                  str_detect(samples$samp, "[:digit:]+,[:digit:]+,[:digit:]+,[:digit:]+,[:digit:]+"))%>% 
  separate(samp, into = paste0("n",1:5), sep=",", remove=TRUE) %>%
  gather(key="n",value="Apt", -c(id,method,term) ) %>%
  select(-n)
samples$Apt <- as.numeric(samples$Apt)

#read, split, and reshape example data
bugSampDF <- read.csv("example.csv", header=TRUE) %>% 
    separate(samp, into = paste0("n",1:5), sep=",", remove=TRUE) %>%
    gather(key="n",value="Apt", -c(id,method) ) %>%
  select(-n) %>% mutate(term="ex")
bugSampDF$Apt <- as.numeric(bugSampDF$Apt)

bugSampDF <- rbind.data.frame(bugSampDF, samples)

ui <- fluidPage(
   
   titlePanel("BugTown Apartment Sample Variation"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("termSelect", "Semester to view:", 
                    unique(bugSampDF$term)#, f18="Fall 2018")
                    )
      ),
      
      mainPanel(
        tabsetPanel(type="tabs",
                    tabPanel("Apartment Plot", plotOutput("apartPlot")),
                    tabPanel("Histogram", plotOutput("apartHist")),
                    tabPanel("Scatter Plot", plotOutput("scatter")),
                    tabPanel("Table", dataTableOutput("table"))
        )
      )
   )
)

# Define server logic
server <- function(input, output) {
df <- reactive({
     bugSampDF[bugSampDF$term==input$termSelect,]
})
     students <- reactive({length(unique(df()$id))})
     sampCntDF <- reactive({df() %>% group_by(Apt) %>%
       summarise(method2 = sum(method=="area2"), method3 = sum(method=="area3"), 
                 maxMeth=as.factor(ifelse(method2>method3, "Method 2", "Method 3")),
                 Select = 3*(method2+method3)/students())})
     results<-reactive({left_join(bugtownDF, sampCntDF(), by=c("apartNumber" = "Apt"))})
       
   output$apartPlot <- renderPlot({
     ggplot(results(),aes(x=Xcoord, y=Ycoord, fill=maxMeth, alpha=Select))+
       geom_tile(size=1)+
       geom_text(aes(label=apartNumber))+theme_void()
   })
   output$apartHist <- renderPlot({
     df() %>% filter(Apt>0, Apt<101) %>%
     ggplot(aes(x=Apt, fill=method))+
       geom_histogram(position = "dodge", bins=100)
   })
   output$scatter <- renderPlot({
     results() %>% 
       select(apartNumber, area, method2,method3) %>% 
       group_by(apartNumber) %>% summarise(Area = max(area), Method2 = max(method2), Method3 = max(method3), Max.Method = as.factor(ifelse(Method2>Method3, "Method 2", "Method 3"))) %>%
       ggplot(aes(x=Method3, y=Method2, size=Area, col=Max.Method))+
       geom_point(alpha=0.6, position = "dodge")+geom_text(aes(label=apartNumber), col="black", size=5)
   })
   output$table <- renderDataTable({results() %>% 
       select(apartNumber, area, method2,method3) %>% 
       group_by(apartNumber) %>% summarise(Area = max(area), Method2 = max(method2), Method3 = max(method3), Max.Method = as.factor(ifelse(Method2>Method3, "Method 2", "Method 3")))})
}

# Run the application 
shinyApp(ui = ui, server = server)

