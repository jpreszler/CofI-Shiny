#
# Course_Time Shiny App

# Allows uses to visualize the number of courses offered 
#during different days and semesters. Users can select 
#divisions, departments, or combinations or departments.

# Copyright Jason Preszler, Aug. 2018
# Licensed under GPLv3

library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(stringr)

#load IR data, from Teagle-cleaned-flat-non-blank, merged with current(18-19)
#catalog and basic mutate done from hist3
dump <- read.csv("course-time-data.csv")

#the main data processing function
library(scales)
tgDF <- function(yrStr){
  if(yrStr=="All")
    dumpF <- dump
  else
    dumpF <- filter(dump, Year %in% yrStr)
  return(
    select(dumpF, Crs.Name, Division, Semester, Days, Start.Time, 
             End.Time) %>%
  filter(Semester=="FA" | Semester=="SP") %>% 
  mutate(Start.Time = as.POSIXct(strptime(Start.Time, 
                                          format = "%I:%M %p")), 
         End.Time = as.POSIXct(strptime(End.Time, format = "%I:%M %p"))) %>%
  filter(Start.Time>strptime("7:50 AM", "%I:%M %p"),
         End.Time<strptime("5:10 PM", "%I:%M %p"), 
         Division != "Other")%>%
  gather(key=SE, value=time, -c(Crs.Name, Division, Semester, Days)) %>%
  group_by(time, SE, Days, Semester, Division) %>%
  summarise(crs.cnt=n())
)
}

#tg$time <- as.POSIXct(strptime(tg$time, format = "%I:%M %p"))
 #%>% filter(time>strptime("7:50 AM", "%I:%M %p"),time<strptime("5:10 PM", "%I:%M %p"))

timeGraph <- function(tgDF, dayStr){
  filter(tgDF, Days %in% dayStr) %>%
    ggplot(aes(x=time, y=cumsum(ifelse(SE=="Start.Time", crs.cnt, -1*crs.cnt)), col=Division)) + 
    geom_smooth(se=FALSE)+ facet_wrap(~Semester) +
    scale_x_datetime(breaks = date_breaks("2 hour"),labels=date_format("%I:%M",tz="MST"))+
    ylab("Number of Courses")+ggtitle("Number of Classes by Division and Semester")
}





ui <- fluidPage(
   
   # Application title
   titlePanel("Course Day/Time Counter"),
   
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("dayStr", label = h3("Meeting Days"), 
                           choices = list("MWF" , "TTH",  "T", "TH", "W","M","MW","MTWF", "MF","F"),
                           selected = "MWF"),
        checkboxGroupInput("Years", label = h3("Years to Include"),
                           choices = as.list(c("All", sort(unique(dump$Year)))),
                           selected = "All")
        
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     tg <- tgDF(input$Years)
     tg <- tg[order(tg$time),]
     timeGraph(tg, input$dayStr)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

