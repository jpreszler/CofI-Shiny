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
dump <- read.csv("course-time-data.csv") %>% mutate(Subject = str_split(Crs.Name, "-", simplify=TRUE)[,1]) %>% filter(Division != "Other")

#the main data processing function
library(scales)
tgDF <- function(yrStr, bldStr, df){
  if(str_detect(yrStr,"All"))
    dumpF <- df
  else
    dumpF <- filter(df, Year %in% yrStr)
  if(!str_detect(bldStr,"All"))
    dumpF <- filter(dumpF, Bldg %in% bldStr)
  return(
    select(dumpF, Crs.Name, Division, Semester, Days, Start.Time, 
             End.Time) %>%
  filter(Semester=="FA" | Semester=="SP") %>% 
  mutate(Start.Time = as.POSIXct(strptime(Start.Time, 
                                          format = "%I:%M %p", tz="MDT")), 
         End.Time = as.POSIXct(strptime(End.Time, format = "%I:%M %p", tz="MDT"))) %>%
  filter(Start.Time>strptime("7:50 AM", "%I:%M %p", tz="MDT"),
         End.Time<strptime("5:10 PM", "%I:%M %p", tz="MDT"))%>%
  gather(key=SE, value=time, -c(Crs.Name, Division, Semester, Days)) %>%
  group_by(time, SE, Days, Semester, Division) %>%
  summarise(crs.cnt=n())
)
}

etgDF <- function(yrStr, bldStr, df){
  if(str_detect(yrStr,"All"))
    dumpF <- df
  else
    dumpF <- filter(df, Year %in% yrStr)
  if(!str_detect(bldStr,"All"))
    dumpF <- filter(dumpF, Bldg %in% bldStr)
  return(
    select(dumpF, Crs.Name, Division, Semester, Days, Start.Time, 
           End.Time, Enrolled) %>%
      filter(Semester=="FA" | Semester=="SP") %>% 
      mutate(Start.Time = as.POSIXct(strptime(Start.Time, 
                                              format = "%I:%M %p", tz="MDT")), 
             End.Time = as.POSIXct(strptime(End.Time, format = "%I:%M %p", tz="MDT"))) %>%
      filter(Start.Time>strptime("7:50 AM", "%I:%M %p", tz="MDT"),
             End.Time<strptime("5:10 PM", "%I:%M %p", tz="MDT"))%>%
#      select(-c(Enroll.Cap, Enrolled)) %>%
      gather(key=SE, value=time, -c(Crs.Name, Division, Semester, Days, Enrolled)) %>%
      group_by(time, SE, Days, Semester, Division) %>%
      summarise(crs.cnt=n(), Total_Enrollment = sum(Enrolled))
  )
}

DTtgDF <- function(yrStr, bldStr, df){
  if(str_detect(yrStr,"All"))
    dumpF <- df
  else
    dumpF <- filter(df, Year %in% yrStr)
  if(!str_detect(bldStr,"All"))
    dumpF <- filter(dumpF, Bldg %in% bldStr)
  return(
    select(dumpF, Crs.Name, Division, Semester, Days, Start.Time, 
           End.Time, Enrolled) %>%
      filter(Semester=="FA" | Semester=="SP") %>% 
      mutate(Start.Time = as.POSIXct(strptime(Start.Time, 
                                              format = "%I:%M %p", tz="MDT")), 
             End.Time = as.POSIXct(strptime(End.Time, format = "%I:%M %p", tz="MDT"))) %>%
      filter(Start.Time>strptime("7:50 AM", "%I:%M %p", tz="MDT"),
             End.Time<strptime("5:10 PM", "%I:%M %p", tz="MDT"))%>%
      #      select(-c(Enroll.Cap, Enrolled)) %>%
      gather(key=SE, value=time, -c(Crs.Name, Division, Semester, Days, Enrolled)) %>%
      group_by(time, SE, Days, Semester, Division, Crs.Name) %>%
      summarise(crs.cnt=n(), Total_Enrollment = sum(Enrolled))
  )
}

#tg$time <- as.POSIXct(strptime(tg$time, format = "%I:%M %p"))
 #%>% filter(time>strptime("7:50 AM", "%I:%M %p"),time<strptime("5:10 PM", "%I:%M %p"))

etimeGraph <- function(etgDF, dayStr){
  filter(etgDF, Days %in% dayStr) %>%
    ggplot(aes(x=time, y=cumsum(ifelse(SE=="Start.Time", Total_Enrollment, -1*Total_Enrollment))/cumsum(ifelse(SE=="Start.Time", crs.cnt, -1*crs.cnt)), col=Division)) + 
    geom_line(size=2)+#geom_smooth(se=TRUE,n=n, span=spStr)+ 
    facet_wrap(~Semester) +
    scale_x_datetime(breaks = date_breaks("2 hour"),labels=date_format("%I:%M", tz="MDT"))+
    ylab("Average Enrollment")+ggtitle("Average Enrollment by Semester")
}

timeGraph <- function(tgDF, dayStr){
  filter(tgDF, Days %in% dayStr) %>%
    ggplot(aes(x=time, y=cumsum(ifelse(SE=="Start.Time", crs.cnt, -1*crs.cnt)), col=Division)) + 
    geom_line(size=2)+#geom_smooth(se=TRUE,n=n, span=spStr)+ 
    facet_wrap(~Semester) +
    scale_x_datetime(breaks = date_breaks("2 hour"),labels=date_format("%I:%M", tz="MDT"))+
    ylab("Number of Courses")+ggtitle("Number of Classes by Semester")
}

ui <- fluidPage(
   
   # Application title
   titlePanel("Course Day/Time Counter"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("dayStr", label = h3("Meeting Days"), 
                           choices = list("MWF" , "TTH",  "T", "TH", "W","M","MW","MTWF", "MF","F"),
                           selected = "MWF", multiple = TRUE),
        selectInput("Years", label = h3("Years to Include"),
                           choices = as.list(c("All", sort(unique(dump$Year)))),
                           selected = "All", multiple = TRUE),
        selectInput("Buildings", label = h3("Classes Meeting in:"),
                           choices = as.list(c("All", sort(unique(as.character(dump$Bldg))))),
                           selected = "All", multiple = TRUE),
        radioButtons("divDept", label = h3("Separate by:"), 
                     choices = list("Divisions", "Subjects"), selected = "Divisions"),
        selectizeInput("seps", label = h3("Subjects: (up to 4)"), 
                       choices = as.list(sort(unique(as.character(dump$Subject)))), 
                       selected = NULL, options = list(maxItems = 4))
        
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("Plots", plotOutput("numCoursePlot"),
         br()#,
 #        plotOutput("enrollPlot")
 ),
         tabPanel("Data", dataTableOutput("etgDT"))
      )
      )  
   )
)

# Define server logic required
server <- function(input, output) {


   output$numCoursePlot <- renderPlot({
     if(input$divDept == "Subjects"){
       dfset <- filter(dump, Subject %in% input$seps) %>% select(-Division) %>% rename(Division = Subject)
     }
     else
       dfset <- dump
     tg <- tgDF(input$Years, input$Buildings, dfset)
     tg <- tg[order(tg$time),]
     timeGraph(tg, input$dayStr)
   })
   
   output$enrollPlot <- renderPlot({
     if(input$divDept == "Subjects"){
       dfset <- filter(dump, Subject %in% input$seps) %>% select(-Division) %>% rename(Division = Subject)
     }
     else
       dfset <- dump
     etg <- etgDF(input$Years, input$Buildings, dfset)
     etg <- etg[order(etg$time),]
     etimeGraph(etg, input$dayStr)
     
   })
   
   output$etgDT <- renderDataTable({
     if(input$divDept == "Subjects"){
       dfset <- filter(dump, Subject %in% input$seps) %>% select(-Division) %>% rename(Division = Subject)
     }
     else
       dfset <- dump
     dt <- DTtgDF(input$Years, input$Buildings, dfset)
     dt <- dt[order(dt$time),]
     dt %>% filter(Days %in% input$dayStr) %>% 
       mutate(Time = strftime(time, format = "%H:%M %p", tz="MDT", usetz=TRUE)) %>%
       ungroup() %>%
       mutate(Avg_Enrollment = round(Total_Enrollment/crs.cnt,2)) %>%
       select(Time, SE, Semester, Crs.Name, crs.cnt, Avg_Enrollment) %>%
       datatable(rownames = FALSE, filter = "top")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
