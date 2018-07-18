#This is a function that does the work of the Canvas2WW app for
#use in R directly since cloud deployment of the app is a FERPA 
#problem.

library(dplyr)
library(stringr)

canvas2ww <- function(filename){
  inDF <- read.csv(filename, header=TRUE) %>% 
    select(Student, SIS.User.ID, SIS.Login.ID, Section) %>% 
    filter(!is.na(SIS.User.ID))
  #clean and manipulate
  inDF$First <- str_split(inDF$Student, " ", n=2, simplify = TRUE)[,1]
  inDF$Last <- str_split(inDF$Student, " ", n=2, simplify = TRUE)[,2]
  inDF$blank1 <- " "
  inDF$blank2 <- " "
  inDF$C <- "C"
  inDF$username <- str_split(inDF$SIS.Login.ID, "@", n=2, simplify=TRUE)[,1]
  inDF <- select(inDF,SIS.User.ID, Last, First, C, blank1, Section, blank2, SIS.Login.ID, username )
  
  #write inDF to file
  outFile <- str_replace(basename(filename),'.csv','.lst')
  write.table(inDF,outFile, row.names = FALSE, col.names = FALSE, quote = FALSE, sep=",")
  
}