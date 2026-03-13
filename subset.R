#setwd("C:/Users/Emily/Downloads/STAT 4996")
library(dplyr)
load(37692-0002-Data.rda)
data<-da37692.0002
data<- data %>%
  select(where(~ !all(grepl("Suppressed", .x), na.rm = T)))
data <- data %>%
  select(where(~ !all(grepl("Not Used", as.character(.x)), na.rm = T)))



v_range <- function(start, end) {
  sprintf("V%04d", as.numeric(start):as.numeric(end))
}

vlist <- c("V0001B", v_range("0015","0023")) #race/US Armed forces

vlist <- v_range("0037","0038")) #combat zones in wartime

vlist <- c(vlist,  v_range("0150","0154")) #Offense type Discuss codebook ...??
vlist <- c(vlist,  v_range("0178", "0182")) #Offense type  ...?

vlist <- c(vlist,  v_range("0401", "0404")) #flat sentence/time sentenced 

vlist <- c(vlist, v_range("0450," "0453"))
vlist <- c(vlist, v_range("0458," "0463")) #What does sentence include?
vlist <- c(vlist, "V0778") # carried during offense
vlist <- c(vlist, v_range("0884," "0887")) #firearm variables
vlist <- c(vlist,v_range("0900," "0915")) #some useful variables for prior offenses


vlist
