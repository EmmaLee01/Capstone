#setwd("C:/Users/Emily/Downloads/STAT 4996")
library(dplyr)
library(stringr)
#load(37692-0002-Data.rda)
data<-da37692.0002
data<- data %>%
  select(where(~ !all(grepl("Suppressed", .x), na.rm = T)))
data <- data %>%
  select(where(~ !all(grepl("Not Used", as.character(.x)), na.rm = T)))



v_range <- function(start, end) {
  sprintf("V%04d", as.numeric(start):as.numeric(end))
}

vlist <- c("V0001B", v_range("1951", "1957"), v_range("0022","0023")) #race/US Armed forces

vlist <- c(vlist, v_range("0037","0038")) #combat zones in wartime

vlist <- c(vlist,  v_range("0150","0154")) #Offense type Discuss code book ...??
vlist <- c(vlist,  v_range("0178", "0181")) #Offense type  ...?

vlist <- c(vlist,  v_range("0401", "0404")) #flat sentence/time sentenced 

vlist <- c(vlist, v_range("0450", "0453"))
vlist <- c(vlist, v_range("0458", "0463")) #What does sentence include?
vlist <- c(vlist, "V0778") # carried during offense
vlist <- c(vlist, v_range("0884", "0887")) #firearm variables
vlist <- c(vlist,v_range("0900", "0915")) #some useful variables for prior offenses
vlist <- c(vlist,v_range("0935", "0937"))# education
vlist <- c(vlist,v_range("0940", "0943")) #education/learning disability
vlist <- c(vlist,v_range("0952", "0961")) #living situation
vlist <- c(vlist, "V0982") #has children
vlist <- c(vlist,v_range("1040", "1052")) # communications information from various people
vlist <- c(vlist, "V1079") #had a job


vlist <- c(vlist,v_range("1179", "1192")) #mental health diagnoses / screening questions we can possibly use for 
vlist <- c(vlist,v_range("1199", "1200")) #hospitalization
vlist <- c(vlist,v_range("1202", "1205")) #treatment


vlist <- c(vlist, "V1265", "V1268") #proposed variables for alcoholism diagnoses
##in our documentation we should write that we do not use weed as one of these drugs 
vlist <- c(vlist,v_range("1316", "1326")) #proposed variables for drugs
vlist <- c(vlist,v_range("1328", "1338"))

vlist <- c(vlist, "V1374") #ever received treatment
## The thing about this one is that there are specifics, we should go over if we want to do specifics, goes up to 1394
#talk about the percentages of negative numbers


vlist <- c(vlist,v_range("1397", "1420")) #rule violation
sublist <- data  %>%
  select(all_of(vlist))














## Begin data cleaning

sublist <- sublist %>% #function returns only yes/no codes as 1/2, 
  mutate(across(2:8, ~ as.numeric(gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(.x))))) 
sublist <- sublist %>% #additionally will have other codes such as negatives but can be classified as NA for our purposes
  mutate(across(10:12, ~ as.numeric(gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(.x)))))

sublist$V0022 <- gsub("^.*=[:space:]*", "", as.character(sublist$V0022))


sublist <- sublist %>%
  mutate(across(13:22, ~ {
    str_remove(as.character(.x), "^.*=[:space:]*")
  }))
## for time sentenced to prison, consider flattening these, as they are all separate as far as I can tell
sublist <- sublist %>% 
  mutate(across(26:50, ~ as.numeric(gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(.x))))) 
#0940 is 1 = GED/HS Diploma
sublist <- sublist %>% 
  mutate(across(57:81, ~ as.numeric(gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(.x))))) 
sublist <- sublist %>%
  mutate(across(82:87, ~ {
    str_remove(as.character(.x), "^.*=[:space:]*")
  }))
frequency_levels <- c(
  "Less Than Once a Month",
  "Monthly", 
  "Weekly", 
  "Daily"
)
sublist <- sublist %>% #adding ordering for this variable
  mutate(across(82:87, ~ factor(.x, levels = frequency_levels, ordered = TRUE)))





sublist$V1079 <- gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(sublist$V1079)) ###


sublist <- sublist %>% #adding ordering for this variable
  mutate(across(89:94, ~ {
    str_remove(as.character(.x), "^.*=[:space:]*")
  }))
mental_health_levels <- c(
  "None of the Time", 
  "A Little of the Time", 
  "Some of the Time", 
  "Most of the Time", 
  "All of the Time"
)
sublist <- sublist %>%
  mutate(across(89:94, ~ factor(.x, levels = mental_health_levels, ordered = TRUE)))


sublist <- sublist %>% 
  mutate(across(95:133, ~ as.numeric(gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(.x))))) 


sublist <- sublist %>%
  mutate(across(where(~ is.character(.x) | is.factor(.x)), ~ {
      str_trim(.x)
    }
  ))

## This is if we need to use code to clean new variables, below is a line of code as an example
##added <- c("V1185", "V1186", "V1187", "V1200")

##sublist <- sublist %>%
##  mutate(across(all_of(target_vars), ~ as.numeric(gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(.x)))))




##Converting Marital Status back to numerical
sublist[9] <- data$V0022
sublist[9]<-gsub(".*\\((-?\\d+)\\).*","\\1", as.character(sublist$V0022))

##temp fix for conversion of All time/none time 
sublist <- sublist %>% 
  mutate(across(57:81, ~ as.numeric(gsub(".*\\((-?\\d+)\\).*", "\\1", as.character(.x))))) 
#sublist[89:]


#Conversion for NA, 0

sublist$V1398[is.na(sublist$V1398)] <- 0
offenses<-data[is.na(data$V0062),]

sublist <- sublist %>%
  mutate(V0062 = data$V0062)
sublist[158]<-gsub(".*\\((-?\\d+)\\).*","\\1", as.character(sublist$V0062))


sublist <- sublist %>%
  mutate(V1398 = ifelse(
    V1397 == 0 & is.na(V1398),
    0,
    V1398
  ))

sublist <- sublist %>%
  filter(!is.na(V1398))







sublist <- sublist %>%
  mutate(V1398 = ifelse(
    V1397 == 0 & is.na(V1398),
    0,
    V1398
  ))

  





##Begin model writing
firstmod<-glm(V1398~V1951+V1952+V1953+V1954+V1955+V1956+V1957+V0037+V0038+V0023+V0062, 
              data = sublist,
              family = poisson(link="log")
              )





