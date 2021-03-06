###Author : Matt
###Date   : 2020-09-19
###Purpose: This script will pre-compute the data sets necessary for the shiny app. 

#Load libs
library(tidyverse)
library(survminer)
library(survival)
library(KMsurv)    #survival data set lives here
library(DT)
library(sparkline)


#Load the burn data set
data(burn)

#Clean the data set up a bit: Recode (based on documentation), keep only what I need
burn_1m <-  burn %>%
              mutate(ID        = Obs,
                     Treatment = if_else(Z1 == 1, "Body Cleansing", "Routine Bath"),
                     Gender    = if_else(Z2 == 1, "Female", "Male"),
                     Race      = if_else(Z3 == 1, "White", "Non-white"),
                     Type      = case_when(Z11 == 1 ~ "Chemical",
                                           Z11 == 2 ~ "Scald",
                                           Z11 == 3 ~ "Electric",
                                           Z11 == 4 ~ "Flame"),
                     Head      = if_else(Z5 == 1, "Yes", "No"),
                     Censor    = D1,
                     Time      = T1) %>%
              select(-c(starts_with("Z"), T1:T3, D1:D3))

  
#Create an inline 'swimmer' or 'event' plot using the Time + Censor variables via the sparkline package!
#General Approach: `uncount`` each ID's Time into a vector of one's (so the sparkline will be a constant line of length Time)
#Then using the Censor variable, color the end points of the sparkline based on whether they had the event or not.
#Lastly, create a custom tool tip that displays the actual Time and event status, so that we can drop those variables.
#This approach is a bit `expensive`, so I do it in advance for all ID's and save the result as a data source.

#Custom spark labeller -  https://github.com/htmlwidgets/sparkline/issues/14
spk_tool <- function(labels) {
  htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
      return %s[[field.x]];
}",
      jsonlite::toJSON(labels)
    )
  )
}

#Sparkline creation + data prep pipe
burn_1 <- burn_1m %>%
            select(ID, Time, Censor) %>%
            mutate(label = if_else(Censor == 1, paste(Time, "Days - Event"), paste(Time, "Days - No Event"))) %>%
            uncount(Time) %>%
            mutate(value = 1) %>%
            group_by(ID) %>%
            summarise(sparkline = spk_chr(value, 
                                          type = "line", 
                                          height = 20, 
                                          width = 150,
                                          lineColor = "grey", 
                                          lineWidth = 13, 
                                          spotColor = "blue", 
                                          spotRadius = 5,
                                          chartRangeMinX = 0, 
                                          chartRangeMaxX = 50,
                                          tooltipFormatter = spk_tool(label)
                                          )) %>%
            #Join back (by ID) to get rest of variables
            left_join(burn_1m) %>%
            #The default color is set to blue. Using the censor variable, re-color them
            mutate(sparkline  = case_when(Censor == 1 ~ str_replace(sparkline, "blue", "orange"),
                                          Censor == 0 ~ str_replace(sparkline, "blue", "red"))) %>% 
            arrange(desc(Time))



#Specify Survival Analysis
sfit <- survfit(Surv(Time, Censor) ~ Treatment, data = burn_1)

#Specify times of interest
time_vector <- seq(0,50,10)

#For each ID, construct a series of indicators that record whether they are:
# at risk of the event for a given time
# had an event at the given time
#This can also be potentially expensive, so again I compute it here in advance and save the resulting data.
for(i in seq_along(time_vector)) {
  if(i==1) {
    #By Definition everyone is at risk at the beginning and no events
    burn_1[["risk_t0"]] = 1
    burn_1[["event_t0"]] = 0
  } else {
    burn_1[[paste0("risk_t", time_vector[i])]]  <- ifelse(burn_1$Time >= time_vector[i], 1, 0)
    burn_1[[paste0("event_t", time_vector[i])]] <- ifelse(burn_1$Censor == 1 & (burn_1$Time <= time_vector[i] & burn_1$Time > time_vector[i-1]), 1, 0)
  }
}

#Create the numbers at risk and event summary tables. These ultimately get displayed in the app.
nar_summary_table <-  burn_1 %>%
                        group_by(Treatment) %>%
                        summarise(across(starts_with("risk_t"), sum)) %>%
                        ungroup()

eve_summary_table <-  burn_1 %>%
                        group_by(Treatment) %>%
                        summarise(across(starts_with("event_t"), sum)) %>%
                        ungroup()


#You can cross reference the calculations in the tables like so
summary(sfit, times=time_vector) 
nar_summary_table

#Save data needed for the actual shiny app
rm(list=setdiff(ls(), c("sfit","burn_1","eve_summary_table","nar_summary_table")))

#Save
save.image(here::here('data', 'pre_computed_data.Rdata'))