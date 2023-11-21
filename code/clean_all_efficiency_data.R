#clean the environment before commencing
rm(list = ls())

library(tidyverse)
library(dplyr)

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

#import data
efficiency_raw = read_csv("../uncleaned_data/Efficiency_complete.csv")

################################################################################################################################################

### FUNCTIONS ###

#create a function for bout number, displacement count, and tool switch count data creation
create_individual_data = function(data, individual){
  
  #subset the data to the individual
  ind_data = subset(data, data$Subject == individual)
  
  #add empty columns
  ind_data$bout_number = 0
  ind_data$displacement_count = NA
  ind_data$tool_switch_count = NA
  ind_data$bout_duration = NA
  ind_data$strike_count = NA
  ind_data$bout_outcome = NA
  ind_data$learner = NA
  
  #bout count
  j = 0
  
  #go through the rows of the dataset
  for(i in 1:nrow(ind_data)){
    
    #if the row says 'Start Bout', the bout counter is updated by 1
    if(ind_data$Behaviour[i] == 'Start Bout'){
      print(i)
      
      #get the start time
      cur_start = ind_data$Start[i]

      #count bout
      j = j + 1
      
      #count variables to be created
      bout_state = "started"
      bout_obvs_count = 1
      displacement_count = 0
      tool_switch_count = 0
      
      while(bout_state == "started"){
        print(bout_state)
        
        #add a count for displacement, move to next row
        if(ind_data$Behaviour[i + bout_obvs_count] == "Displacement"){
          displacement_count = displacement_count + 1
          bout_obvs_count = bout_obvs_count + 1
          if((bout_obvs_count + i) > nrow(ind_data)){
            break
          }       
        }
        
        if(ind_data$Behaviour[i + bout_obvs_count] == "Tool Switch"){
          tool_switch_count = tool_switch_count + 1
          bout_obvs_count = bout_obvs_count + 1
          if((bout_obvs_count + i) > nrow(ind_data)){
            break
          }
        }
        
        #ignore and go to the next row
        if(ind_data$Behaviour[i + bout_obvs_count] == "End Bout"){
          
          #get the end time (and duration time)
          cur_stop = ind_data$Stop[i + bout_obvs_count]
          cur_duration_time = cur_stop - cur_start
          print(cur_duration_time)
          
          #get the strike count
          cur_strike_count = ind_data$Strikes[i + bout_obvs_count]
          
          #get the bout outcome
          cur_outcome = ind_data$Outcome[i + bout_obvs_count]
          
          #get the learner presence
          cur_learner = ind_data$Learner[i + bout_obvs_count]
          
          #add to bout count
          bout_obvs_count = bout_obvs_count + 1
          if((bout_obvs_count + i) > nrow(ind_data)){
            break
          }
        }
        
        #end the while loop
        if(ind_data$Behaviour[i + bout_obvs_count] == "Start Bout"){
          bout_state = "ended"
        }
        
        #ignore and go to the next row
        if(ind_data$Behaviour[i + bout_obvs_count] == "Strike"){
          bout_obvs_count = bout_obvs_count + 1
          if((bout_obvs_count + i) > nrow(ind_data)){
            break
          }
        }
      }
    }
    
    #add bout number, displacement count, and tool switch count
    ind_data$bout_number[i] = j
    ind_data$displacement_count[i] = displacement_count
    ind_data$tool_switch_count[i] = tool_switch_count
    ind_data$bout_duration[i] = cur_duration_time
    ind_data$strike_count[i] = cur_strike_count
    ind_data$bout_outcome[i] = cur_outcome
    ind_data$learner[i] = cur_learner
    
  }
  
  #return the data frame
  return(ind_data)
}

################################################################################################################################################

### CREATE DATA ###
  
#get a list of individuals
inds = unique(efficiency_raw$Subject)

#list of data frames and count
datalist = list()
counter = 1

#create bout number, displacement count, tool switch count, bout duration, strike count, and bout outcome data for each individual
for(i in inds){
  
  ind_dat = create_individual_data(efficiency_raw, i)
  datalist[[i]] = ind_dat
  
  counter = counter + 1
}

#create total data frame
total_data = do.call(rbind, datalist)

#subset to only the first row of the bout
total_data = subset(total_data, total_data$Behaviour == "Start Bout")

#remove unnecessary or repeat columns 
total_data = select(total_data, -c(Behaviour, Start, Stop, Duration, Strikes, Outcome, Learner, MaternalModel, Tolerance, Intolerance)) 

#rename columns
colnames(total_data) = c("uid", "date", "subject", "sex", "age", "nut_species", "bout_number", "displacement_count",
                         "tool_switch_count", "bout_duration", "strike_count", "bout_outcome", "learner")

#drop observations from 2018
total_data = subset(total_data, total_data$date != 2018)

#save the dataframe
write.csv(total_data, "../cleaned_data/cleaned_efficiency_data.csv", row.names = FALSE)
