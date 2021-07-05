library(dplyr)
outcome_data <- read.csv(unz("~/Desktop/datasciencecoursera/R_programming/ProgrammingAssignment2/rprog_data_ProgAssignment3-data.zip", "outcome-of-care-measures.csv"), header = TRUE,
                         sep = ",",colClasses = "character") %>% rename(Hospital="Hospital.Name",
                                                                        'heart attack'="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                                                        'heart failure'= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                                                        pneumonia="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" ) 

best <- function(state, outcome){
        #state="TX"
        #outcome="heart failure"
        possible_state <- (unique(outcome_data$State) == state)
        possible_outcome <- (c("heart attack", "heart failure", "pneumonia") == outcome)
        
        if(sum(possible_state) != 1){
                stop(print("invalid state"))
        } else if(sum(possible_outcome) != 1){
                stop(print("invalid outcome"))
        } else {
                
                state_1<-outcome_data %>% filter(State==state) %>% 
                        mutate_all(.,~gsub("Not Available",NA,.))%>% 
                        mutate(Hospital=as.character(Hospital),State=as.character(State),
                               `heart attack`=as.numeric(`heart attack`),
                               `heart failure`=as.numeric(`heart failure`),
                               `pneumonia`=as.numeric(`pneumonia`))
                state_1 <-  state_1[order( state_1[, outcome]), ]
                
                
                if(outcome == 'heart attack') {
                        best_hospital<-state_1 %>% 
                                filter(`heart attack`==min(`heart attack`,na.rm = TRUE)) %>% 
                                select(Hospital) 
                        
                        
                        
                } else if (outcome == 'heart failure') {
                        best_hospital<-state_1 %>% 
                                filter(`heart failure`==min(`heart failure`,na.rm = TRUE)) %>%
                                select(Hospital) 
                        
                } else if (outcome == 'pneumonia'){
                        best_hospital<-state_1 %>% 
                                filter(pneumonia==min(pneumonia,na.rm = TRUE)) %>% 
                                select(Hospital) 
                        
                } else {
                        stop(print("Not valid input for outcome."))
                }
                
                print(best_hospital)
        }
}
