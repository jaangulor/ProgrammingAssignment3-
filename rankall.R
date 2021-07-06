rankall <- function(outcome, num = 'best') {
        library(dplyr)
        #outcome="pneumonia"
        #num="best"
        possible_outcome <- (c("heart attack", "heart failure", "pneumonia") == outcome)
        outcome_data <- read.csv(unz("~/Desktop/datasciencecoursera/R_programming/ProgrammingAssignment2/rprog_data_ProgAssignment3-data.zip", "outcome-of-care-measures.csv"), header = TRUE,
                                 sep = ",",colClasses = "character") %>% rename(Hospital="Hospital.Name",
                                                                                'heart attack'="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                                                                'heart failure'= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                                                                pneumonia="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" ) %>%
                suppressWarnings()
        
        if(sum(possible_outcome) != 1){
                stop(print("invalid outcome"))
        }
        
        state_1<-outcome_data %>% 
                mutate_all(.,~gsub("Not Available",NA,.))%>% 
                mutate(Hospital=as.character(Hospital),State=as.character(State),
                       `heart attack`=as.numeric(`heart attack`),
                       `heart failure`=as.numeric(`heart failure`),
                       `pneumonia`=as.numeric(`pneumonia`)) 
        state_1<-state_1[order(state_1[, outcome], state_1[, "Hospital"]), ]
        state_1 <-state_1[!is.na(state_1[, outcome]), ]
        Rank<-state_1 %>% select(Hospital,State,outcome) %>% 
                arrange(State,outcome) %>% 
                group_by(State) %>% 
                mutate(rnum=seq(1:n())) %>% 
                suppressWarnings()
        
        if(num == "best") {
                return(Rank %>% filter(rnum==min(rnum,na.rm = TRUE)) %>% 
                               rename(state=State,hospital=Hospital) %>% 
                               select(1,2))
        } else if (num == "worst") {
                return(Rank %>% filter(rnum==max(rnum,na.rm = TRUE)) %>% 
                               rename(state=State,hospital=Hospital) %>% 
                               select(1,2))
                }else{ 
                         
        return(Rank %>% filter(rnum==num) %>% 
                       rename(state=State,hospital=Hospital) %>% 
                       select(1,3))
        }
       
}

