best <- function(state,outcome){
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character");
        
        if(state %in% data$State == FALSE){
                stop("invalid state");
        }
        
        checkstate <- data$State == state;
        data <- data[checkstate,];
        
        if(outcome == "heart attack"){
                good <- data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available" ;
                field <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack";
        }else if(outcome == "heart failure"){
                good <- data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available";
                field <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure";
        }else if(outcome == "pneumonia"){
                good <- data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available";
                field<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia";
        }else{
                stop("invalid outcome");
        }
        
        data <- data[good,];
        data[,field] <- as.numeric(data[,field]);
        min_val <- min(data[,field]);
        hospital_name <- data$Hospital.Name[data[field] == min_val];
        print(hospital_name);
        
}