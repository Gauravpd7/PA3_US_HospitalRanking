rankhospital <- function(state, outcome, num ="best"){
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
        orderfac<-order(data[,field],data$Hospital.Name);
        data<-data[orderfac,];
        nobs<-nrow(data);
        if(num == "best"){
                return(data$Hospital.Name[1]);
        }else if(num == "worst"){
                return(data$Hospital.Name[nobs]);
        }else if(as.numeric(num)>=1 && as.numeric(num)<=nobs){
                return(data$Hospital.Name[num]);
        }else{
                return(NA);
        }
}