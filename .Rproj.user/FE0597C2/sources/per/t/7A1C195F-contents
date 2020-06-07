rankall <- function(outcome, num = "best"){
        data<-read.csv("outcome-of-care-measures.csv",colClasses = "character");
        uniq_state <- unique(data$State);
        orderfac<-order(uniq_state);
        uniq_state<-uniq_state[orderfac];
        ranked <- data.frame(hospital = vector("character"),state = vector("character"));
        for(i in seq_len(length(uniq_state))){
                newobj<-c(rankhospital(uniq_state[i],outcome,num),uniq_state[i]);
                ranked<-rbind(ranked,newobj);
        }
        names(ranked)<-c("hospital","state");
        row.names(ranked)<-uniq_state;
        ranked;
}