#Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
#outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome) {
        ## Read outcome data
        outcomeData<-loadDataFile()
        
        ## Check that state and outcome are valid
        statesInData<-levels(factor(outcomeData$State))
        if(!is.element(state,statesInData)){
                stop("invalid state")
        }
        validOutcomes<-c("heart attack","heart failure","pneumonia")
        if(!is.element(outcome,validOutcomes)){
                stop("invalid outcome")
        }
        
        #pick only the required state data
        d2<-outcomeData[outcomeData$State==state,]
        
        #convert to numeric and keep only what's required
        #d3<-data.frame(hospitalName=d2$Hospital.Name, ha=as.numeric(d2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), hf=as.numeric(d2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), pn=as.numeric(d2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) )
        
        d3<-data.frame(hospitalName=d2$Hospital.Name, ha=suppressWarnings(as.numeric(d2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), hf=suppressWarnings(as.numeric(d2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), pn=suppressWarnings(as.numeric(d2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) )
        
        #order by the desired requirement
        
        
        if(outcome=="heart attack"){
                d41<-d3[with (d3, order(ha,hospitalName)),]
        }
        else if(outcome=="heart failure"){
                d41<-d3[with (d3, order(hf,hospitalName)),]
        }
        else if(outcome=="pneumonia"){
                d41<-d3[with (d3, order(pn,hospitalName)),]
        }
        result<-as.character(d41$hospitalName[1])
        return(result)
        
}

loadDataFile<-function(){
        outcome<-read.csv("R:/data/GD/OurDocuments/family/Daniel/US_education/Coursera/R Programming/programming assignment 3/outcome-of-care-measures.csv", colClasses = "character")
        return(outcome)
}