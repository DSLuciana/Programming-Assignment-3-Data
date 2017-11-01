# 3 - We create a fucntion that returns the hospital with specified ranking


rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        colNum <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)[outcome]
        if (is.na(colNum)) {
                stop('invalid outcome')
        }
        x[, colNum] <- suppressWarnings(as.numeric(x[, colNum]))
        
        stateIdx <- x$State == state
        if (sum(stateIdx) == 0) {
                stop('invalid state')
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        nameCol <- 2
        unordered <- x[stateIdx & !is.na(x[colNum]),]
        orderedNames <- unordered[order(unordered[colNum],unordered[nameCol]),][,nameCol]
        
        position <- c('best'=1, 'worst'= length(orderedNames))[num]
        orderedNames[if(!is.na(position)){position} else {num}]
}

# Testing:
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)