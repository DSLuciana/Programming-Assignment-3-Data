## 1 Plot the 30-day mortality rates for heart attack
## - Reading the data and checking the first few rows.

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome) 
names(outcome)

## Example for a simple histogram containing the following information:
## - 30-day mortality rate from heart attack (data from column 11).
## - The R will issue a Warning message, due to the need of data collation for numerical.

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

## 2 Creating the function to find the best hospital in a state
##  x = outcome specified by user

best <- function (state, outcome) {
        x <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")

        
        ## Checking whether state names are valid or invalid
        
        select_o <- if (outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        valid_state <- which(x$State == state)
        for (i in 1:nrow(x)) {
                if(x$State[i] == state) {
                        valid_state
                        break} 
                if (length(valid_state) == 0) {
                        stop("invalid state")}
        }
        
        
        ## selecting the matching set to state and expected result
        best_data <- x[valid_state, c("Hospital.Name", select_o)] 
        
        ## changing the data type to numerical
        best_data[, select_o] <- as.numeric(best_data[, select_o])
        
        ## returning the best hospitals (with the lowest mortality rate) ordered by name
        ordered <- order(best_data[, select_o], best_data[, "Hospital.Name"])
        as.character(best_data[, "Hospital.Name"][ordered[1]])
}

# Testing:

best("TX", "heart attack")   ## "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")  ## "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")   ## "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")      ## "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")   ## invalid state
best("NY", "hert attack")    ## invalid outcome

best("AL", "heart attack")   ## "CRESTWOOD MEDICAL CENTER"
best("IA", "pneumonia")      ## "MARY GREELEY MEDICAL CENTER"





