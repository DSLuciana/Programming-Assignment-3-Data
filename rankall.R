
## 4 - Ranking hospitals in all states
## x: Dado input

rankall <- function (outcome, num = "best") {
        x <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                            na.strings = "Not Available")
        
        # Verifying whether the result is valid or invalid
        select_o <- if (outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        # Refining to return only the required columns
        rk_data <- x[, c("Hospital.Name","State", select_o)]
        rk_data[, select_o] <- as.numeric(rk_data[, select_o])
        rk_data <- na.omit(rk_data)
        names(rk_data) <- c("hospital", "state", "rate")
        dat <- NULL
        
        # Creating loop over each unique state value
        for(i in unique(rk_data$state)) {
                y <- rk_data[rk_data$state == i, ]
                
                # Classifying the results
                rank <- as.integer() 
                
                rank <- if (num == "best") {
                        1
                } else if (num == "worst") {
                        nrow(y)
                } else if (num < nrow(y)) {
                        num
                } else {
                        y$hospital <- "NA"
                } 
                
                # Ordered by rate and names by state
                ordered <- y[order(y$rate, y$hospital), c(1, 2)][rank, ]
                ordered$state <- rep(i, nrow(ordered))
                
                # Presenting the result in data frame
                dat <- rbind(dat, ordered)
                names(dat) <- c("hospital", "state")
                tie <- dat[order(dat$state), ]
        }
        
        tie
}

# Testing:
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)