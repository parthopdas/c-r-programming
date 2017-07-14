rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    od <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    validStates <- unique(od$State)
    validOutcomes = c(11, 17, 23)
    names(validOutcomes) = c("heart attack", "heart failure", "pneumonia")

    ## Check that state and outcome are valid
    if (!(state %in% validStates)) {
        stop("invalid state")
    }

    outcome <- validOutcomes[outcome]
    if (is.na(outcome)) {
        stop("invalid outcome")
    }

    getNumRow <- if (num == "best") {
        function (data) { data[1, ] }
    } else if (num == "worst") {
        function (data) { data[NROW(data), ] }
    } else {
        num <- as.integer(num)
        if (is.na(num)) {
            stop("invalud num")
        }
        function (data) { data[num, ] }
    }

    od[, outcome] <- as.numeric(od[, outcome])
    sod <- od[od$State == state & !is.na(od[outcome]), ]
    osod <- sod[order(sod[ ,outcome], sod$Hospital.Name), ]

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    getNumRow(osod)$Hospital.Name
}
