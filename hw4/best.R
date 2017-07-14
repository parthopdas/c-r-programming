best <- function(state, outcome) {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    validStates <- unique(outcomeData$State)
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

    outcomeData[, outcome] <- as.numeric(outcomeData[, outcome])
    stateOutcomeData <- outcomeData[outcomeData$State == state & !is.na(outcomeData[outcome]), ]

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    minVal <- min(stateOutcomeData[, outcome])
    stateOutcomeData[round(stateOutcomeData[outcome] - minVal, 5) == 0.0, ]$Hospital.Name
}
