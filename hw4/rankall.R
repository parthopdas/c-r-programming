rankall <- function(outcome, num = "best") {
    ## Read outcome data
    od <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    validOutcomes = c(11, 17, 23)
    names(validOutcomes) = c("heart attack", "heart failure", "pneumonia")

    outcome <- validOutcomes[outcome]
    if (is.na(outcome)) {
        stop("invalid outcome")
    }

    getNumRow <- if (num == "best") {
        function (data) { data[1, ] }
    } else if (num == "worst") {
        function (data) {
            data[NROW(data), ]
        }
    } else {
        num <- as.integer(num)
        if (is.na(num)) {
            stop("invalud num")
        }
        function (data) { data[num, ] }
    }

    getNumRowForState <- function(data) {
        data[, outcome] <- as.numeric(data[, outcome])
        data <- data[!is.na(data[outcome]), ]
        data <- data[order(data[, outcome], data$Hospital.Name), ]
        getNumRow(data)[, c("Hospital.Name")]
    }

    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    odByState <- split(od, od$State)
    ret <- lapply(odByState, getNumRowForState)
    ret <- as.data.frame(cbind(unlist(ret, use.names = F), names(ret)))
    names(ret) <- c("hospital", "state")
    ret
}
