corr <- function(directory, threshold = 0) {
    res <- numeric()
    for (i in 1:332) {
        c <- corrOfId(directory, i, threshold)
        if (!is.na(c)) {
            res <- c(res, corrOfId(directory, i, threshold))
        }
    }
    res
}

corrOfId <- function(directory, id, threshold) {
    csvPath <- file.path(directory, sprintf("%03d.csv", id))
    data <- read.csv(csvPath)
    completeData <- data[complete.cases(data), ]
    if (nrow(completeData) > threshold) {
        cor(completeData$sulfate, completeData$nitrate)
    } else {
        NA
    }
}
