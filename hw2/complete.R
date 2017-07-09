complete <- function(directory, id=1:332) {
    res <- data.frame()
    for (i in 1:length(id)) {
        res <- rbind(res, completeOfId(directory, id[i]))
    }
    res
}

completeOfId <- function(directory, id) {
    csvPath <- file.path(directory, sprintf("%03d.csv", id))
    data <- read.csv(csvPath)
    completeData <- data[!is.na(data$sulfate & data$nitrate), ]
    list(id = id, nobs = nrow(completeData))
}
