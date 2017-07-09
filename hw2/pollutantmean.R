pollutantmean <- function(directory, pollutant, id=1:332) {
    res <- matrix(NA, nrow=length(id), ncol=2)
    for (i in 1:length(id)) {
        res[i,] <- pollutantmeanOfId(directory, pollutant, id[i])
    }
    sums <- colSums(res)
    sums[1]/sums[2]
}

pollutantmeanOfId <- function(directory, pollutant, id) {
    csvPath <- file.path(directory, sprintf("%03d.csv", id))
    data <- read.csv(csvPath)
    na <- is.na(data[pollutant])
    fpData <- data[pollutant][!na]
    c(sum(fpData), length(fpData))
}
