data("osusers")
data("osmap")

predict.os <- function(a_Year, a_Month) {
      osu <- osusers
      dates <- as.character(osu$Date)
      dates <- paste(dates,"01",sep="-")
      dates <- as.Date(dates)

      ## become a list of regression models
      fits <- apply(osu[,-1], 2, function(x) {lm(x~dates)})

      fromDate <- max(dates)
      toDate   <- paste(a_Year, a_Month, "01", sep = "-")
      toDate   <- as.Date(toDate)
      xtoDate  <- as.POSIXct(toDate)

      xDate <- as.POSIXlt(fromDate)
      xDate$mon <- xDate$mon + 1
      predictDates <- data.frame(dates = as.Date(xDate))

      while (xDate < xtoDate) {
            xDate$mon <- xDate$mon + 1
            tmpDates <- data.frame(dates = as.Date(xDate))
            predictDates <- rbind(predictDates, tmpDates)
      }

      predictions <- lapply(fits, function(x) {predict(x, newdata = p)})

      resDataSet <- predictDates
      names(resDataSet) <- c("Date")
      resDataSet$Date <- as.character(resDataSet$Date)
      resDataSet$Date <- gsub("-01$","",resDataSet$Date)

      for (osNameIdx in 2:length(names(osusers))) {
            values <- predictions[[names(osusers)[osNameIdx]]]
            resDataSet <- cbind(resDataSet, values)
            # names(resDataSet) <- c(names(resDataSet), names(osusers)[osNameIdx])
      }

      names(resDataSet)[-1] <- names(osusers)[-1]

      return(resDataSet)

}


