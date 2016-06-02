data("osusers")
data("osmap")


fitRegressionModels <- function()
{
      osu <- osusers
      dates <- as.character(osu$Date)
      dates <- paste(dates,"01",sep="-")
      dates <- as.Date(dates)

      ## become a list of regression models
      fits <- apply(osu[,-1], 2, function(x) {lm(x~dates)})

      return(fits)
}

getForecastingInterval <- function(a_Year, a_Month)
{
      osu <- osusers
      dates <- as.character(osu$Date)
      dates <- paste(dates,"01",sep="-")
      dates <- as.Date(dates)

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

      return(predictDates)
}

getPredictions <- function(a_Year, a_Month) {

      fits <- fitRegressionModels()

      predictDates <- getForecastingInterval(a_Year, a_Month)

      predictions <- lapply(fits, function(x) {predict(x, newdata = predictDates, interval = "confidence")})

      return(predictions)
}

getPredValues <- function(a_Year, a_Month) {

      predictDates <- getForecastingInterval(a_Year, a_Month)
      predictions <- getPredictions(a_Year, a_Month)

      resDataSet <- predictDates
      names(resDataSet) <- c("Date")
      resDataSet$Date <- as.character(resDataSet$Date)
      resDataSet$Date <- gsub("-01$","",resDataSet$Date)

      for (osNameIdx in 2:length(names(osusers))) {
            values <- predictions[[names(osusers)[osNameIdx]]][,1]
            resDataSet <- cbind(resDataSet, values)
      }

      names(resDataSet)[-1] <- names(osusers)[-1]

      return(resDataSet)
}


getPredLwr <- function(a_Year, a_Month) {

      predictDates <- getForecastingInterval(a_Year, a_Month)
      predictions <- getPredictions(a_Year, a_Month)

      resDataSet <- predictDates
      names(resDataSet) <- c("Date")
      resDataSet$Date <- as.character(resDataSet$Date)
      resDataSet$Date <- gsub("-01$","",resDataSet$Date)

      for (osNameIdx in 2:length(names(osusers))) {
            values <- predictions[[names(osusers)[osNameIdx]]][,2]
            resDataSet <- cbind(resDataSet, values)
      }

      names(resDataSet)[-1] <- names(osusers)[-1]

      return(resDataSet)
}

getPredUpr <- function(a_Year, a_Month) {

      predictDates <- getForecastingInterval(a_Year, a_Month)
      predictions <- getPredictions(a_Year, a_Month)

      resDataSet <- predictDates
      names(resDataSet) <- c("Date")
      resDataSet$Date <- as.character(resDataSet$Date)
      resDataSet$Date <- gsub("-01$","",resDataSet$Date)

      for (osNameIdx in 2:length(names(osusers))) {
            values <- predictions[[names(osusers)[osNameIdx]]][,3]
            resDataSet <- cbind(resDataSet, values)
      }

      names(resDataSet)[-1] <- names(osusers)[-1]

      return(resDataSet)
}


