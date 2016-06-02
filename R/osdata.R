
data("osusers")
data("osmap")

#' Fitting regression models for each operating system from osusers dataset
#'
#' @return list of the regression models of regression models
#' @details
#' For each variable of the OS popularity is built a regression model using \code{lm}
#' function.
#' @seealso \code{lm} \code{apply}
#' @import stats
#' @export

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

#' Building dataset of monthes for forecasting
#'
#' @param a_Year a character value of the last year in the date list for forecasting in form "YYYY" (for example, "2017", "2018" etc.)
#' @param a_Month a character value of the last month in the date list for forecasting in form "MM" (for example, "01", "07", "10" etc.)
#' @return a data frame containg the dates for futher forecasting in format "YYYY-MM-01"
#' @details
#' Building a dataset containing monthes later the greatest date from the osusers dataset
#' to provide a forecasting
#' @export

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

#' Building predictions for the number of monthes
#'
#' @param a_Year a character value of the last year in the date list for forecasting in form "YYYY" (for example, "2017", "2018" etc.)
#' @param a_Month a character value of the last month in the date list for forecasting in form "MM" (for example, "01", "07", "10" etc.)
#' @return a list of the predictions for given monthes
#' @details
#' This function uses possibilities of some other function to give predictions of operating systems popularity indexis
#' for the month after the latest date from the osusers dataset to the month, which is given within the parameters of the
#' function. The function builds a number iof the regression models, each for the given within the dataset operating
#' system. Then it defined a list of dates for the forecasting and makes predictions.
#' Each prediction in the list contains the predicted value and the upper and lower values of the 95%-confidence interval
#' @seealso \code{fitRegressionModels} \code{getForecastingInterval} \code{lapply}
#' @import stats
#' @export

getPredictions <- function(a_Year, a_Month) {

      fits <- fitRegressionModels()

      predictDates <- getForecastingInterval(a_Year, a_Month)

      predictions <- lapply(fits, function(x) {predict(x, newdata = predictDates, interval = "confidence")})

      return(predictions)
}

#' Extract the predicted values
#'
#' @param a_Year a character value of the last year in the date list for forecasting in form "YYYY" (for example, "2017", "2018" etc.)
#' @param a_Month a character value of the last month in the date list for forecasting in form "MM" (for example, "01", "07", "10" etc.)
#' @return a dataset of the predicted values
#' @details
#' This function uses predicted value, taken from the \code{getPredictions} function for given time period ended with a_Year
#' and a_Month. Then extract it the predicted values from the predictions.
#' @seealso \code{fitRegressionModels} \code{getForecastingInterval} \code{lapply}
#' @import stats
#' @export

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


#' Extract the lower values of the confidence intervals
#'
#' @param a_Year a character value of the last year in the date list for forecasting in form "YYYY" (for example, "2017", "2018" etc.)
#' @param a_Month a character value of the last month in the date list for forecasting in form "MM" (for example, "01", "07", "10" etc.)
#' @return a dataset of the predicted values
#' @details
#' This function uses predicted value, taken from the \code{getPredictions} function for given time period ended with a_Year
#' and a_Month. Then extract it the lower values of the confidence intervals from the predictions.
#' @seealso \code{fitRegressionModels} \code{getForecastingInterval} \code{lapply}
#' @import stats
#' @export

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

#' Extract the upper values of the confidence intervals
#'
#' @param a_Year a character value of the last year in the date list for forecasting in form "YYYY" (for example, "2017", "2018" etc.)
#' @param a_Month a character value of the last month in the date list for forecasting in form "MM" (for example, "01", "07", "10" etc.)
#' @return a dataset of the predicted values
#' @details
#' This function uses predicted value, taken from the \code{getPredictions} function for given time period ended with a_Year
#' and a_Month. Then extract it the upper values of the confidence intervals from the predictions.
#' @seealso \code{fitRegressionModels} \code{getForecastingInterval} \code{lapply}
#' @import stats
#' @export

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


