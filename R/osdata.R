data("osusers")
data("osmap")

predict.os <- function(a_Year, a_Month) {
      osu <- osusers
      osu$Date <- as.character(osu$Date)
      osu$Date <- paste(osu$Date,"01",sep="-")
      osu$Date <- as.Date(osu$Date)
      fromDate <- osu[nrow(osu),"Date"]
      toDate <- paste(a_Year,a_Month,"01",sep="-")
      toDate <- as.Date(toDate)
}


