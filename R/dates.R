days_in_month <- function(date){
  require(lubridate)
    day(floor_date(as.Date(date),'month') + months(1) - days(1))
}


get_month_range <- function(months = 4, daysStartBefore = 1, fromDate = NA, toDate = NA){
  require(lubridate)
  require(dplyr)
  # set month-range for queries and further calculation

  mindate <- floor_date(Sys.Date() - days(daysStartBefore), "month") - months(months)
  lastdate <- floor_date(Sys.Date() - days(daysStartBefore), "month")

  if (!is.na(fromDate) & !is.na(toDate)){
    mindate <- floor_date(as.Date(fromDate), "month")
    lastdate <- floor_date(as.Date(toDate), "month")
  }

  month_range <- tibble(
    YM01 = as.Date(seq(mindate, lastdate,by = 'month')),
    YM = format(YM01, format='%Y-%m'),
    M = month(YM01),
    MYM = paste0('M_',YM)
  )
  month_range
}
