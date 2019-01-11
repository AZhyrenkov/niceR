days_in_month <- function(date){
  require(lubridate)
    day(floor_date(as.Date(date),'month') + months(1) - days(1))
}


get_month_range <- function(months_n = 5, daysStartBefore = 1){
  require(lubridate)
  require(dplyr)
  # set month-range for queries and further calculation

  mindate <- floor_date(Sys.Date() - days(daysStartBefore), "month") - months(months_n-1)
  lastdate <- floor_date(Sys.Date() - days(daysStartBefore), "month")

    month_range <- tibble(
    YM01 = as.Date(seq(mindate, lastdate,by = 'month')),
    YM = format(YM01, format='%Y-%m'),
    M = month(YM01),
    MYM = paste0('M_',YM)
  )
  month_range
}
