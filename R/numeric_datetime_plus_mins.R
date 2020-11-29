# proposed numeric date time addition function
# works 3 times faster than the equivalent lubridate operation:
#
# lubridate::ymd_hms(current_date) + lubridate::minutes(times) 
#
numeric_datetime_plus_mins = function(date_time_val, times_min, tz_val = NA,
                                      time_origin = "1970-01-01 00:00.00"){
  # processing time zone info
  isEmptyTz = !nzchar(tz_val)
  isInvalidTz = !is.character(tz_val)
  tz_val = attributes(date_time_val)$tzone
  if(isEmptyTz | isInvalidTz) tz_val = "UTC"
  
  # set numeric date origin with appropriate tz
  time_origin = paste(time_origin, tz_val)
  numeric_date = as.numeric(date_time_val)
  # add 
  as.POSIXct(numeric_date + round(times_min * 60), 
             origin = time_origin, tz = tz_val)
}