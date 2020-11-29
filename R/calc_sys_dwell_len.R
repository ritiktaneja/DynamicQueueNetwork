
#' @export
calc_sys_dwell_len <- function(Passenger_df, airport_day = ymd_hms("2017-08-07 00:00:00"), metric_func=max, arrivals, departures, group_col = NA, time_step = "30 mins"){

  # define groups:
  # calc_groups: columns to group by before
  # simpler to define function pipeline groupings here!
  if( is.na(group_col) ){
    group_col_names = c(arrivals, departures)
    calc_groups = list()
    summary_groups = rlang::syms("epoch")
  } else {
    group_col_names = c(group_col, arrivals, departures)
    calc_groups = rlang::syms(group_col)
    summary_groups = rlang::syms(c("epoch", group_col))
  }

  dwell_len <- Passenger_df %>%

    # probably faster than dplyr::select()
    .[ group_col_names ] %>%

    # using group_col to construct qroups for queue_lengths() calc
    # preserves data groups column if supplied
    group_by( !!!calc_groups ) %>%

    do(queue_lengths(arrivals = .[[arrivals]], departures = .[[departures]])) %>%
    mutate(value = queuelength) %>%
    mutate(epoch = round(times)) %>%

    # using "epoch" and/or "group_col" to construct qroups for summarise() calc
    group_by( !!!summary_groups ) %>%

    # do.call() was too slow and mean() is the metric going forward
    summarise(value = mean(x=value)) %>%
    ungroup() %>%
    mutate(
      # epoch = round_date(airport_day + minutes(round(epoch)), time_step)
      # 3x faster than lubridate op above!
      epoch = lubridate::round_date(
        numeric_datetime_plus_mins(
          airport_day,
          round(epoch)),
        time_step
      )
    )

  return(dwell_len)

}
