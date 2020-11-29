
#' @import dplyr
#' @import queuecomputer
#' @export
AirportSimulate1 <-
  function(
    global_level,
    flight_level,
    gate_level,
    nat_level,
    route_level
  ){

    if(is.data.frame(global_level)){
      global_level <- as.list(global_level)
    }

    for(i in 1:length(global_level)){
      assign(names(global_level)[i], global_level[[i]])
    }

    passenger_table <- flight_level %>%
      left_join(data_frame(flight = rep(.$flight, times = .$passengers)), by = "flight") %>%
      mutate(ID = c(1:n())) %>%
      left_join(gate_level, by = "gate")

    passenger_table <- passenger_table %>%
      group_by(flight) %>%
      mutate(nat = factor(sample(c(rep("local", first(locals)), rep("foreign", n() - first(locals)))))) %>%
      left_join(nat_level, by = "nat") %>%
      ungroup() %>%
      group_by(nat) %>%
      mutate(route = sample(c("imm_sg", "imm_mg"), size = n(), replace = TRUE, prob = c(first(imm_sg), first(imm_mg)))) %>%
      left_join(route_level, by = "route")

    passenger_table <- passenger_table %>%
      group_by(flight, nat, route) %>%
      mutate(
        deplane      = rgamma(n(), shape = shape_dpl[1], scale = scale_dpl[1]),
        arrive_ac    = arrive + deplane,
        walk_ac   = simulate_walk(n(), c(walk_alpha[1], walk_beta[1]), distance_gate[1], max_time = max_walk),
        arrive_imm   = arrive_ac + walk_ac,
        service_imm  = rexp(n(), rate_imm[1])
      ) %>%
      ungroup() %>%
      group_by(route) %>%
      mutate(
        depart_imm   = queue(arrive_imm, service_imm, server_imm[[1]])
      )

    passenger_table <- passenger_table %>%
      mutate(
        system_dpl   = arrive_ac     - arrive,
        system_ac    = arrive_imm    - arrive_ac,
        system_imm   = depart_imm     - arrive_imm,
        system_total = system_dpl + system_ac + system_imm,
        wait_imm     = system_imm - service_imm,
        start_imm    = arrive_imm + wait_imm
      )

    return(passenger_table)
  }


#' @export
simulate_walk <- function(n, walking_param, distance, max_time = Inf, dist = NULL){

  output <- distance * rgamma(n, shape = walking_param[1], rate = walking_param[2]) / 60

  output <- pmin.int(output, max_time * distance)

  return(output)
}
