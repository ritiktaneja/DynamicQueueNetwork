#' @export
synthetic_realisations <- function(passenger_df, full = FALSE){

  prop_arrive_imm <- dplyr::data_frame(key = "arrive_imm", value = passenger_df$arrive_imm)

  prop_depart_imm <- dplyr::data_frame(key = "depart_imm", value = passenger_df$depart_imm)

  prop_depart_imm_sg <- dplyr::data_frame(key = "depart_imm_sg", value = passenger_df %>% filter(route == "imm_sg") %>% pull(depart_imm))

  output_df <- dplyr::bind_rows(prop_arrive_imm, prop_depart_imm, prop_depart_imm_sg)

  if(full){
    prop_arrive_imm_sg <- dplyr::data_frame(key = "arrive_imm_sg", value = passenger_df %>% filter(route == "imm_sg") %>% pull(arrive_imm))

    prop_arrive_imm_mg <- dplyr::data_frame(key = "arrive_imm_mg", value = passenger_df %>% filter(route == "imm_mg") %>% pull(arrive_imm))

    prop_depart_imm_mg <- dplyr::data_frame(key = "depart_imm_mg", value = passenger_df %>% filter(route == "imm_mg") %>% pull(depart_imm))

    output_df <- dplyr::bind_rows(prop_arrive_imm, prop_depart_imm, prop_arrive_imm_sg, prop_arrive_imm_mg, prop_depart_imm_sg, prop_depart_imm_mg)
  }

  return(output_df)
}

#' @export
Airport_parameters <- function(theta, inp){
  global_level_1 <- inp$global_level_1

  global_level_1 <- global_fun_1(walk_alpha = theta[1], walk_beta = theta[2])

  route_level <- inp$route_level

  route_level$rate_imm <- theta[3:4]

  prop_df <- AirportSimulate1(global_level_1, inp$flight_level, inp$gate_level, inp$nat_level, route_level = route_level)

  return(prop_df)
}

#' @export
generate_realisations <- function(theta, inp, full = FALSE){

  prop_df <- Airport_parameters(theta, inp)

  output_df <- synthetic_realisations(prop_df, full = full)

  return(output_df)
}

#' @export
weight_obs <- function(observations){
  observations$value <- round(observations$value)

  observations_w <- observations %>% group_by(key, value) %>% summarise(count = n())

  output <- split(observations_w[,-1], observations_w$key)

  return(output)
}

