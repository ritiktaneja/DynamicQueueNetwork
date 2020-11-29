
library(dplyr)
library(tidyr)
library(ggplot2)
library(AirportPassengerFlow)

set.seed(1)

flight_level_disembark
gate_level
nat_level
route_level
observations_w_10

global_level_1 <- global_fun_1(walk_alpha = 1.5, walk_beta = 3)

sigma_k <- 20

inp <- list(global_level_1 = global_level_1, flight_level = flight_level_disembark, gate_level = gate_level, nat_level = nat_level, route_level = route_level, observations_w = observations_w_10)

inp$sigma_k <- sigma_k
inp$threshold <- 6

set.seed(1)

synthetic_observations <- AirportPassengerFlow::generate_realisations(c(3.1, 2.2, 1.4, 0.8), inp = inp)

synthetic_observations_w <- weight_obs(synthetic_observations)

simulator <- AirportPassengerFlow::airport_distance

inp_s <- list(global_level_1 = global_level_1, flight_level = flight_level_disembark, gate_level = gate_level, nat_level = nat_level, route_level = route_level, observations_w = synthetic_observations_w)

inp_s$sigma_k <- sigma_k
inp_s$threshold <- 6
