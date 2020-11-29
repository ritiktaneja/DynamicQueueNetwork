
# Scenarios

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(latex2exp)
library(AirportPassengerFlow)
library(lubridate)
library(ggalt)

set.seed(1)

# args <- commandArgs(trailingOnly = TRUE)
# fn <- args[1]
# synthetic <- as.logical(args[2])

fn <- "runs/26/MMD_real.csv"
synthetic <- FALSE

samples <- read.csv(fn, header = FALSE)

names(samples) <- c("Walking_1", "Walking_2", "Service_manual", "Service_smart")

samples <- samples %>%
  dplyr::sample_n(500) %>%
  mutate(sim_number = 1:n())

mini_flights <- flight_level_disembark[c(14:18),]

mini_flights2 <- mini_flights

mini_flights2$arrive[2] <- mini_flights2$arrive[2] + 15

inp <- list(global_level_1 = global_level, flight_level = mini_flights, gate_level = gate_level, nat_level = nat_level, route_level = route_level, observations_w = observations_w)

inp2 <- list(global_level_1 = global_level, flight_level = mini_flights2, gate_level = gate_level, nat_level = nat_level, route_level = route_level, observations_w = observations_w)

mini_flights3 <- mini_flights2
# mini_flights3$arrive[4] <- mini_flights3$arrive[4] + 10

route_level3 <- route_level
route_level3$server_imm[[1]]$y[3] <- 13
route_level3$server_imm[[1]]$y[4] <- 6

inp3 <- list(global_level_1 = global_level, flight_level = mini_flights3, gate_level = gate_level, nat_level = nat_level, route_level = route_level3, observations_w = observations_w)

scenario_1 <- samples %>%
  group_by(sim_number) %>%
  do(AirportPassengerFlow::Airport_parameters(c(.$Walking_1, .$Walking_2, .$Service_manual, .$Service_smart), inp = inp)) %>%
  mutate(scenario = 1)

scenario_2 <- samples %>%
  group_by(sim_number) %>%
  do(AirportPassengerFlow::Airport_parameters(c(.$Walking_1, .$Walking_2, .$Service_manual, .$Service_smart), inp = inp2)) %>%
  mutate(scenario = 2)

scenario_3 <- samples %>%
  group_by(sim_number) %>%
  do(AirportPassengerFlow::Airport_parameters(c(.$Walking_1, .$Walking_2, .$Service_manual, .$Service_smart), inp = inp3)) %>%
  mutate(scenario = 3)

scenario_df <- dplyr::bind_rows(scenario_1, scenario_2, scenario_3) %>%
  filter(route == "imm_mg")


flight_df <- dplyr::bind_rows(mini_flights %>% mutate(scenario = 1), mini_flights2 %>% mutate(scenario = 2), mini_flights3 %>% mutate(scenario = 3)) %>%
  mutate(epoch = lubridate::as_datetime("2017-08-07") + minutes(arrive))

route_df1 <- data.frame(x = c(5000, route_level$server_imm[[1]]$x), y = route_level$server_imm[[1]]$y)

route_df3 <- data.frame(x = c(5000, route_level3$server_imm[[1]]$x), y = route_level3$server_imm[[1]]$y)

route_df <- bind_rows(route_df1 %>% mutate(scenario = 1), route_df1 %>% mutate(scenario = 2), route_df3 %>% mutate(scenario = 3)) %>%
  mutate(epoch = as_datetime("2017-08-07") + minutes(x))

wait_df <- scenario_df %>%
  group_by(sim_number, scenario) %>%
  do(calc_sys_wait_time(., arrivals = "arrive_imm", wait_times = "wait_imm", group_col = "route", time_step = "5 mins")) %>%
  group_by(epoch, scenario, route) %>%
  summarise(lower = quantile(value, 0.025), med = quantile(value, 0.5), upper = quantile(value, 0.975)) %>%
  ungroup() %>%
  padr::pad(group = c("scenario", "route"))

# wait_df[which(is.na(wait_df$lower)),][,c(4,5,6)] <- 0

scenario_labels <- c(
  `1` = "Case 1: Predictions based on planned flight schedule",
  `2` = "Case 2: Predictions after news of flight delay is received",
  `3` = "Case 3: Predictions after corrective action is taken"
)

ggplot(wait_df) + aes(x = epoch, y = med, ymin = lower, ymax = upper) + geom_ribbon(fill = "red") +
  facet_wrap(~scenario, ncol = 1, labeller = as_labeller(scenario_labels)) +
  xlab("Time of day") +
  theme_few() +
  geom_step(data = route_df, mapping = aes(x = epoch, y = y * 5), inherit.aes = FALSE, col = "grey") +
  geom_point(data = flight_df, mapping = aes(x = epoch, y = passengers / 4), inherit.aes = FALSE, size = 1) +
  geom_linerange(data = flight_df, mapping = aes(x = epoch, ymin = 0, ymax = passengers / 4), inherit.aes = FALSE, linetype = "dashed", size = 0.5, col = "black") +
  coord_cartesian(xlim=c(as_datetime("2017-08-07 09:00:00"), as_datetime("2017-08-07 13:30:00"))) +
  scale_y_continuous("Waiting time (min)",
                     sec.axis = sec_axis(~ . * 1/5, name = "Number of servers"), expand = expand_scale(mult = c(0, 0.05)))

ggsave(file = "wait_pf.pdf", height = 10, width = 7.5, scale = 0.8)

queue_df <- scenario_df %>%
  group_by(sim_number, scenario) %>%
  do(calc_sys_dwell_len(., arrivals = "arrive_imm", departures = "start_imm", group_col = "route", time_step = "1 mins")) %>%
  group_by(epoch, scenario, route) %>%
  summarise(lower = quantile(value, 0.025), med = quantile(value, 0.5), upper = quantile(value, 0.975)) %>%
  filter(epoch >= lubridate::as_datetime("2017-08-07 06:00:00"))

ggplot(queue_df) + aes(x = epoch, y = med, ymin = lower, ymax = upper) + geom_ribbon(fill = "red") +
  facet_wrap(~scenario, ncol = 1, labeller = as_labeller(scenario_labels)) +
  xlab("Time of day") +
  theme_few() +
  geom_step(data = route_df, mapping = aes(x = epoch, y = y * 20), inherit.aes = FALSE, col = "grey") +
  geom_point(data = flight_df, mapping = aes(x = epoch, y = passengers), inherit.aes = FALSE, size = 1) +
  geom_linerange(data = flight_df, mapping = aes(x = epoch, ymin = 0, ymax = passengers), inherit.aes = FALSE, linetype = "dashed", size = 0.5) +
  coord_cartesian(xlim=c(as_datetime("2017-08-07 09:00:00"), as_datetime("2017-08-07 13:30:00"))) +
  scale_y_continuous("Queue length",
                     sec.axis = sec_axis(~ . * 1/20, name = "Number of servers"), expand = expand_scale(mult = c(0, 0.05)))

ggsave(file = "queue_pf.pdf", height = 10, width = 7.5, scale = 0.8)
