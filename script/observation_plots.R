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

w_10 <- TRUE

parameter_names <- c(
  `arrive_imm`     = "Enter immigration (overall)",
  `arrive_imm_mg`  = "Enter immigration (manual-gate)",
  `arrive_imm_sg`  = "Enter immigration (smart-gate)",
  `depart_imm`     = "Leave immigration (overall)",
  `depart_imm_mg`  = "Leave immigration (manual-gate)",
  `depart_imm_sg`  = "Leave immigration (smart-gate)"
)



if(!w_10){

  ggplot(observations) +
    geom_freqpoly(breaks = 360:1140 * 60, mapping = aes(x = value * 60)) +
    facet_wrap(~key, ncol = 1, labeller = as_labeller(parameter_names)) +
    geom_linerange(data = flight_level, mapping = aes( x = arrive * 60, ymin = passengers * 0, ymax = passengers / 5), inherit.aes = FALSE, col = "blue") +
    geom_point(data = flight_level, mapping = aes( x = arrive * 60, y = passengers / 5), inherit.aes = FALSE, col = "blue") +
    theme_few() +
    scale_y_continuous(sec.axis = sec_axis(~. * 5, name = "Passengers on flight (Passengers)"), expand = expand_scale(mult = c(0, 0.05))) +
    scale_x_time() +
    xlab("Time of day") +
    ylab("Passenger/min")

} else {

  x <- expand.grid(key = c("arrive_imm", "arrive_imm_mg", "arrive_imm_sg", "depart_imm", "depart_imm_mg", "depart_imm_sg"), value = c(360:1130))

  input <- dplyr::bind_rows(observations_w_10, .id = "key") %>%
    right_join(x)

  input$count[which(is.na(input$count))] <- 0

  ggplot(input) +
    geom_line(mapping = aes(x = value * 60, y = count), col = "black") +
    facet_wrap(~key, ncol = 1, labeller = as_labeller(parameter_names)) +
    geom_linerange(data = flight_level, mapping = aes( x = arrive * 60, ymin = passengers * 0, ymax = passengers / 5), inherit.aes = FALSE, size = 0.1, linetype = "dashed") +
    geom_point(data = flight_level, mapping = aes( x = arrive * 60, y = passengers / 5), inherit.aes = FALSE, size = 0.2) +
    theme_few() +
    theme(axis.title.y.right = element_text(margin = margin(l = 10))) +
    scale_y_continuous(sec.axis = sec_axis(~. * 5, name = "Passengers on flight (Passengers)", breaks = scales::pretty_breaks(2)), expand = expand_scale(mult = c(0, 0.05)), breaks = scales::pretty_breaks(2)) +
    scale_x_time() +
    xlab("Time of day") +
    ylab("Passenger/min")

}

ggsave(file = paste0("observation_plot.pdf"), height = 10, width = 7.5, scale = 0.8)
