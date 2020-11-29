library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(AirportPassengerFlow)
requireNamespace("broom")
requireNamespace("robust")

set.seed(1)

flight_level_true

# Fit deplane parameters ------------------

disembark_tidy <- read.csv("data-raw/disembark.csv")

disembark_tidy <- disembark_tidy %>% group_by(flight) %>% mutate(cumdist = cumsum(value) / sum(value)) %>%
  filter(value != 0)

max_min_flights <- summarise(disembark_tidy, start = min(which(cumdist > 0.025)), end = max(which(cumdist < 0.975)), disembark_time = end - start)

gamma_df <- left_join(disembark_tidy, max_min_flights) %>% group_by(flight) %>% filter(from_chocks >= start & from_chocks <= end) %>%
  do(data.frame(times = rep(.$from_chocks, times = .$value))) %>%
  group_by(flight) %>%
  filter(flight != 33) %>%
  do(broom::tidy(robust::gammaRob(.$times, "tdmean")[["estimate"]])) %>%
  rename(term = names, estimate = x) %>%
  spread(term, estimate) %>%
  rename(shape_dpl = shape, scale_dpl = scale)

disembark_joined <- left_join(disembark_tidy, gamma_df) %>%
  group_by(flight) %>%
  mutate(dgam = dgamma(from_chocks, first(shape_dpl), scale = first(scale_dpl)))

ggplot(flight_level_true %>% left_join(disembark_joined, by = "flight")) + aes(x = from_chocks, y = value) + geom_col() +
  geom_line(mapping = aes(x = from_chocks, y = dgam * passengers)) +
  facet_wrap(~flight) +
  theme(panel.grid = element_blank()) +
  xlab("Time from first passenger off plane (min)") +
  ylab("Passenger flow (Passengers/min)") +
  scale_x_continuous(expand = expand_scale(add = c(0, 0))) +
  scale_y_continuous(expand = expand_scale(add = c(0, 0)))

ggsave(filename = "disembark.pdf", height = 6, width = 6)

flight_level_true <- left_join(flight_level_true, gamma_df, by = "flight")

write.csv(flight_level_true, file = "data-raw/flight_level_true_gamma.csv", row.names = FALSE)
