library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(latex2exp)
library(AirportPassengerFlow)
library(lubridate)

set.seed(1)

args <- commandArgs(trailingOnly = TRUE)
fn <- args[1]
synthetic <- as.logical(args[2])

# fn <- "runs/17/MMD_real.csv"
# synthetic <- FALSE

samples <- read.csv(fn, header = FALSE)

abc_priors <-
  data_frame(
    Walking_mu = c(0, 2.5),
    Walking_sigma = c(0, 2.5),
    Service_manual = c(0, 2.5),
    Service_smart = c(0, 2.5)
  )

quanty <- function(x){quantile(x,probs = 0.9)}

names(samples) <- c("Walking_1", "Walking_2", "Service_manual", "Service_smart")

samples <- samples %>%
  mutate(
    Walking_mu = Walking_1/Walking_2,
    Walking_sigma = sqrt(Walking_1)/Walking_2
  ) %>%
  select(-Walking_1, -Walking_2)

prior_samples <- samples

n_prior <- 10000

prior_samples <-
  data_frame(
    alpha_ac = runif(n_prior, 0, 10),
    beta_ac = runif(n_prior, 0, 10),
    Walking_mu = alpha_ac/beta_ac,
    Walking_sigma = sqrt(alpha_ac)/beta_ac,
    Service_manual = runif(n_prior, 0, 2.5),
    Service_smart = runif(n_prior, 0, 2.5)
  )

abc_true_values <-
  data_frame(
    key = names(samples),
    value = c(1.4, 0.8, 3.1/2.2, sqrt(3.1)/2.2),
    estimator = "True value"
  )

abc_input <- bind_rows(samples, abc_priors)

abc_samples_tidy <-
  gather(
    abc_input,
    key = key,
    value = value
  )

#print(abc_samples %>% group_by(key) %>% summarise(median(value)))

abc_estimators <- abc_samples_tidy %>% group_by(key) %>% summarise(Median = median(value), "90% credible interval" = quantile(value, 0.05), upper = quantile(value, 0.95)) %>% gather(key = estimator, value = value, -key)

print(abc_estimators)

abc_estimators$estimator[which(abc_estimators$estimator == "upper")] <- "90% credible interval"

abc_estimators <- bind_rows(abc_estimators, abc_true_values)

if(!synthetic){
  abc_estimators <- abc_estimators[-which(abc_estimators$estimator == "True value"), ]
}

parameter_names <- c(
  `Walking_mu` = "μ^'ac':` Mean walking time `(min~m^'-1')",
  `Walking_sigma` = "σ^'ac':` St. dev walking time `(min~m^'-1')",
  `Service_manual` = "λ[MG]:` Service rate manual-gate `(min^'-1')",
  `Service_smart` = "λ[SG]:` Service rate smart-gate `(min^'-1')"
)

if(synthetic){
  tag <- "Synthetic data"
  y_post <- TeX('ABC posterior density $\\pi_{ABC} (\\theta | y_{syn})$')
} else {
  tag <- "Real data"
  y_post <- TeX('ABC posterior density $\\pi_{ABC} (\\theta | y)$')
}

output <- ggplot(abc_samples_tidy) +
  aes(x = value) +
  geom_density(trim = TRUE) +
  facet_wrap(
    ~ key,
    scales = "free",
    ncol = 2,
    labeller = as_labeller(parameter_names, label_parsed),
    strip.position = "bottom"
  ) +
  xlab(expression(theta)) +
  ylab(y_post) +
  geom_vline(data = abc_estimators, aes(xintercept = value, col = estimator, linetype = estimator)) +
  scale_linetype_manual(values = c("dotted","dashed", "solid")) +
  scale_color_manual(values = c("red", "red", "blue")) +
  theme_few() +
  scale_x_continuous(expand = expand_scale(add = c(0, 0)),
                     breaks = scales::pretty_breaks(n = 4), limits = c(0, 2.5)) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0, 0.05))
  ) +
  theme(
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    plot.margin = margin(10, 20, 10, 10),
    legend.title=element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(margin = margin(r = 10)),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(vjust = 1.9),
    panel.border = element_blank()
  )

# output

bn <- base::basename(getwd())
ggsave(file = paste0(tools::file_path_sans_ext(fn), "_", bn, "_density.pdf"), device = cairo_pdf)


# cairo_pdf(file = paste0(tools::file_path_sans_ext(fn), "_", bn, "_density.pdf"))

output

# dev.off()

walk_df <- samples %>%
  mutate(ID = c(1:n())) %>%
  group_by(ID) %>%
  do(
    data.frame(
      speed = 1/AirportPassengerFlow::simulate_walk(100, c(.$Walking_mu^2 / .$Walking_sigma^2, .$Walking_mu / .$Walking_sigma^2), 1, max_time = Inf)
    )
  ) %>% ungroup()

walk_df$ID <- "Posterior predictive distribution"

true_walk_df <- data.frame(ID = "True value", speed = 1/AirportPassengerFlow::simulate_walk(1e6, c(abc_true_values$value[3]^2 / abc_true_values$value[4]^2, abc_true_values$value[3] / abc_true_values$value[4]^2), 1, max_time = Inf))

if(synthetic){
  walk_df <- bind_rows(walk_df, true_walk_df)
}

walk_est <- walk_df %>%
  group_by(ID) %>%
  summarise(
    Median = median(speed),
    "80% PI" = quantile(speed, 0.1),
    upper = quantile(speed, 0.9)
  ) %>%
  gather(key, value, -ID)

walk_est$key[which(walk_est$key == "upper")] <- "80% PI"

print(walk_est)

output <- ggplot(walk_df) +
  aes(x = speed, col = ID) +
  stat_density(geom="line", show.legend = FALSE, position = "identity") +
  theme_few() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  xlab("Passenger walking speed (m/min)") +
  ylab("Density") +
  scale_x_continuous(expand = expand_scale(mult = c(0, 0))) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0, 0.05))
  ) +
  xlim(c(0, 400)) +
  geom_vline(data = walk_est, aes(xintercept = value, linetype = key, col = ID)) +
  scale_linetype_manual(values = c("dotted","dashed")) +
  scale_color_manual(values = c("red", "blue", "blue"))

pdf(file = paste0(tools::file_path_sans_ext(fn), "_", bn, "_walk.pdf"))

  output

dev.off()

if(synthetic){
  stop()
}

# flight_level <- read.csv("../../data/flight_level_gamma.csv")
# gate_level <- read.csv("../../data/gate_level.csv")
# nat_level <- read.csv("../../data/nat_level.csv")
# source("../../data/route_level.R")
# observations <- read.csv("../../data/flow_counts.csv")


observations_w <- observations_w_10


global_level_1 <- global_fun_1(walk_alpha = 1.5, walk_beta = 3)

sigma_k <- 20

inp <- list(global_level_1 = global_level_1, flight_level = flight_level_disembark, gate_level = gate_level, nat_level = nat_level, route_level = route_level, observations_w = observations_w)

inp$sigma_k <- sigma_k
inp$threshold <- 6

set.seed(1)



samples <- read.csv(fn, header = FALSE)

names(samples) <- c("Walking_1", "Walking_2", "Service_manual", "Service_smart")

samples <- samples %>% mutate(simID = 1:n())

out_obs <- samples %>%
  dplyr::sample_n(400, replace = TRUE) %>%
  group_by(simID) %>%
  dplyr::do(generate_realisations(c(.$Walking_1, .$Walking_2, .$Service_manual, .$Service_smart), inp = inp, full = TRUE)) %>%
  mutate(value = round(value)) %>%
  group_by(simID, value, key) %>%
  mutate(count = n()) %>%
  group_by(value, key)

zero_df <- expand.grid(simID = unique(out_obs$simID), key = c("arrive_imm", "depart_imm", "arrive_imm_sg", "arrive_imm_mg", "depart_imm_sg", "depart_imm_mg"), value = c(360:1200))

x <- full_join(out_obs, zero_df, by = c("simID", "key", "value")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

y <- x %>%
  group_by(value, key) %>%
  summarise(low = quantile(count, 0.025), med = quantile(count, 0.5), high = quantile(count, 0.975))

parameter_names <- c(
  `arrive_imm` = "Enter immigration (overall)",
  `arrive_imm_mg` = "Enter immigration (manual-gate)",
  `arrive_imm_sg` = "Enter immigration (smart-gate)",
  `depart_imm` = "Exit immigration (overall)",
  `depart_imm_mg` = "Exit immigration (manual-gate)",
  `depart_imm_sg` = "Exit immigration (smart-gate)"
)


x <- expand.grid(key = c("arrive_imm", "arrive_imm_mg", "arrive_imm_sg", "depart_imm", "depart_imm_mg", "depart_imm_sg"), value = c(360:1130))

input <- dplyr::bind_rows(observations_w_10, .id = "key") %>%
  right_join(x)

input$count[which(is.na(input$count))] <- 0

output <- ggplot(input) +
  geom_ribbon(mapping = aes(ymin = low, ymax = high, fill = "95% prediction interval", x = value * 60), data = y) +
  geom_line(mapping = aes(x = value * 60, y = count, col = "Passenger flow")) +
  scale_fill_manual(values = "red") +
  scale_color_manual(values = "black") +
  facet_wrap(~key, ncol = 1, labeller = as_labeller(parameter_names)) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0, 0.05)),
    breaks = scales::pretty_breaks(n = 2)
  ) +
  theme_few() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Time of day") +
  ylab("Passenger flow (Passenger/min)") +
  scale_x_time(limits = c(NA,19*3600))


output

ggsave(file = paste0(tools::file_path_sans_ext(fn), "_", bn, "_flow-pred.pdf"), height = 10, width = 7.5, scale = 0.8)



