
#' @export
airport_distance <- function(theta, inp, type = "MMD"){


  global_level_1 <- inp$global_level_1

  global_level_1 <- global_fun_1(walk_alpha = theta[1], walk_beta = theta[2])

  route_level <- inp$route_level

  route_level$rate_imm <- theta[3:4]


  prop_df <- AirportSimulate1(global_level_1, inp$flight_level, inp$gate_level, inp$nat_level, route_level = route_level)

  p <- weight_obs(synthetic_realisations(prop_df))
  o <- inp$observations_w

  if(type == "MMD"){

    dist_out <- EasyMMD::MMD(o$arrive_imm$value, p$arrive_imm$value, bias = TRUE, sigma = inp$sigma_k, threshold = inp$threshold, w_y = o$arrive_imm$count, w_x = p$arrive_imm$count) +
      EasyMMD::MMD(o$depart_imm$value, p$depart_imm$value, bias = TRUE, sigma = inp$sigma_k, threshold = inp$threshold, w_y = o$depart_imm$count, w_x = p$depart_imm$count) +
      EasyMMD::MMD(o$depart_imm_sg$value, p$depart_imm_sg$value, bias = TRUE, sigma = inp$sigma_k, threshold = inp$threshold, w_y = o$depart_imm_sg$count, w_x = p$depart_imm_sg$count)

  } else if(type == "Wass"){

    dist_out <- transport::wasserstein1d(o$arrive_imm$value, p$arrive_imm$value, wa = o$arrive_imm$count, wb = p$arrive_imm$count) +
      transport::wasserstein1d(o$depart_imm$value, p$depart_imm$value, wa = o$depart_imm$count, wb = p$depart_imm$count) +
      transport::wasserstein1d(o$depart_imm$value, p$depart_imm$value, wa = o$depart_imm$count, wb = p$depart_imm$count)

  }

  return(dist_out)
}
