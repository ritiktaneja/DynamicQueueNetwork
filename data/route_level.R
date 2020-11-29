route_level <-
  tibble::tribble(
    ~route      , ~rate_imm, ~server_imm                              ,
    "imm_mg" , 1.2      , queuecomputer::as.server.stepfun(c(360, 420, 630, 855, 945, 1260), c(4, 14, 15, 4, 6, 6, 4)),
    "imm_sg"   , 1.7      , 6
  )
