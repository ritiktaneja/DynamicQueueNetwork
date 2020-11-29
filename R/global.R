#' Global
#' @name Global
#' @aliases Global_fun
#' @aliases Global_list
#' @title Global
#'
#' @rdname Global
#' @export
global_fun_1 <- function(
  walk_alpha = 4,
  walk_beta  = 4,
  max_walk = 0.2,
  shape_dpl = 2,
  scale_dpl = 2
){

  return(as.list(environment(), all=TRUE))
}

global_level_1 <- global_fun_1()
