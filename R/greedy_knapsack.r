#' Greedy heuristic solve for the knapsack problem
#'
#' A function that uses greedy heuristic to solve the knapsack problem
#'
#' @param x A data frame with two variables w = weight and v = value
#'
#' @param W A numeric scalar with the maximum weight the knapsack is allowed to contain
#'
#' @return The function returns a list with the optimum packed value and the corresponding elements
#'
#' @examples
#' n <- 10000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#'                             v = runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects, W = 3500)
#'
#' @import dplyr
#'
#' @export
greedy_knapsack <- function(x, W){
  x <- mutate(.data = x, vpw = v/w, index = 1:nrow(x))
  x <- arrange(x, desc(vpw))
  W_current <- 0
  V_current <- 0
  elements_used <- 0
  for(i in 1:nrow(x)){
    if((W_current + x$w[i]) <= W){
      W_current <- W_current + x$w[i]
      V_current <- V_current + x$v[i]
      elements_used[i] <- x$index[i]
    }
  }
  res_list <- list(value = V_current, elements = elements_used[!is.na(elements_used)])
  return(res_list)
}
