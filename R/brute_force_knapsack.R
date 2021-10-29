#' Brute force solve for the knapsack problem
#'
#' A function that uses brute force to solve the knapsack problem
#'
#' @param x A data frame with two variables w = weight and v = value
#'
#' @param W A numeric scalar with the maximum weight the knapsack is allowed to contain
#'
#' @return The function returns a list with the optimum packed value and the corresponding elements
#'
#' @examples
#' n <- 10
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#'                             v = runif(n = n, 0, 10000))
#' brute_force_knapsack(x = knapsack_objects, W = 3500)
#'
#' @import dplyr
#'
#' @export
brute_force_knapsack <- function(x, W){
  if(!is.data.frame(x)) stop("x is not a data frame!")
  if(ncol(x) != 2) stop("x does not have correct dimension!")
  if(colnames(x)[1] != "w" & colnames(x)[2] != "v") stop("Columns in x are not named correctly!")
  if(any(x < 0)) stop("Faulty values in x!")
  possible_combinations <- data.frame("weight" = NA, "value" = NA)
  for (i in 1:(2^(nrow(x)))){
    possible_combinations[i, "weight"] <-
      sum(knapsack_objects[which(as.numeric(intToBits(i)) == 1), 1])
    possible_combinations[i, "value"] <-
      sum(knapsack_objects[which(as.numeric(intToBits(i)) == 1), 2])
  }
  output <- possible_combinations %>% filter(weight <= W) %>% slice_max(value, n = 1)
  elements <- which(as.numeric(intToBits(which(
    possible_combinations[ , "value"] == output[1, "value"]))) == 1)
  return(list(value = output[ , "value"], elements = elements))
}
