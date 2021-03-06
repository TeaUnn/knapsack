% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy_knapsack.R
\name{greedy_knapsack}
\alias{greedy_knapsack}
\title{Greedy heuristic solve for the knapsack problem}
\usage{
greedy_knapsack(x, W)
}
\arguments{
\item{x}{A data frame with two variables w = weight and v = value}

\item{W}{A numeric scalar with the maximum weight the knapsack is allowed to contain}
}
\value{
The function returns a list with the optimum packed value and the corresponding elements
}
\description{
A function that uses greedy heuristic to solve the knapsack problem
}
\examples{
n <- 10000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                            v = runif(n = n, 0, 10000))
greedy_knapsack(x = knapsack_objects, W = 3500)

}
