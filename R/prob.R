#' Random deviates from upper-truncated exponential distribution
#'
#' @param n Number of draws
#' @param rate Rate
#' @param lower Lower truncation (inclusive: [lower, upper])
#' @param upper Upper truncation (inclusive: [lower, upper])
#'
#' @return Random draws from truncated exponential
rtexp <- function (n = 1, rate = 1, lower = 0, upper = Inf){
  stopifnot(n >= 1,
            rate > 0,
            lower >= 0,
            lower < upper)

  cdf1 = pexp(lower, rate = rate)
  cdf2 = pexp(upper, rate = rate)
  stats::qexp(cdf1 + runif(n) * (cdf2 - cdf1), rate = rate)
}

#' Convert rate to probability
#'
#' The probability a event with a constant rate will occur within a given time period
#'
#' @param r rate
#' @param t time period
#'
#' @return Probability of event in timestep
rate_to_prob <- function(r, t = 1){
  1 - exp(-r * t)
}


#' Convert probability to rate
#'
#' The constant rate at which an event with a given probability will occur
#'
#' @param p probability
#' @param t time period
#'
#' @return Probability of event in timestep
prob_to_rate <- function(p, t = 1){
  -log(1 - p) / t
}
