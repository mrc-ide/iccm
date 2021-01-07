#' Random deviates from upper-truncated exponential distribution
#'
#' Code adapted from Lorenzo Rimella (2017). RGeode: Geometric Density Estimation.
#'    R package version 0.1.0. https://CRAN.R-project.org/package=RGeode
#'
#' @param n Number of draws
#' @param rate Rate
#' @param lower Lower truncation (inclusive: [])
#' @param upper Upper truncation (inclusive: [])
#'
#' @return Random draws from truncated exponential
rtexp <- function (n = 1, rate = 1, lower = 0, upper = Inf){
  stopifnot(n >= 1,
            rate > 0,
            lower >= 0,
            lower < upper)

  cdf1 = stats::pexp(lower, rate = rate)
  cdf2 = stats::pexp(upper, rate = rate)
  stats::qexp(cdf1 + stats::runif(n) * (cdf2 - cdf1), rate = rate)
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
