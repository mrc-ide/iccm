#' Random deviates from upper-truncated exponential distribution
#'
#' @param n Number of draws
#' @param rate Rate
#' @param upper Upper truncation (inclusive: (0, upper])
#'
#' @return Random draws from truncated exponential
rtexp <- function (n = 1, rate = 1, upper = Inf){
  stopifnot(n >= 1,
            rate > 0,
            upper > 0,
            is.numeric(n),
            is.numeric(rate),
            is.numeric(upper))

  cdf = stats::pexp(upper, rate = rate)
  stats::qexp(stats::runif(n) * cdf, rate = rate)
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
