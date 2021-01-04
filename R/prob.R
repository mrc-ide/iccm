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
  stopifnot(r >= 0,
            t > 0,
            is.numeric(r),
            is.numeric(t))

  1 - exp(-r * t)
}
