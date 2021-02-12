#' Maternal immunity
#'
#' Modifier of infection between 0 (all blocked) and 1 (none-blocked), dependent on
#' a child's age.
#'
#' @param age Age
#' @param hl Halflife of duration of maternal immunity
#'
#' @return Maternal immunity modifier
#' @export
maternal_immunity <- function(age, hl){
  stats::pexp(age, 1 / hl)
}

#' Exposure-driven immunity
#'
#' Modifier of infection 0 (all blocked) and 1 (none-blocked), dependent on
#' a child's prior exposure.
#'
#' @param exposure Number of previous infections
#' @param shape Shape parameter
#' @param rate Rate parameter (for a fixed shape, a higher rate will lead to a faster build up of immunity)
#'
#' @return Exposure immunity modifier
#' @export
exposure_immunity <- function(exposure, shape, rate){
  1 - stats::pgamma(q = exposure, shape = shape, rate = rate)
}


#' Draw individual level heterogeneities
#'
#' @param n Number
#' @param het_sd Standard deviation
heterogeneity <- function(n, het_sd){
 exp(stats::rnorm(n, -het_sd / 2, het_sd^0.5))
}
