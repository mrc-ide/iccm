#' Vaccine impact
#'
#' Estimate modifier due to vaccine impact
#'
#' @param ages Individuals ages
#' @param p Condition specific parameters
#' @param variable Vaccine coverage variable for specific disease
#'
#' @return Vaccine modifier
vaccine_impact <- function(disease, target, ages, p, variables){
  if(p$vaccine) {
    modifier <- 1 - variables[[paste0(disease, "_vx")]]$get_values(target) * vaccine_effect(ages, p$vx_start, p$vx_initial_efficacy, p$vx_hl)
  } else {
    modifier <- rep(1, target$size())
  }
  return(modifier)
}

#' Vaccine effect
#'
#' Estimates the level of protection from vaccination given time since vaccination and vaccine properties.
#'
#' @param ages Ages of children
#' @param vx_start Age of vaccinated child when fully protected
#' @param vx_initial_efficacy Initial (maximum) efficacy
#' @param vx_hl Vaccine protection half life
#'
#' @return Vaccine effects
vaccine_effect <- function(ages, vx_start, vx_initial_efficacy, vx_hl){
  vaccine_age <- ages - vx_start
  vaccine_age[vaccine_age < 0] <- Inf
  vx_initial_efficacy * exp(-vaccine_age * (1/ vx_hl))
}

#' LLIN impact
#'
#' Estimate modifier due to LLIN impact
#'
#' @inheritParams vaccine_impact
llin_impact <- function(disease, target, p, variables){
  if(disease == "plasmodium_falciparum"){
    modifier = 1 - variables$llin$get_values(target) * p$llin_efficacy
  } else {
    modifier <- rep(1, target$size())
  }
  return(modifier)
}

#' Intervention community impact modifier
#'
#' @inheritParams vaccine_impact
community_impact <- function(disease, index, p){
  community_modifier <- 1
  if(disease %in% c("hib", "pneumococcus", "rotavirus")){
    community_modifier <- 1 - p$vx_ci[index]
  }
  if(disease == "llin"){
    community_modifier <- 1- p$llin_ci
  }
  return(community_modifier)
}


