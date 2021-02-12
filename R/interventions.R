#' Vaccine impact
#'
#' Estimate modifier due to vaccine impact
#'
#' @param type Infection type
#' @param target Individual indices
#' @param ages Individuals ages
#' @param p Condition specific parameters
#' @param individuals Model individuals
#' @param variables Model variables
#' @param api Model api
#'
#' @return Vaccine modifier
vaccine_impact <- function(type, target, ages, p, individuals, variables, api){
  modifier <- rep(1, length(target))
  if(type %in% c("hib", "pneumococcus", "rotavirus")){
    if(type == "hib"){
      covered = api$get_variable(individuals$child, variables$hib_vx, target)
    }
    if(type == "pneumococcus"){
      covered = api$get_variable(individuals$child, variables$pneumococcus_vx, target)
    }
    if(type == "rotavirus"){
      covered = api$get_variable(individuals$child, variables$rotavirus_vx, target)
    }
    modifier = covered * vaccine_effect(ages, p$vx_start, p$vx_initial_efficacy, p$vx_hl)
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
  1 - vx_initial_efficacy * exp(-vaccine_age * (1/ vx_hl))
}

#' LLIN impact
#'
#' Estimate modifier due to LLIN impact
#'
#' @inheritParams vaccine_impact
llin_impact <- function(type, target, p, individuals, variables, api){
  modifier <- rep(1, length(target))
  if(type == "pf"){
    modifier = api$get_variable(individuals$child, variables$llin, target) * p$llin_efficacy
  }
  return(modifier)
}
