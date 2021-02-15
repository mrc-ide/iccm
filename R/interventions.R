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
vaccine_impact <- function(type, index, target, ages, p, individuals, variables, api){
  modifier <- rep(1, length(target))
  if(type %in% c("hib", "pneumococcus", "rotavirus")){
    if(type == "hib"){
      vaccinated <- api$get_variable(individuals$child, variables$hib_vx, target)
    }
    if(type == "pneumococcus"){
      vaccinated <- api$get_variable(individuals$child, variables$pneumococcus_vx, target)
    }
    if(type == "rotavirus"){
      vaccinated <- api$get_variable(individuals$child, variables$rotavirus_vx, target)
    }
    modifier <- 1 - vaccinated * vaccine_effect(ages, p$vx_start[index], p$vx_initial_efficacy[index], p$vx_hl[index])
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
llin_impact <- function(type, target, p, individuals, variables, api){
  modifier <- rep(1, length(target))
  if(type == "pf"){
    modifier = api$get_variable(individuals$child, variables$llin, target) * p$llin_efficacy
  }
  return(modifier)
}

#' Intervention community impact modifier
#'
#' @inheritParams vaccine_impact
community_impact <- function(type, index, p){
  community_modifier <- 1
  if(type %in% c("hib", "pneumococcus", "rotavirus")){
    community_modifier <- 1 -p$vx_ci[index]
  }
  if(type == "llin"){
    community_modifier <- 1- p$llin_ci
  }
  return(community_modifier)
}
