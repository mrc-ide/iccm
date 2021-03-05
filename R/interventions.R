#' Vaccine impact
#'
#' Estimate modifier due to vaccine impact
#'
#' @param type Infection type
#' @param index Infection type index
#' @param target Individual indices
#' @param ages Individuals ages
#' @param p Condition specific parameters
#' @param variables Model variables
#'
#' @return Vaccine modifier
vaccine_impact <- function(type, index, target, ages, p, variables){
  modifier <- rep(1, target$size())
  if(type %in% c("hib", "pneumococcus", "rotavirus")){
    if(type == "hib"){
      vaccinated <- variables$hib_vx$get_values(target)
    }
    if(type == "pneumococcus"){
      vaccinated <- variables$pneumococcus_vx$get_values(target)
    }
    if(type == "rotavirus"){
      vaccinated <- variables$rotavirus_vx$get_values(target)
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
llin_impact <- function(type, target, p, variables){
  modifier <- rep(1, target$size())
  if(type == "pf"){
    modifier = variables$llin$get_values(target) * p$llin_efficacy
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
