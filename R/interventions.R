#' Vaccine impact
#'
#' Estimate modifier due to vaccine impact
#'
#' @param target Target
#' @param disease Disease index
#' @param ages Individuals ages
#' @param parameters Model parameters
#' @param variables Model variables
#'
#' @return Vaccine modifier
vaccine_impact <- function(target, disease, ages, parameters, variables){
  if(parameters$disease[[disease]]$vaccine_coverage > 0) {
    modifier <- 1 - variables$vaccine[[disease]]$get_values(target) *
      vaccine_effect(
        ages,
        parameters$disease[[disease]]$vaccine_start,
        parameters$disease[[disease]]$vaccine_initial_efficacy,
        parameters$disease[[disease]]$vaccine_hl
      )
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
#' @param vaccine_start Age of vaccinated child when fully protected
#' @param vaccine_initial_efficacy Initial (maximum) efficacy
#' @param vaccine_hl Vaccine protection half life
#'
#' @return Vaccine effects
vaccine_effect <- function(ages, vaccine_start, vaccine_initial_efficacy, vaccine_hl){
  vaccine_age <- ages - vaccine_start
  vaccine_age[vaccine_age < 0] <- Inf
  vaccine_initial_efficacy * exp(-vaccine_age * (1/ vaccine_hl))
}

#' LLIN impact
#'
#' Estimate modifier due to LLIN impact
#'
#' @inheritParams vaccine_impact
llin_impact <- function(target, disease, parameters, variables){
  if(names(parameters$disease)[disease] == "plasmodium_falciparum"){
    modifier = 1 - variables$llin$get_values(target) * parameters$disease[[disease]]$llin_efficacy
  } else {
    modifier <- rep(1, target$size())
  }
  return(modifier)
}

#' Intervention community impact modifier
#'
community_impact <- function(){

}


