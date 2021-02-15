#' Graduation
#'
#' Replace a child who reaches the maximum age in the simulation with a new child.
#'
#' @param parameters Model parameters
#' @param individuals Model individuals
#' @param variables Model variables
graduate <- function(parameters, individuals, variables){
  function(api) {
    # Find children who have reached the maximum age
    to_graduate <- get_age(api, individuals, variables) == parameters$age_upper

    # Number graduating in this timestep
    n_graduate <- sum(to_graduate)

    # Save graduations
    api$render('graduation', n_graduate)

    # Replace graduating individuals
    if(n_graduate > 0){
      replace_child(api, which(to_graduate), individuals, variables, parameters)
    }
  }
}

#' Background mortality
#'
#' Replace a child who dies from the background mortality rate. This is currently
#' a constant probability estimated using the average_age parameter.
#'
#' @inheritParams graduate
background_mortality <- function(parameters, individuals, variables){
  function(api) {
    # Randomly draw background mortality
    background_death <- stats::rbinom(parameters$population, 1, rate_to_prob(1 / parameters$average_age))

    # Number dying from background mortality
    n_die <- sum(background_death)

    # Save deaths
    api$render('background_mortality', n_die)

    # Replace individuals who have died
    if(sum(n_die) > 0){
      replace_child(api, which(background_death == 1), individuals, variables, parameters)
    }
  }
}

#' Replace child with new
#'
#' @param api Model API
#' @param target Indices of children to replace
#' @inheritParams graduate
replace_child <- function(api, target, individuals, variables, parameters) {
  api$queue_variable_update(individuals$child, variables$birth_t, api$get_timestep() - parameters$age_lower, target)
  # Reset infection status
  api$queue_variable_update(individuals$child, variables$diarrhoea_status, 0, target)
  api$queue_variable_update(individuals$child, variables$diarrhoea_disease_index, 0, target)
  api$queue_variable_update(individuals$child, variables$diarrhoea_bacteria_prior, 0, target)
  api$queue_variable_update(individuals$child, variables$diarrhoea_virus_prior, 0, target)
  api$queue_variable_update(individuals$child, variables$diarrhoea_parasite_prior, 0, target)
  api$queue_variable_update(individuals$child, variables$diarrhoea_rotavirus_prior, 0, target)

  n <- length(target)
  # re-draw individual level heterogeneity
  new_het <- heterogeneity(n, parameters$het_sd)
  api$queue_variable_update(individuals$child, variables$het, new_het, target)

  # re-draw interventions and vaccination
  api$queue_variable_update(individuals$child, variables$llin, stats::rbinom(n, 1, parameters$llin_coverage), target)
  api$queue_variable_update(individuals$child, variables$rotavirus_vx, stats::rbinom(n, 1, parameters$rotavirus_vx_coverage), target)
  api$queue_variable_update(individuals$child, variables$pneumococcal_vx, stats::rbinom(n, 1, parameters$pneumococcal_vx_coverage), target)
  api$queue_variable_update(individuals$child, variables$hib_vx, stats::rbinom(n, 1, parameters$hib_vx_coverage), target)

  # TODO: Clear any scheduled disease progression

}

#' Get children's ages
#'
#' @param api Model API
#' @param index optionally return a subset of the variable vector
#' @inheritParams graduate
get_age <- function(api, individuals, variables, index = NULL){
  timestep <- api$get_timestep()
  timestep - api$get_variable(individuals$child, variables$birth_t, index)
}

#' Render demographic outputs
#'
#' Average ages and number of children in each year age-group.
#'
#' @inheritParams graduate
render_demography <- function(individuals, variables){
  function(api){
    age_days <- api$get_timestep() - api$get_variable(individuals$child, variables$birth_t)
    ages <- round(floor(age_days / 365))
    api$render("N", length(ages))
    api$render("age_0", sum(ages == 0))
    api$render("age_1", sum(ages == 1))
    api$render("age_2", sum(ages == 2))
    api$render("age_3", sum(ages == 3))
    api$render("age_4", sum(ages == 4))
    api$render("average_age", mean(age_days) / 365)
  }
}


