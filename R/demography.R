#' Graduation
#'
#' Replace a child who reaches the maximum age in the simulation with a new child.
#'
#' @param parameters Model parameters
#' @param variables Model variables
#' @param renderer Model renderer
graduate <- function(parameters, variables, renderer, events){
  function(timestep) {

    # Find children who have reached the maximum age
    to_graduate <- get_age(timestep, variables) == parameters$age_upper

    # Number graduating in this timestep
    n_graduate <- sum(to_graduate)
    # Save graduations
    renderer$render('graduation', n_graduate, timestep)

    # Replace graduating individuals
    if(n_graduate > 0){
      replace_child(timestep, variables, which(to_graduate), parameters, events)
    }
  }
}

#' Background mortality
#'
#' Replace a child who dies from the background mortality rate. This is currently
#' a constant probability estimated using the average_age parameter.
#'
#' @inheritParams graduate
background_mortality <- function(parameters, variables, renderer, events){
  function(timestep) {
    # Randomly draw background mortality
    background_death <- stats::rbinom(parameters$population, 1, rate_to_prob(1 / parameters$average_age))

    # Number dying from background mortality
    n_die <- sum(background_death)

    # Save deaths
    renderer$render('background_mortality', n_die, timestep)

    # Replace individuals who have died
    if(sum(n_die) > 0){
      replace_child(timestep, variables, which(background_death == 1), parameters, events)
    }
  }
}

#' Replace child with new
#'
#' @param timestep Current time
#' @param target Target indices
#' @param parameters Model parameters
#' @inheritParams graduate
replace_child <- function(timestep, variables, target, parameters, events) {
  variables$birth_t$queue_update(value = timestep - parameters$age_lower, index = target)
  # Reset infection status
  variables$dia_status$queue_update("S", target)
  variables$dia_type$queue_update("None", target)
  variables$dia_prior_bacteria$queue_update(0, target)
  variables$dia_prior_virus$queue_update(0, target)
  variables$dia_prior_parasite$queue_update(0, target)
  variables$dia_prior_rotavirus$queue_update(0, target)

  n <- length(target)
  # re-draw individual level heterogeneity
  new_het <- heterogeneity(n, parameters$het_sd)
  variables$het$queue_update(new_het, target)

  # re-draw interventions and vaccination
  variables$llin$queue_update(stats::rbinom(n, 1, parameters$llin_coverage), target)
  variables$rotavirus_vx$queue_update(stats::rbinom(n, 1, parameters$rotavirus_vx_coverage), target)
  variables$pneumococcal_vx$queue_update(stats::rbinom(n, 1, parameters$pneumococcal_vx_coverage), target)
  variables$hib_vx$queue_update(stats::rbinom(n, 1, parameters$hib_vx_coverage), target)

  # TODO: Clear any scheduled disease progression
  events$dia_recover$clear_schedule(target)
}

#' Get children's ages
#'
#' @param timestep Current time
#' @param index optionally return a subset of the variable vector
#' @inheritParams graduate
get_age <- function(timestep, variables, index = NULL){
  timestep - variables$birth_t$get_values(index = index)
}

#' Render demographic outputs
#'
#' Average ages and number of children in each year age-group.
#'
#' @inheritParams graduate
render_demography <- function(variables, renderer){
  function(timestep){
    age_days <- timestep - variables$birth_t$get_values()
    ages <- round(floor(age_days / 365))
    renderer$render("N", length(ages), timestep)
    renderer$render("age_0", sum(ages == 0), timestep)
    renderer$render("age_1", sum(ages == 1), timestep)
    renderer$render("age_2", sum(ages == 2), timestep)
    renderer$render("age_3", sum(ages == 3), timestep)
    renderer$render("age_4", sum(ages == 4), timestep)
    renderer$render("average_age", mean(age_days) / 365, timestep)
  }
}


