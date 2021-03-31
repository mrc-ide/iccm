#' Background mortality
#'
#' Replace a child who dies from the background mortality rate. This is currently
#' a constant probability estimated using the average_age parameter.
#'
#' @param parameters Model parameters
#' @param variables Model variables
#' @param renderer Model renderer
#' @param events Model events
background_mortality <- function(parameters, variables, renderer, events){
  function(timestep) {

    died <- individual::Bitset$new(parameters$population)
    # Randomly draw background mortality
    to_die <- sample.int(parameters$population, stats::rbinom(1, parameters$population, rate_to_prob(1 / parameters$average_age)))
    died$insert(to_die)

    # Number dying from background mortality
    n_die <- died$size()

    # Output deaths
    renderer$render('background_mortality', n_die, timestep)

    # Replace individuals who have died
    if(sum(n_die) > 0){
      replace_child(died, timestep, variables, parameters, events)
    }
  }
}

#' Replace child with new
#'
#' @param timestep Current time
#' @param target Target indices
#' @param parameters Model parameters
#' @inheritParams background_mortality
replace_child <- function(target, timestep, variables, parameters, events) {
  variables$birth_t$queue_update(value = timestep - parameters$age_lower, index = target)
  graduate_t <- parameters$age_upper - parameters$age_lower
  events$graduate$clear_schedule(target)
  events$graduate$schedule(target, delay = rep(graduate_t, target$size()))

  # Reset infection status
  variables$dia_status$queue_update(0, target)
  variables$dia_disease$queue_update(0, target)
  variables$dia_symptom_start$queue_update(NA, target)
  variables$dia_fever$queue_update(0, target)
  variables$dia_prior_bacteria$queue_update(0, target)
  variables$dia_prior_virus$queue_update(0, target)
  variables$dia_prior_parasite$queue_update(0, target)
  variables$dia_prior_rotavirus$queue_update(0, target)

  variables$malaria_status$queue_update(0, target)
  variables$malaria_disease$queue_update(0, target)
  variables$malaria_symptom_start$queue_update(NA, target)
  variables$malaria_fever$queue_update(0, target)
  variables$malaria_prior_pf$queue_update(0, target)

  n <- target$size()
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
  events$dia_asymptomatic$clear_schedule(target)
  events$malaria_recover$clear_schedule(target)
  events$malaria_asymptomatic$clear_schedule(target)
}

#' Get children's ages
#'
#' @param timestep Current time
#' @param index optionally return a subset of the variable vector
#' @inheritParams background_mortality
get_age <- function(timestep, variables, index = NULL){
  timestep - variables$birth_t$get_values(index = index)
}

#' Render demographic outputs
#'
#' Average ages and number of children in each year age-group.
#'
#' @inheritParams background_mortality
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


