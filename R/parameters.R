#' @title Get model parameters
#' @description
#' Create a named list of parameters for use in the model.
#'
#' @param user_overwrite User overwrites to default parameters
#'
#' The parameters are defined below.
#'
#' \strong{Demography:}
#'
#' * pop - Population size
#' * average_age - The average lifespan of an individual within the population
#' * age_upper - The upper limit to modelled ages
#'
get_parameters <- function(user_overwrite = NULL){

  parameters <- list(
    # Demography
    population = 1000,
    average_age = 60 * 365,
    age_lower = 30,
    age_upper = 5 * 365 - 1,
    # Epidemiology
    het_sd = 0,#1.6,
    # Interventions
    llin_coverage = 0.5,
    rotavirus_vx_coverage = 0,
    pneumococcal_vx_coverage = 0.1,
    hib_vx_coverage = 0.1
  )

  parameters$dia <- list(
    groups = 4,
    type = c("bacteria", "virus", "parasite", "rotavirus"),
    index = c(1, 2, 3, 4),
    sigma = c(0.005, 0.005, 0.005, 0.005),
    # Average duration of clinical episode
    clin_dur = c(14, 14, 14, 14),
    daily_prob_severe = c(0.1, 0, 0, 0),
    daily_prob_death = c(0.01, 0, 0, 0),
    # Maternal immunity
    ## Half life
    mi_hl = c(100, 100, 100, 100),
    # Infection immunity
    ## shape
    ii_shape = c(5, 5, 5, 5),
    ## rate
    ii_rate = c(1, 1, 1, 1),
    # Vaccination
    vx_start = c(0, 0, 0, 0),
    vx_initial_efficacy = c(0, 0, 0, 0),
    vx_hl = c(0, 0, 0, 0),
    # TODO this should be a function of coverage:
    vx_ci = c(0, 0, 0, 0)
  )

  # Overwrite_defaults
  if(!is.null(user_overwrite)){
    parameters <- overwrite_params(parameters, user_overwrite)
  }

  return(parameters)
}

#' Extract names from nested list
#'
#' @param x List
#'
#' @return Names from nested list. Levels are separated by ".".
nested_list_names <- function(x){
  names(rapply(x, function(y)  utils::head(y, 1)))
}

#' Overwrite default parameters
#'
#' @param parameters Default parameters
#' @param user_overwrite User input (nested) list of parameters
#'
#' @return Set of user-modified simulation parameters
overwrite_params <- function(parameters, user_overwrite){
  stopifnot(is.list(user_overwrite))
  # Check user input parameter names are all recognised
  missing <- setdiff(nested_list_names(user_overwrite), nested_list_names(parameters))
  if(length(missing) > 0){
    missing_names <- gsub("[.]", "$", missing)
    stop("User input parameter(s) not recognised: ", paste(missing_names, collapse = " "))
  }
  # Overwrite defaults
  parameters <- utils::modifyList(parameters, user_overwrite)
  return(parameters)
}
