#' Exposure process
#'
#' Governs exposure of all diseases.
#'
#' @param variables Model variables
#' @param parameters Model parameters
#' @param events Model events
#' @param renderer Model renderer
exposure <- function(variables, parameters, events, renderer){
  function(timestep){
    for(disease in 1:length(parameters$disease)){
      # Who will get exposed
      exposed <- variables$infection_status[[disease]]$get_index_of(c("uninfected", "asymptomatic"))
      # Estimate infection probabilities
      infection_prob <- infection_probability(
        target = exposed,
        disease = disease,
        parameters = parameters,
        variables = variables,
        timestep = timestep
      )
      # Draw those infected
      infected <- exposed$sample(infection_prob)
      # Infection
      if(infected$size() > 0){
        infection(
          infected = infected,
          disease = disease,
          parameters = parameters,
          variables = variables,
          events = events,
          renderer = renderer,
          timestep = timestep
        )
      }
    }
  }
}

#' Infection
#'
#' Infection of children (to asymptomatic or symptomatic states).
#'
#' @param infected Target bitset of those infected
#' @param disease Disease
#' @param parameters Model parameters
#' @param variables Model variables
#' @param events Model events
#' @param renderer Model renderer
#' @param timestep Model timestep
infection <- function(infected,
                      disease,
                      parameters,
                      variables,
                      events,
                      renderer,
                      timestep){


  if(parameters$disease[[disease]]$asymptomatic_pathway){
    infection_to_render <- 0
    # Currently uninfected
    currently_uninfected <- variables$infection_status[[disease]]$get_index_of(c("uninfected"))$and(infected)
    if(currently_uninfected$size() > 0){
      pi <- variables$prior_exposure[[disease]]$get_values(currently_uninfected)
      ci <- clinical_immunity(pi, parameters$disease[[disease]]$clinical_immunity_shape, parameters$disease[[disease]]$clinical_immunity_rate)
      clinical_index <- stats::runif(currently_uninfected$size()) < ci
      S_to_I <- individual::filter_bitset(currently_uninfected, which(clinical_index))
      S_to_A <- individual::filter_bitset(currently_uninfected, which(!clinical_index))
      events$clinical[[disease]]$schedule(S_to_I, delay = 0)
      infection_to_render <- infection_to_render + S_to_I$size()
      events$asymptomatic[[disease]]$schedule(S_to_A, delay = 0)
    }

    # Currently asymptomatically infected
    currently_asymptomatic <- variables$infection_status[[disease]]$get_index_of(c("asymptomatic"))$and(infected)
    if(currently_uninfected$size() > 0){
      pi <- variables$prior_exposure[[disease]]$get_values(currently_asymptomatic)
      ci <- clinical_immunity(pi, parameters$disease[[disease]]$clinical_immunity_shape, parameters$disease[[disease]]$clinical_immunity_rate)
      clinical_index <- stats::runif(currently_asymptomatic$size()) < ci
      A_to_I <- individual::filter_bitset(currently_asymptomatic, which(clinical_index))
      infection_to_render <- infection_to_render + A_to_I$size()
      events$clinical[[disease]]$schedule(A_to_I, delay = 0)
    }

    renderer$render(paste0(names(parameters$disease)[disease], "_clinical_infection"), infection_to_render, timestep)
  } else {
    events$clinical[[disease]]$schedule(infected, delay = 0)
    renderer$render(paste0(names(parameters$disease)[disease], "_clinical_infection"), infected$size() , timestep)
  }

  # Update record of prior exposure
  increment_counter(
    target = infected,
    variable = variables$prior_exposure[[disease]]
  )
}

#' Infection probability vector
#'
#' @param target Those exposed
#' @param disease disease index
#' @param parameters Model parameters
#' @param variables Model variables
#' @param timestep Timestep
infection_probability <- function(target,
                                  disease,
                                  parameters,
                                  variables,
                                  timestep){
  ### Infection immunity ###################################################
  # Childrens ages
  ages <- get_age(
    timestep = timestep,
    birth_t = variables$birth_t,
    target = target)
  # Childrens heterogeneity modifier
  het <- variables$heterogeneity$get_values(target)
  # Maternal immunity modifier
  mi <- maternal_immunity(
    age = ages,
    hl = parameters$disease[[disease]]$maternal_immunity_halflife
  )
  # Prior infections
  pi <- variables$prior_exposure[[disease]]$get_values(target)
  # Infection immunity modifier
  ii <- exposure_immunity(
    exposure = pi,
    shape = parameters$disease[[disease]]$infection_immunity_shape,
    rate = parameters$disease[[disease]]$infection_immunity_rate
  )
  # Vaccine modifier
  vi <- vaccine_impact(
    target = target,
    disease = disease,
    ages = ages,
    parameters = parameters,
    variables = variables
  )
  # LLIN modifier
  li <- llin_impact(
    target = target,
    disease = disease,
    parameters = parameters,
    variables = variables
  )
  # Treatment prophylaxis modifier
  tp <- treatment_prophylaxis(
    target = target,
    disease = disease,
    parameters = parameters,
    variables = variables,
    timestep = timestep
  )
  # Estimate infection rate
  infection_rate <- parameters$disease[[disease]]$sigma * mi * ii * het * vi * li * tp
  # Estimate infection probability
  infection_prob <- rate_to_prob(infection_rate)
  ##########################################################################
  return(infection_prob)
}

#' Increment an integer variable +1
#'
#' @param target Bitset of individuals
#' @param variable Variable to increment
increment_counter <- function(target, variable){
  current_prior <- variable$get_values(target)
  variable$queue_update(current_prior + 1, target)
}
