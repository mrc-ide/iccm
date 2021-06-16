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
        p = parameters$disease[[disease]],
        prior_exposure = variables$prior_exposure[[disease]],
        heterogeneity = variables$het,
        birth_t = variables$birth_t,
        timestep = timestep
      )
      # Draw those infected
      infected <- exposed$sample(infection_prob)
      # Infection
      if(infected$size() > 0){
        infection(
          infected = infected,
          p = parameters$disease[[disease]],
          prior_exposure = variables$prior_exposure[[disease]],
          infection_status = variables$infection_status[[disease]],
          clinical = events$clinical[[disease]],
          asymptomatic = events$asymptomatic[[disease]],
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
                      p,
                      prior_exposure,
                      infection_status,
                      clinical,
                      asymptomatic,
                      renderer,
                      timestep){
  # Update record of prior exposure
  increment_counter(target = infected, variable = prior_exposure)

  if(p$asymptomatic_pathway){
    infection_to_render <- 0
    # Currently uninfected
    currently_uninfected <- infection_status$get_index_of(c("uninfected"))$and(infected)
    if(currently_uninfected$size() > 0){
      pi <- prior_exposure$get_values(currently_uninfected)
      ci <- clinical_immunity(pi, p$clinical_immunity_shape, p$clinical_immunity_rate)
      clinical_index <- stats::runif(currently_uninfected$size()) < ci
      S_to_I <- individual::filter_bitset(currently_uninfected, which(clinical_index))
      S_to_A <- individual::filter_bitset(currently_uninfected, which(!clinical_index))
      clinical$schedule(S_to_I, delay = 0)
      infection_to_render <- infection_to_render + S_to_I$size()
      asymptomatic$schedule(S_to_A, delay = 0)
    }

    # Currently asymptomatically infected
    currently_asymptomatic <- infection_status$get_index_of(c("asymptomatic"))$and(infected)
    if(currently_uninfected$size() > 0){
      pi <- prior_exposure$get_values(currently_asymptomatic)
      ci <- clinical_immunity(pi, p$clinical_immunity_shape, p$clinical_immunity_rate)
      clinical_index <- stats::runif(currently_asymptomatic$size()) < ci
      A_to_I <- individual::filter_bitset(currently_asymptomatic, which(clinical_index))
      infection_to_render <- infection_to_render + A_to_I$size()
      clinical$schedule(A_to_I, delay = 0)
    }

    renderer$render(paste0(disease, "_clinical_infection"), infection_to_render, timestep)
  } else {
    clinical$schedule(infected, delay = 0)
    renderer$render(paste0(disease, "_clinical_infection"), infected$size() , timestep)
  }
}

#' Infection probability vector
#'
#' @param target Those exposed
#' @param p Disease parameters
#' @param prior_exposure Prior exposure to disease
#' @param heterogeneity Individual heterogeneity
#' @param birth_t Birth times
#' @param timestep Timestep
infection_probability <- function(target,
                                  p,
                                  prior_exposure,
                                  heterogeneity,
                                  birth_t,
                                  timestep){
  ### Infection immunity ###################################################
  ages <- get_age(timestep, birth_t, target)
  het <- heterogeneity$get_values(target)
  # Maternal immunity modifier
  mi <- maternal_immunity(ages, p$maternal_immunity_halflife)
  # Prior infections
  pi <- prior_exposure$get_values(target)
  # Infection immunity modifier
  ii <- exposure_immunity(pi, p$infection_immunity_shape, p$infection_immunity_rate)
  # Vaccine modifier
  vi <- vaccine_impact(disease = disease, target = target, ages = ages, p = p, variables = variables)
  # LLIN modifier
  li <- llin_impact(disease = disease, target = target, p = p, variables = variables)
  # Community impacts modifier (vaccine or LLIN)
  #ci <- community_impact(disease = disease, index = i, p = p)
  # Treatment prophylaxis modifier
  #tp <- treatment_prophylaxis(time_since_treatment, p$prophylaxis_hl[i])
  # Estimate infection rate
  infection_rate <- p$sigma * mi * ii * het * vi * li# * ci * tp
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
