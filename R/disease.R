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
    for(disease in names(parameters$disease)){
      exposed <- variables[[paste0(disease, "_status")]]$get_index_of(c("uninfected", "asymptomatic"))
      infection_prob <- infection_probability(disease, exposed, parameters$disease[[disease]], timestep, variables)

      # Draw those infected
      infected <- exposed$sample(infection_prob)
      if(infected$size() > 0){
        infection(infected, disease, parameters, variables, events, renderer, timestep)
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
infection <- function(infected, disease, parameters, variables, events, renderer, timestep){
  # Update record of prior exposure
  increment_counter(target = infected, variable = variables[[paste0(disease, "_prior_exposure")]])

  if(parameters$disease[[disease]]$asymptomatic_pathway){
    infection_to_render <- 0
    # Currently uninfected
    currently_uninfected <- variables[[paste0(disease, "_status")]]$get_index_of(c("uninfected"))$and(infected)
    if(currently_uninfected$size() > 0){
      pi <- variables[[paste0(disease, "_prior_exposure")]]$get_values(currently_uninfected)
      ci <- clinical_immunity(pi, parameters$disease[[disease]]$clinical_immunity_shape, parameters$disease[[disease]]$clinical_immunity_rate)
      clinical_index <- stats::runif(currently_uninfected$size()) < ci
      S_to_I <- individual::filter_bitset(currently_uninfected, which(clinical_index))
      S_to_A <- individual::filter_bitset(currently_uninfected, which(!clinical_index))
      events[[paste0(disease, "_progress_to_clinical_infection")]]$schedule(S_to_I, delay = 0)
      infection_to_render <- infection_to_render + S_to_I$size()
      events[[paste0(disease, "_progress_to_asymptomatic_infection")]]$schedule(S_to_A, delay = 0)
    }

    # Currently asymptomatically infected
    currently_asymptomatic <- variables[[paste0(disease, "_status")]]$get_index_of(c("asymptomatic"))$and(infected)
    if(currently_uninfected$size() > 0){
      pi <- variables[[paste0(disease, "_prior_exposure")]]$get_values(currently_asymptomatic)
      ci <- clinical_immunity(pi, parameters$disease[[disease]]$clinical_immunity_shape, parameters$disease[[disease]]$clinical_immunity_rate)
      clinical_index <- stats::runif(currently_asymptomatic$size()) < ci
      A_to_I <- individual::filter_bitset(currently_asymptomatic, which(clinical_index))
      infection_to_render <- infection_to_render + A_to_I$size()
      events[[paste0(disease, "_progress_to_clinical_infection")]]$schedule(A_to_I, delay = 0)
    }

    renderer$render(paste0(disease, "_clinical_infection"), infection_to_render, timestep)
  } else {
    events[[paste0(disease, "_progress_to_clinical_infection")]]$schedule(infected, delay = 0)
    renderer$render(paste0(disease, "_clinical_infection"),infected$size() , timestep)
  }
}

#' Infection probability vector
#'
#' @param disease Disease
#' @param target Those exposed
#' @param p Disease parameters
#' @param timestep Model timestep
#' @param variables Model variables
infection_probability <- function(disease, target, p, timestep, variables){
  ### Infection immunity ###################################################
  ages <- get_age(timestep, variables, target)
  het <- variables$het$get_values(target)
  # Maternal immunity modifier
  mi <- maternal_immunity(ages, p$maternal_immunity_halflife)
  # Prior infections
  pi <- variables[[paste0(disease, "_prior_exposure")]]$get_values(target)
  # Infection immunity modifier
  ii <- exposure_immunity(pi, p$infection_immunity_shape, p$infection_immunity_rate)
  # Vaccine modifier
  vi <- rep(1, target$size())
  if(p$vaccine){
    vi <- vaccine_impact(disease = disease, index = i, target = target, ages = ages, p = p, variables = variables)
  }
  # LLIN modifier
  li <- rep(1, target$size())
  if(disease == "plasmodium_falciparum"){
    li <- llin_impact(disease = disease, target = target, p = p, variables = variables)
  }
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
