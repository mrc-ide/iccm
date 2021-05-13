#' Record prevalence
#'
#' Record overall (condition) and disaggregated (disease) prevalence
#'
#' @inheritParams condition_exposure
render_prevalence <- function(variables, renderer, parameters){
  function(timestep){
    for(disease in names(parameters$disease)){
      prevalence <- (parameters$population - variables[[paste0(disease, "_status")]]$get_index_of("uninfected")$size()) / parameters$population
      renderer$render(paste0(disease, "_prevalence"), prevalence, timestep)
    }
  }
}

#' Record average prior infections
#'
render_prior <- function(variables, renderer, parameters){
  function(timestep){
    for(disease in names(parameters$disease)){
      priors <- mean(variables[[paste0(disease, "_prior_exposure")]]$get_values())
      renderer$render(paste0(disease, "_prior_exposure"), priors, timestep)
    }
  }
}

render_fever_prevalence <- function(parameters, variables, renderer){
  function(timestep){
    fever_prev <- any_fever(parameters, variables)$size() / parameters$population
    renderer$render("fever_prevalence", fever_prev, timestep)
  }
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

#' Add default values to render variables that won't get called every timestep
#'
#' @param renderer Model renderer
initialise_render_defaults <- function(renderer, parameters){
  renderer$set_default("graduation", 0)
  renderer$set_default("hf_patients", 0)
  renderer$set_default("hf_severe_diarrhoea_tx", 0)
  renderer$set_default("hf_severe_malaria_tx", 0)
  renderer$set_default("hf_severe_pneumonia_tx", 0)
  renderer$set_default("hf_ors", 0)
  renderer$set_default("hf_act", 0)
  renderer$set_default("hf_amoxicillin", 0)
  renderer$set_default("chw_patients", 0)
  renderer$set_default("chw_referral", 0)
  renderer$set_default("chw_ors", 0)
  renderer$set_default("chw_act", 0)
  renderer$set_default("chw_amoxicillin", 0)
  renderer$set_default("chw_followup", 0)
  renderer$set_default("private_patients", 0)
  renderer$set_default("private_ors", 0)
  renderer$set_default("private_act", 0)
  renderer$set_default("private_amoxicillin", 0)
  for(disease in names(parameters$disease)){
    renderer$set_default(paste0(disease, "_clinical_infection"), 0)
    renderer$set_default(paste0(disease, "_severe_incidence"), 0)
    renderer$set_default(paste0(disease, "_death"), 0)
  }
  renderer$set_default("other_death", 0)
}
