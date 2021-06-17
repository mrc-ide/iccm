#' Sample provider preference
#'
#' Used to select the provider preference assigned to each child at birth. Provider
#' choice can be weighted. Providers are currently: HF, CHW or Private. If no providers
#' are available preference is set to "None".
#'
#' @param n Number of children
#' @param parameters Model parameters
#'
#' @return Vector of provider preference for each child
sample_preference <- function(n, parameters){
  stopifnot(n > 0)
  providers <- c(parameters$hf$hf, parameters$chw$chw, parameters$private$private)
  if(all(providers == 0)){
    return(rep("none", n))
  }
  provider_weights <- parameters$treatment_seeking$provider_preference_weights
  prob <- providers * provider_weights
  prob <- prob / sum(prob)
  preference <- sample(c("hf", "chw", "private"), n, replace = TRUE, prob = prob)
  return(preference)
}


#' Clear any scheduled treatment visits
#'
#' @param target Children
#' @param events Model events
clear_scheduled_treatment_visits <- function(target, events){
  events$hf_treatment$clear_schedule(target)
  events$chw_treatment$clear_schedule(target)
  events$private_treatment$clear_schedule(target)
}





#' Private treatment
#'
#' Treats children coming to a private provider.
#'
#' @param variables Model variables
#' @param parameters Model parameters
#' @param renderer Model renderer
#' @param events Model events
private_treat <- function(variables, parameters, renderer, events){
  function(timestep, target){
    renderer$render("private_patients", target$size(), timestep)
    n_ors_given <- 0
    n_act_given <- 0
    n_amoxicillin_given <- 0
    # Clear any duplicate future tx scheduling
    clear_scheduled_treatment_visits(target, events)

    ### Non-severe illness ######################################################
    # Diarrhoea
    dia_to_treat <- diagnosis(target,
                              "diarrhoea",
                              sens = parameters$private$diarrhoea_sensitivity,
                              spec = parameters$private$diarrhoea_specificity,
                              variables,
                              parameters)
    dia_to_treat <- dia_to_treat$sample(parameters$private$efficacy)
    if(dia_to_treat$size() > 0){
      give_ors(dia_to_treat, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_to_treat$size()
    }

    # Malaria
    malaria_to_treat <- diagnosis(target,
                                  "malaria",
                                  sens = parameters$dx_tx$rdt_sensitivity,
                                  spec = parameters$dx_tx$rdt_specificity,
                                  variables,
                                  parameters)
    #malaria_to_treat <- malaria_to_treat$and(any_fever(parameters, variables))
    malaria_to_treat <- malaria_to_treat$sample(parameters$private$efficacy)
    if(malaria_to_treat$size() > 0){
      give_act(malaria_to_treat, parameters, variables, events, timestep)
      n_act_given <- n_act_given + malaria_to_treat$size()
    }

    # Pneumonia
    # Those who test +ve for malaria are not treated for pneumonia
    target <- target$set_difference(malaria_to_treat)
    pneumonia_to_treat <- diagnosis(target,
                                    "pneumonia",
                                    sens = parameters$private$pneumonia_sensitivity,
                                    spec = parameters$private$pneumonia_specificity,
                                    variables,
                                    parameters)
    pneumonia_to_treat <- pneumonia_to_treat$sample(parameters$private$efficacy)
    if(pneumonia_to_treat$size() > 0){
      give_amoxicillin(pneumonia_to_treat, parameters, variables, events, timestep)
      n_amoxicillin_given <- n_amoxicillin_given + pneumonia_to_treat$size()
    }
    ############################################################################

    ### Record activity ########################################################
    renderer$render("private_ors", n_ors_given, timestep)
    renderer$render("private_act", n_act_given, timestep)
    renderer$render("private_amoxicillin", n_amoxicillin_given, timestep)
    ############################################################################
  }
}
