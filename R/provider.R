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

#' Health Facility treatment
#'
#' Treats children coming to the health facility. Severe cases take precedent, other
#' outcomes depend on diagnostics and treatments.
#'
#' @param variables Model variables
#' @param parameters Model parameters
#' @param renderer Model renderer
#' @param events Model events
hf_treat <- function(variables, parameters, renderer, events){
  function(timestep, target){
    renderer$render("hf_patients", target$size(), timestep)
    n_ors_given <- 0
    n_act_given <- 0
    # Clear any duplicate future tx scheduling
    events$hf_treatment$clear_schedule(target)

    ### Severe illness #########################################################
    # Diarrhoea
    dia_severe_to_treat <- dx(target,
                              status_variable = variables$dia_status,
                              sens = parameters$hf$severe_diarrhoea_sensitivity,
                              spec = parameters$hf$severe_diarrhoea_specificity,
                              positive = 3, negative = 0:2)
    dia_severe_to_treat <- dia_severe_to_treat$sample(parameters$hf$efficacy)
    give_severe_treatment_diarrhoea(dia_severe_to_treat, parameters, variables, events, timestep)

    # Malaria
    malaria_severe_to_treat <- dx(target,
                                  status_variable = variables$malaria_status,
                                  sens = parameters$hf$severe_malaria_sensitivity,
                                  spec = parameters$hf$severe_malaria_specificity,
                                  positive = 3, negative = 0:2)
    malaria_severe_to_treat <- malaria_severe_to_treat$sample(parameters$hf$efficacy)
    give_severe_treatment_malaria(malaria_severe_to_treat, parameters, variables, events, timestep)
    ############################################################################

    ### Non-severe illness ######################################################
    # Remaining children who have not been treated for severe disease
    target <- target$set_difference(dia_severe_to_treat)$set_difference(malaria_severe_to_treat)

    if(target$size() > 0){
      # Diarrhoea
      dia_to_treat <- dx(target,
                         status_variable = variables$dia_status,
                         sens = parameters$hf$diarrhoea_sensitivity,
                         spec = parameters$hf$diarrhoea_specificity,
                         positive = 1:3, negative = 0)
      dia_to_treat <- dia_to_treat$sample(parameters$hf$efficacy)
      give_ors(dia_to_treat, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_to_treat$size()

      # Malaria
      malaria_to_treat <- dx(target,
                             status_variable = variables$malaria_status,
                             sens = parameters$dx_tx$rdt_sensitivity,
                             spec = parameters$dx_tx$rdt_specificity,
                             positive = 1:3, negative = 0)
      malaria_to_treat <- malaria_to_treat$and(any_fever(variables))
      malaria_to_treat <- malaria_to_treat$sample(parameters$hf$efficacy)
      give_act(malaria_to_treat, parameters, variables, events, timestep)
      n_act_given <- n_act_given + malaria_to_treat$size()

      # Those who test +ve for malaria are not treated for pneumonia
      target <- target$set_difference(malaria_to_treat)

      # Pneumonia


    }
    ############################################################################

    ### Record activity ########################################################
    renderer$render("hf_severe_diarrhoea_tx", dia_severe_to_treat$size(), timestep)
    renderer$render("hf_severe_malaria_tx", malaria_severe_to_treat$size(), timestep)
    renderer$render("hf_ors", n_ors_given, timestep)
    renderer$render("hf_act", n_act_given, timestep)
    ############################################################################
  }
}

#' Community Health Worker treatment
#'
#' Treats children coming to a CHW. Severe cases take precedent and are referred if detected, other
#' outcomes depend on protocol, diagnostics and treatments.
#'
#' @param variables Model variables
#' @param parameters Model parameters
#' @param renderer Model renderer
#' @param events Model events
chw_treat <- function(variables, parameters, renderer, events){
  function(timestep, target){
    renderer$render("chw_patients", target$size(), timestep)
    n_ors_given <- 0
    n_act_given <- 0
    # Clear any duplicate future tx scheduling
    events$chw_treatment$clear_schedule(target)

    ### Follow up ##############################################################
    on_followup <- variables$awaiting_followup$get_index_of(set = 1)$and(target)
    to_followup <- variables$awaiting_followup$get_index_of(set = 0)$and(target)
    # Schedule future follow ups
    if(to_followup$size() > 0){
      events$chw_treatment$schedule(to_followup, delay = parameters$chw$followup_period)
      variables$awaiting_followup$queue_update(1, to_followup)
    }
    variables$awaiting_followup$queue_update(0, on_followup)
    ############################################################################

    ### Severe illness #########################################################
    # Diarrhoea
    dia_severe_to_dx <- dx(target,
                           status_variable = variables$dia_status,
                           sens = parameters$chw$severe_diarrhoea_sensitivity,
                           spec = parameters$chw$severe_diarrhoea_specificity,
                           positive = 3, negative = 0:2)
    dia_long_symptoms <- long_symptoms(variables$dia_symptom_start, target, parameters$dia$symptom_time_refer, timestep)
    dia_severe_to_refer <- dia_severe_to_dx$or(dia_long_symptoms)$sample(parameters$hf$efficacy)
    give_ors(dia_severe_to_refer, parameters, variables, events, timestep)
    n_ors_given <- n_ors_given + dia_severe_to_refer$size()

    # Malaria
    malaria_severe_to_dx <- dx(target,
                               status_variable = variables$malaria_status,
                               sens = parameters$chw$severe_malaria_sensitivity,
                               spec = parameters$chw$severe_malaria_specificity,
                               positive = 3, negative = 0:2)
    malaria_long_symptoms <- long_symptoms(variables$malaria_symptom_start, target, parameters$malaria$symptom_time_refer, timestep)
    malaria_severe_to_refer <- malaria_severe_to_dx$or(malaria_long_symptoms)$sample(parameters$hf$efficacy)
    give_act(malaria_severe_to_refer, parameters, variables, events, timestep)
    n_act_given <- n_act_given + malaria_severe_to_refer$size()

    # Pneumonia
    ############################################################################

    ### Referral ###############################################################
    to_refer <- dia_severe_to_refer$copy()$or(malaria_severe_to_refer)
    if(to_refer$size() > 0){
      if(parameters$hf$hf){
        renderer$render("chw_referral", to_refer$size(), timestep)
        events$hf_treatment$schedule(to_refer, delay = parameters$hf$travel_time + 1)
      }
    }
    ############################################################################

    ### Non-severe illness ######################################################
    # Remaining children who have not been treated for severe disease
    target <- target$set_difference(dia_severe_to_refer)$set_difference(malaria_severe_to_refer)

    if(target$size() > 0){
      # Diarrhoea
      dia_to_treat <- dx(target,
                         status_variable = variables$dia_status,
                         sens = parameters$chw$diarrhoea_sensitivity,
                         spec = parameters$chw$diarrhoea_specificity,
                         positive = 1:3, negative = 0)
      dia_to_treat <- dia_to_treat$sample(parameters$chw$efficacy)
      give_ors(dia_to_treat, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_to_treat$size()

      # Malaria
      malaria_to_treat <- dx(target,
                             status_variable = variables$malaria_status,
                             sens = parameters$dx_tx$rdt_sensitivity,
                             spec = parameters$dx_tx$rdt_specificity,
                             positive = 1:3, negative = 0)
      malaria_to_treat <- malaria_to_treat$and(any_fever(variables))
      malaria_to_treat <- malaria_to_treat$sample(parameters$chw$efficacy)
      give_act(malaria_to_treat, parameters, variables, events, timestep)
      n_act_given <- n_act_given + malaria_to_treat$size()

      # Those who test +ve for malaria are not treated for pneumonia
      target <- target$set_difference(malaria_to_treat)

      # Pneumonia

    }
    ############################################################################

    ### Record activity ########################################################
    renderer$render("chw_ors", n_ors_given, timestep)
    renderer$render("chw_act", n_act_given, timestep)
    renderer$render("chw_followup", on_followup$size(), timestep)
    ############################################################################
  }
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
    # Clear any duplicate future tx scheduling
    events$private_treatment$clear_schedule(target)

    ### Non-severe illness ######################################################
    # Diarrhoea
    dia_to_treat <- dx(target,
                       status_variable = variables$dia_status,
                       sens = parameters$private$diarrhoea_sensitivity,
                       spec = parameters$private$diarrhoea_specificity,
                       positive = 1:3, negative = 0)
    dia_to_treat <- dia_to_treat$sample(parameters$private$efficacy)
    give_ors(dia_to_treat, parameters, variables, events, timestep)
    n_ors_given <- n_ors_given + dia_to_treat$size()

    # Malaria
    malaria_to_treat <- dx(target,
                           status_variable = variables$malaria_status,
                           sens = parameters$dx_tx$rdt_sensitivity,
                           spec = parameters$dx_tx$rdt_specificity,
                           positive = 1:3, negative = 0)
    malaria_to_treat <- malaria_to_treat$and(any_fever(variables))
    malaria_to_treat <- malaria_to_treat$sample(parameters$private$efficacy)
    give_act(malaria_to_treat, parameters, variables, events, timestep)
    n_act_given <- n_act_given + malaria_to_treat$size()

    # Those who test +ve for malaria are not treated for pneumonia
    target <- target$set_difference(malaria_to_treat)

    # Pneumonia

    ############################################################################

    ### Record activity ########################################################
    renderer$render("private_ors", n_ors_given, timestep)
    renderer$render("private_act", n_act_given, timestep)
    ############################################################################
  }
}
