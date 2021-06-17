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
    n_amoxicillin_given <- 0
    # Clear any duplicate future tx scheduling
    clear_scheduled_treatment_visits(target, events)

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
    dia_severe_dx <- severe_diagnosis(target,
                                      "diarrhoea",
                                      sens = parameters$chw$severe_diarrhoea_sensitivity,
                                      spec = parameters$chw$severe_diarrhoea_specificity,
                                      variables,
                                      parameters)
    dia_long_symptoms <- long_symptoms(target, "diarrhoea", parameters$chw$diarrhoea_long_symptoms, timestep, variables, parameters)
    dia_severe_to_refer <- dia_severe_dx$or(dia_long_symptoms)
    dia_severe_to_refer <- dia_severe_to_refer$sample(parameters$chw$efficacy)
    if(dia_severe_to_refer$size() > 0){
      give_ors(dia_severe_to_refer, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_severe_to_refer$size()
    }

    # Malaria
    malaria_severe_dx <- severe_diagnosis(target,
                                          "malaria",
                                          sens = parameters$chw$severe_malaria_sensitivity,
                                          spec = parameters$chw$severe_malaria_specificity,
                                          variables,
                                          parameters)
    malaria_long_symptoms <- long_symptoms(target, "malaria", parameters$chw$malaria_long_symptoms, timestep, variables, parameters)
    malaria_severe_to_refer <- malaria_severe_dx$or(malaria_long_symptoms)
    malaria_severe_to_refer <- malaria_severe_to_refer$sample(parameters$chw$efficacy)
    if(malaria_severe_to_refer$size() > 0){
      give_act(malaria_severe_to_refer, parameters, variables, events, timestep)
      n_act_given <- n_act_given + malaria_severe_to_refer$size()
    }

    # Pneumonia
    pneumonia_severe_dx <- severe_diagnosis(target,
                                            "pneumonia",
                                            sens = parameters$chw$severe_pneumonia_sensitivity,
                                            spec = parameters$chw$severe_pneumonia_specificity,
                                            variables,
                                            parameters)
    pneumonia_long_symptoms <- long_symptoms(target, "pneumonia", parameters$chw$pneumonia_long_symptoms, timestep, variables, parameters)
    pneumonia_severe_to_refer <- pneumonia_severe_dx$or(pneumonia_long_symptoms)
    pneumonia_severe_to_refer <- pneumonia_severe_to_refer$sample(parameters$chw$efficacy)
    if(pneumonia_severe_to_refer$size() > 0){
      give_amoxicillin(pneumonia_severe_to_refer, parameters, variables, events, timestep)
      n_amoxicillin_given <- n_amoxicillin_given + pneumonia_severe_to_refer$size()
    }
    ############################################################################

    ### Referral ###############################################################
    to_refer <- dia_severe_to_refer$or(malaria_severe_to_refer)$or(pneumonia_severe_to_refer)
    if(to_refer$size() > 0){
      if(parameters$hf$hf){
        renderer$render("chw_referral", to_refer$size(), timestep)
        events$hf_treatment$schedule(to_refer, delay = parameters$hf$travel_time + 1)
      }
    }
    ############################################################################

    ### Non-severe illness ######################################################
    # Remaining children who have not been treated for severe disease
    target <- target$set_difference(to_refer)

    if(target$size() > 0){
      # Diarrhoea
      dia_to_treat <- diagnosis(target,
                                "diarrhoea",
                                sens = parameters$chw$diarrhoea_sensitivity,
                                spec = parameters$chw$diarrhoea_specificity,
                                variables,
                                parameters)
      dia_to_treat <- dia_to_treat$sample(parameters$chw$efficacy)
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
      malaria_to_treat <- malaria_to_treat$sample(parameters$chw$efficacy)
      if(malaria_to_treat$size() > 0){
        give_act(malaria_to_treat, parameters, variables, events, timestep)
        n_act_given <- n_act_given + malaria_to_treat$size()
      }

      # Pneumonia
      # Those who test +ve for malaria are not treated for pneumonia
      target <- target$set_difference(malaria_to_treat)
      pneumonia_to_treat <- diagnosis(target,
                                      "pneumonia",
                                      sens = parameters$chw$pneumonia_sensitivity,
                                      spec = parameters$chw$pneumonia_specificity,
                                      variables,
                                      parameters)
      pneumonia_to_treat <- pneumonia_to_treat$sample(parameters$chw$efficacy)
      if(pneumonia_to_treat$size() > 0){
        give_amoxicillin(pneumonia_to_treat, parameters, variables, events, timestep)
        n_amoxicillin_given <- n_amoxicillin_given + pneumonia_to_treat$size()
      }
    }
    ############################################################################

    ### Record activity ########################################################
    renderer$render("chw_followup", on_followup$size(), timestep)
    renderer$render("chw_referral", to_refer$size(), timestep)
    renderer$render("chw_ors", n_ors_given, timestep)
    renderer$render("chw_act", n_act_given, timestep)
    renderer$render("chw_amoxicillin", n_amoxicillin_given, timestep)
    ############################################################################
  }
}

#' CHW malaria diagnosis
#'
#' Accounts for the diagnostic a CHW uses as well as general human error
#'
#' @param target Children presenting at the CHW
#' @param variables Model variables
#' @param parameters Model parameters
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
chw_diagnose_malaria <- function(target, variables, parameters){
  disease_index = type_index(parameters, "malaria")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$dx_tx$rdt_sensitivity,
    spec = parameters$dx_tx$rdt_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$chw$efficacy)
  return(diagnosed)
}

#' CHW facility severe malaria diagnosis
#'
#' Accounts for the diagnostic a health facility uses and long-symptom diagnosis
#' as well as general human error
#'
#' @inheritParams chw_diagnose_malaria
#' @param timestep Timestep
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
chw_diagnose_severe_malaria <- function(target, variables, parameters, timestep){
  disease_index = type_index(parameters, "malaria")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$chw$severe_malaria_sensitivity,
    spec = parameters$chw$severe_malaria_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed_long_symptoms <- long_symptoms(
    target = target,
    disease_index = disease_index,
    threshold = parameters$chw$malaria_long_symptoms,
    timestep = timestep,
    variables = variables,
    parameters = parameters
  )
  diagnosed <- diagnosed$or(diagnosed_long_symptoms)$sample(parameters$chw$efficacy)
  return(diagnosed)
}

#' CHW diarrhoea diagnosis
#'
#' Accounts for the diagnostic a CHW uses as well as general human error
#'
#' @inheritParams chw_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
chw_diagnose_diarrhoea <- function(target, variables, parameters){
  disease_index = type_index(parameters, "diarrhoea")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$chw$diarrhoea_sensitivity,
    spec = parameters$chw$diarrhoea_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$chw$efficacy)
  return(diagnosed)
}

#' CHW facility severe diarrhoea diagnosis
#'
#' Accounts for the diagnostic a health facility uses and long-symptom diagnosis
#' as well as general human error
#'
#' @inheritParams chw_diagnose_malaria
#' @param timestep Timestep
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
chw_diagnose_severe_diarrhoea <- function(target, variables, parameters, timestep){
  disease_index = type_index(parameters, "diarrhoea")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$chw$severe_diarrhoea_sensitivity,
    spec = parameters$chw$severe_diarrhoea_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed_long_symptoms <- long_symptoms(
    target = target,
    disease_index = disease_index,
    threshold = parameters$chw$diarrhoea_long_symptoms,
    timestep = timestep,
    variables = variables,
    parameters = parameters
  )
  diagnosed <- diagnosed$or(diagnosed_long_symptoms)$sample(parameters$chw$efficacy)
  return(diagnosed)
}

#' CHW pneumonia diagnosis
#'
#' Accounts for the diagnostic a CHW uses as well as general human error
#'
#' @inheritParams chw_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
chw_diagnose_pneumonia <- function(target, variables, parameters){
  disease_index = type_index(parameters, "pneumonia")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$chw$pneumonia_sensitivity,
    spec = parameters$chw$pneumonia_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$chw$efficacy)
  return(diagnosed)
}

#' CHW facility severe pneumonia diagnosis
#'
#' Accounts for the diagnostic a health facility uses and long-symptom diagnosis
#' as well as general human error
#'
#' @inheritParams chw_diagnose_malaria
#' @param timestep Timestep
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
chw_diagnose_severe_pneumonia <- function(target, variables, parameters, timestep){
  disease_index = type_index(parameters, "pneumonia")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$chw$severe_pneumonia_sensitivity,
    spec = parameters$chw$severe_pneumonia_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed_long_symptoms <- long_symptoms(
    target = target,
    disease_index = disease_index,
    threshold = parameters$chw$pneumonia_long_symptoms,
    timestep = timestep,
    variables = variables,
    parameters = parameters
  )
  diagnosed <- diagnosed$or(diagnosed_long_symptoms)$sample(parameters$chw$efficacy)
  return(diagnosed)
}
