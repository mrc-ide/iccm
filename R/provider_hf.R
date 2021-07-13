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
    n_amoxicillin_given <- 0
    # Clear any duplicate future tx scheduling
    clear_scheduled_treatment_visits(target, events)

    ### Severe illness #########################################################
    # Diarrhoea
    dia_severe_to_treat <- hf_diagnose_severe_diarrhoea(
      target = target,
      variables = variables,
      parameters = parameters
    )
    if(dia_severe_to_treat$size() > 0){
      hf_treat_severe_diarrhoea(
        target = dia_severe_to_treat,
        variables = variables,
        parameters = parameters,
        events = events
      )
    }

    # Malaria
    malaria_severe_to_treat <- hf_diagnose_severe_malaria(
      target = target,
      variables = variables,
      parameters = parameters
    )
    if(malaria_severe_to_treat$size() > 0){
      hf_treat_severe_malaria(
        target = malaria_severe_to_treat,
        variables = variables,
        parameters = parameters,
        events = events
      )
    }

    # Pneumonia
    pneumonia_severe_to_treat <- hf_diagnose_severe_pneumonia(
      target = target,
      variables = variables,
      parameters = parameters
    )
    if(pneumonia_severe_to_treat$size() > 0){
      hf_treat_severe_pneumonia(
        target = pneumonia_severe_to_treat,
        variables = variables,
        parameters = parameters,
        events = events
      )
    }
    ############################################################################

    ### Non-severe illness ######################################################
    # Remaining children who have not been treated for severe disease
    target <- target$set_difference(dia_severe_to_treat$or(malaria_severe_to_treat)$or(pneumonia_severe_to_treat))

    if(target$size() > 0){
      # Diarrhoea
      dia_to_treat <- hf_diagnose_diarrhoea(
        target = target,
        variables = variables,
        parameters = parameters
      )
      if(dia_to_treat$size() > 0){
        give_ors(
          target = dia_to_treat,
          parameters = parameters,
          variables = variables,
          events = events,
          timestep = timestep
        )
        n_ors_given <- n_ors_given + dia_to_treat$size()
      }

      # Malaria
      malaria_to_treat <- hf_diagnose_malaria(
        target = target,
        variables = variables,
        parameters = parameters
      )
      if(malaria_to_treat$size() > 0){
        give_act(
          target = malaria_to_treat,
          parameters = parameters,
          variables = variables,
          events = events,
          timestep = timestep
        )
        n_act_given <- n_act_given + malaria_to_treat$size()
      }

      # Pneumonia
      # Those who test +ve for malaria are not treated for pneumonia
      target <- target$set_difference(malaria_to_treat)
      pneumonia_to_treat <- hf_diagnose_pneumonia(
        target = target,
        variables = variables,
        parameters = parameters
      )
      if(pneumonia_to_treat$size() > 0){
        give_amoxicillin(
          target = pneumonia_to_treat,
          parameters = parameters,
          variables = variables,
          events = events,
          timestep = timestep
        )
        n_amoxicillin_given <- n_amoxicillin_given + pneumonia_to_treat$size()
      }
    }
    ############################################################################

    ### Record activity ########################################################
    renderer$render("hf_severe_diarrhoea_tx", dia_severe_to_treat$size(), timestep)
    renderer$render("hf_severe_malaria_tx", malaria_severe_to_treat$size(), timestep)
    renderer$render("hf_severe_pneumonia_tx", pneumonia_severe_to_treat$size(), timestep)
    renderer$render("hf_ors", n_ors_given, timestep)
    renderer$render("hf_act", n_act_given, timestep)
    renderer$render("hf_amoxicillin", n_amoxicillin_given, timestep)
    ############################################################################
  }
}


#' Health facility malaria diagnosis
#'
#' Accounts for the diagnostic a health facility uses as well as general human error
#'
#' @param target Children presenting at the health facility
#' @param variables Model variables
#' @param parameters Model parameters
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
hf_diagnose_malaria <- function(target, variables, parameters){
  disease_index = type_index(parameters, "malaria")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$dx_tx$rdt_sensitivity,
    spec = parameters$dx_tx$rdt_specificity,
    parameters = parameters,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$hf$efficacy)
  return(diagnosed)
}

#' Health facility severe malaria diagnosis
#'
#' Accounts for the diagnostic a health facility uses as well as general human error
#'
#' @inheritParams hf_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
hf_diagnose_severe_malaria <- function(target, variables, parameters){
  disease_index = type_index(parameters, "malaria")
  diagnosed <- severe_diagnosis(
    target = target,
    sens = parameters$hf$severe_malaria_sens,
    spec = parameters$hf$severe_malaria_spec,
    parameters = parameters,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$hf$efficacy)
  return(diagnosed)
}

#' Health facility treatment for severe malaria
#' @inheritParams hf_diagnose_malaria
#' @param events Model events
hf_treat_severe_malaria <- function(target, variables, parameters, events){

}

#' Health facility diarrhoea diagnosis
#'
#' Accounts for the diagnostic a health facility uses as well as general human error
#'
#' @inheritParams hf_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
hf_diagnose_diarrhoea <- function(target, variables, parameters){
  disease_index = type_index(parameters, "diarrhoea")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$hf$diarrhoea_sensitivity,
    spec = parameters$hf$diarrhoea_specificity,
    parameters = parameters,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$hf$efficacy)
  return(diagnosed)
}

#' Health facility severe diarrhoea diagnosis
#'
#' Accounts for the diagnostic a health facility uses as well as general human error
#'
#' @inheritParams hf_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
hf_diagnose_severe_diarrhoea <- function(target, variables, parameters){
  disease_index = type_index(parameters, "diarrhoea")
  diagnosed <- severe_diagnosis(
    target = target,
    sens = parameters$hf$severe_diarrhoea_sens,
    spec = parameters$hf$severe_diarrhoea_spec,
    parameters = parameters,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$hf$efficacy)
  return(diagnosed)
}

#' Health facility treatment for severe diarrhoea
#' @inheritParams hf_diagnose_malaria
#' @param events Model events
hf_treat_severe_diarrhoea <- function(target, variables, parameters, events){

}

#' Health facility pneumonia diagnosis
#'
#' Accounts for the diagnostic a health facility uses as well as general human error
#'
#' @inheritParams hf_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
hf_diagnose_pneumonia <- function(target, variables, parameters){
  disease_index = type_index(parameters, "pneumonia")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$hf$pneumonia_sensitivity,
    spec = parameters$hf$pneumonia_specificity,
    parameters = parameters,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$hf$efficacy)
  return(diagnosed)
}

#' Health facility svere pneumonia diagnosis
#'
#' Accounts for the diagnostic a health facility uses as well as general human error
#'
#' @inheritParams hf_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
hf_diagnose_severe_pneumonia <- function(target, variables, parameters){
  disease_index = type_index(parameters, "pneumonia")
  diagnosed <- severe_diagnosis(
    target = target,
    sens = parameters$hf$severe_pneumonia_sens,
    spec = parameters$hf$severe_pneumonia_spec,
    parameters = parameters,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$hf$efficacy)
  return(diagnosed)
}

#' Health facility treatment for severe pneumonia
#' @inheritParams hf_diagnose_malaria
#' @param events Model events
hf_treat_severe_pneumonia <- function(target, variables, parameters, events){

}
