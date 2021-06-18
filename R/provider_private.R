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
    dia_to_treat <- private_diagnose_diarrhoea(
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
    malaria_to_treat <- private_diagnose_malaria(
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
    pneumonia_to_treat <- private_diagnose_pneumonia(
      target = target,
      variables = variables,
      parameters = parameters
    )
    if(pneumonia_to_treat$size() > 0){
      give_amoxicillin(
        target = pneumonia_severe_to_refer,
        parameters = parameters,
        variables = variables,
        events = events,
        timestep = timestep
      )
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


#' Private provider malaria diagnosis
#'
#' Accounts for the diagnostic a private provider uses as well as general human error
#'
#' @param target Children presenting at the Private
#' @param variables Model variables
#' @param parameters Model parameters
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
private_diagnose_malaria <- function(target, variables, parameters){
  disease_index = type_index(parameters, "malaria")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$dx_tx$rdt_sensitivity,
    spec = parameters$dx_tx$rdt_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$private$efficacy)
  return(diagnosed)
}

#' Private provider diarrhoea diagnosis
#'
#' Accounts for the diagnostic a private provider  uses as well as general human error
#'
#' @inheritParams private_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
private_diagnose_diarrhoea <- function(target, variables, parameters){
  disease_index = type_index(parameters, "diarrhoea")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$private$diarrhoea_sensitivity,
    spec = parameters$private$diarrhoea_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$private$efficacy)
  return(diagnosed)
}

#' Private provider pneumonia diagnosis
#'
#' Accounts for the diagnostic a private provider  uses as well as general human error
#'
#' @inheritParams private_diagnose_malaria
#'
#' @return A bit set of children that have tested +ve (including false +ves) and
#' who have been successfully processed by the health facility.
private_diagnose_pneumonia <- function(target, variables, parameters){
  disease_index = type_index(parameters, "pneumonia")
  diagnosed <- diagnosis(
    target = target,
    sens = parameters$private$pneumonia_sensitivity,
    spec = parameters$private$pneumonia_specificity,
    variables = variables,
    disease_index = disease_index
  )
  diagnosed <- diagnosed$sample(parameters$private$efficacy)
  return(diagnosed)
}


