#' Diagnostic result
#'
#' Returns a boolean indicating results of a diagnostic test
#'
#' @param status Child status
#' @param sens Diagnostic sensitivity
#' @param spec Diagnostic specificity
#' @param positive Status categories associated that are true +ves
#'
#' @return Boolean vector, TRUE = positive test, FALSE = negative test
dx <- function(status, sens, spec, positive = 1:3){
  prob <- rep(0, length(status))
  index <- status %in% positive
  prob[index] <- sens # True positive
  prob[!index] <- 1 - spec # False positive
  dx_result <- stats::runif(length(prob), 0, 1) < prob
  return(dx_result)
}

#' Give ORS
#'
#' Provides oral rehydration salts (ORS) to a child
#'
#' @param target Target children
#' @param parameters Model parameters
#' @param variables Model variable
#' @param events Model events
#' @param timestep Model timestep
give_ors <- function(target, parameters, variables, events, timestep){
  target <- target$copy()$sample(parameters$dx_tx$ors_efficacy)
  cure(target, "dia", variables, events)
}

#' Give treatment for severe diarrhoea
#'
#' Provides treatment for severe diarrhoea
#'
#' @inheritParams give_ors
give_severe_treatment_diarrhoea <- function(target, parameters, variables, events, timestep){
  target <- target$copy()$sample(parameters$hf$severe_diarrhoea_efficacy)
  cure(target, "dia", variables, events)
}

#' Cure a condition
#'
#' @param condition Condition to cure
#' @inheritParams give_ors
cure <- function(target, condition, variables, events){
  variables[[paste0(condition, "_status")]]$queue_update(0, target)
  variables[[paste0(condition, "_disease")]]$queue_update(0, target)
  variables[[paste0(condition, "_symptom_start")]]$queue_update(NA, target)
  events[[paste0(condition, "_recover")]]$clear_schedule(target)
}

