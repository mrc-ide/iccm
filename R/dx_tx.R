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
  variables$dia_last_tx$queue_update(timestep, target)
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
  variables[[paste0(condition, "_fever")]]$queue_update(0, target)
  events[[paste0(condition, "_asymptomatic")]]$clear_schedule(target)
  events[[paste0(condition, "_recover")]]$clear_schedule(target)
}

#' Treatment prophylaxis modifier
#'
#' @param time_since_treatment Time since disease last treated
#' @param pr_hl Prophylaxis half life
#'
#' @return Treatment prophylaxis effect
treatment_prophylaxis <- function(time_since_treatment, pr_hl){
  tp <- 1 - exp(-time_since_treatment * (1 / pr_hl))
  # Individuals who have never received treatment have no prophylaxis
  tp[is.na(tp)] <- 1
  return(tp)
}
