#' Diagnostic result
#'
#' Returns a bitset of those testing positive
#'
#' @param target Children visiting provider
#' @param status_variable Disease status variable
#' @param sens Diagnostic sensitivity
#' @param spec Diagnostic specificity
#' @param positive Status categories that are true +ves
#' @param negative Status categories  that are true -ves
#'
#' @return Bitset
dx <- function(target, status_variable, sens, spec, positive, negative){
  # True positives OR False negatives
  status_variable$get_index_of(positive)$sample(sens)$or(status_variable$get_index_of(negative)$sample(1 - spec))$and(target)
}

#' Long symptoms
#'
#' Returns a bitset of those who have had symptoms for a period > the threshold to define illness as severe
#'
#' @param symptom_start_var Variable of symptom start time
#' @param target Children seeking treatment
#' @param threshold Period after which illness is classified as severe
#' @param timestep Timestyep
#'
#' @return Bitset
long_symptoms <- function(symptom_start_var, target, threshold, timestep){
  individual::filter_bitset(target, which(time_since_onset(target, symptom_start_var, timestep) > threshold))
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

#' Give ACT
#'
#' Provides artemisinin combination therapy to child
#'
#' @param target Target children
#' @param parameters Model parameters
#' @param variables Model variable
#' @param events Model events
#' @param timestep Model timestep
give_act <- function(target, parameters, variables, events, timestep){
  target <- target$copy()$sample(parameters$dx_tx$act_efficacy)
  variables$malaria_last_tx$queue_update(timestep, target)
  cure(target, "malaria", variables, events)
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

#' Give treatment for severe malaria
#'
#' Provides treatment for severe malaria
#'
#' @inheritParams give_act
give_severe_treatment_malaria <- function(target, parameters, variables, events, timestep){
  target <- target$copy()$sample(parameters$hf$severe_malaria_efficacy)
  variables$malaria_last_tx$queue_update(timestep, target)
  cure(target, "malaria", variables, events)
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
