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

severe_diagnosis <- function(target, illness, sens, spec, variables, parameters){
  true_positives <- individual::Bitset$new(parameters$population)
  for(disease in names(parameters$disease)){
    if(parameters$disease[[disease]]$type == illness){
      true_positives <- true_positives$or(variables[[paste0(disease, "_status")]]$get_index_of("severe"))
    }
  }
  true_negatives <- true_positives$not()
  # True positives OR False positives
  diagnosed <- true_positives$sample(sens)$or(true_negatives$sample(1 - spec))$and(target)
  return(diagnosed)
}

diagnosis <- function(target, illness, sens, spec, variables, parameters){
  true_positives <- individual::Bitset$new(parameters$population)
  for(disease in names(parameters$disease)){
    if(parameters$disease[[disease]]$type == illness){
      true_positives <- true_positives$or(variables[[paste0(disease, "_status")]]$get_index_of(c("asymptomatic", "symptomatic", "severe")))
    }
  }
  true_negatives <- true_positives$not()
  # True positives OR False positives
  diagnosed <- true_positives$sample(sens)$or(true_negatives$sample(1 - spec))$and(target)

  return(diagnosed)
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
long_symptoms <- function(target, disease_type, threshold, timestep, variables, parameters){
  long_symptom_duration <- rep(FALSE, target$size())
  for(disease in names(parameters$disease)){
    if(parameters$disease[[disease]]$type == disease_type){
      symptom_duration <- timestep - variables[[paste0(disease, "_symptom_onset")]]$get_values(target)
      symptom_duration[is.na(symptom_duration)] <- 0
      long_symptom_duration[symptom_duration > threshold] <- TRUE
    }
  }
  individual::filter_bitset(target, which(long_symptom_duration))
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
  for(disease in names(parameters$disease)){
    if(parameters$disease[[disease]]$type == "diarrhoea"){
      # If severe: -> symptomatic and clear fever
      to_ameliorate_severe <- variables[[paste0(disease, "_status")]]$get_index_of("severe")$and(target)
      to_ameliorate_severe <- to_ameliorate_severe$sample(parameters$dx_tx$ors_efficacy_severe)
      variables[[paste0(disease, "_status")]]$queue_update("symptomatic",to_ameliorate_severe)
      variables[[paste0(disease, "_fever")]]$queue_update("nonfebrile", to_ameliorate_severe)
      # If non-severe: if severe scheduled, cancel and schedule recovery
      to_avert_severe <- variables[[paste0(disease, "_status")]]$get_index_of(c("asymptomatic", "symptomatic"))$and(target)
      to_avert_severe <- to_avert_severe$and(events[[paste0(disease, "_progress_to_severe_infection")]]$get_scheduled())
      to_avert_severe <- to_avert_severe$sample(parameters$dx_tx$ors_efficacy)
      events[[paste0(disease, "_progress_to_severe_infection")]]$clear_schedule(to_avert_severe)
      clinical_duration <-  rexp(to_avert_severe$size(), 1 / parameters$disease[[disease]]$clinical_duration)
      events[[paste0(disease, "_progress_to_uninfected")]]$schedule(to_avert_severe, delay = clinical_duration)
    }
  }
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
  # Record when ACT was administered (for prophylaxsis)
  variables$time_of_last_act$queue_update(timestep, target)
  for(disease in names(parameters$disease)){
    if(parameters$disease[[disease]]$type == "malaria"){
      # Those treated and with disease targeted by ACT
      to_cure <- variables[[paste0(disease, "_status")]]$get_index_of(c("asymptomatic", "symptomatic", "severe"))$and(target)
      # And with successful treatment
      to_cure <- to_cure$sample(parameters$dx_tx$act_efficacy)
      if(to_cure$size() > 0){
        cure(to_cure, disease, variables, events)
      }
    }
  }
}

#' Give amoxicillin
#'
#' Provides amoxicillin antibiotic therapy to child
#'
#' @param target Target children
#' @param parameters Model parameters
#' @param variables Model variable
#' @param events Model events
#' @param timestep Model timestep
give_amoxicillin <- function(target, parameters, variables, events, timestep){
  # Record when amoxicillin was administered (for prophylaxsis)
  variables$time_of_last_amoxicillin$queue_update(timestep, target)
  for(disease in names(parameters$disease)){
    if(grepl("bacterial", disease)){
      # Those treated and with disease targeted by amoxicillin
      to_cure <- variables[[paste0(disease, "_status")]]$get_index_of(c("asymptomatic", "symptomatic", "severe"))$and(target)
      # And with successful treatment
      to_cure <- to_cure$sample(parameters$disease[[disease]]$amoxicillin_efficacy)
      if(to_cure$size() > 0){
        cure(to_cure, disease, variables, events)
      }
    }
  }
}

#' Give treatment for severe diarrhoea
#'
#' Provides treatment for severe diarrhoea
#'
#' @inheritParams give_ors
give_severe_treatment_diarrhoea <- function(target, parameters, variables, events, timestep){
  #target <- target$copy()$sample(parameters$hf$severe_diarrhoea_efficacy)
  #cure(target, "dia", variables, events)
}

#' Give treatment for severe pneumonia
#'
#' Provides treatment for severe pneumonia
give_severe_treatment_pneumonia <- function(target, parameters, variables, events, timestep){
  #target <- target$copy()$sample(parameters$hf$severe_diarrhoea_efficacy)
  #cure(target, "dia", variables, events)
}

#' Give treatment for severe malaria
#'
#' Provides treatment for severe malaria
#'
#' @inheritParams give_act
give_severe_treatment_malaria <- function(target, parameters, variables, events, timestep){
  # to_cure <- target$copy()$sample(parameters$hf$severe_malaria_efficacy)
  #variables$malaria_last_tx$queue_update(timestep, target)
  # cure(to_cure, "plasmodium_falciparum", variables, events)
}

#' Cure a condition
#'
#' @param condition Condition to cure
#' @inheritParams give_ors
cure <- function(target, disease, variables, events){
  variables[[paste0(disease, "_status")]]$queue_update("uninfected", target)
  variables[[paste0(disease, "_fever")]]$queue_update("nonfebrile", target)
  variables[[paste0(disease, "_symptom_onset")]]$queue_update(NA, target)
  # Clear any future scheduled life course of disease
  events[[paste0(disease, "_progress_to_asymptomatic_infection")]]$clear_schedule(target)
  events[[paste0(disease, "_progress_to_clinical_infection")]]$clear_schedule(target)
  events[[paste0(disease, "_progress_to_severe_infection")]]$clear_schedule(target)
  events[[paste0(disease, "_progress_to_uninfected")]]$clear_schedule(target)
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


any_fever <- function(parameters, variables){
  has_fever <- individual::Bitset$new(parameters$population)
  for(disease in names(parameters$disease)){
    has_fever <- has_fever$or(variables[[paste0(disease, "_fever")]]$get_index_of("febrile"))
  }
  return(has_fever)
}
