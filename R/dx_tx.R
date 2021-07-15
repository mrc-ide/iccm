#' Index diseases of a given type
#'
#' @param parameters Model parameters
#' @param type Disease type (diarrhoea, malaria or pneumonia)
#'
#' @return A vector indexing which disease are of requested type
type_index <- function(parameters, type){
  which(sapply(parameters$disease, "[[", "type") == type)
}

#' Diagnostic result for disease
#'
#' @param target Children visiting provider
#' @param sens Diagnostic sensitivity
#' @param spec Diagnostic specificity
#' @param parameters Model parameters
#' @param variables Model variables
#' @param disease_index Disease index
#' @param positive_index State that are positive
#'
#' @return A bitset of those testing positive
diagnosis <- function(target, sens, spec, parameters, variables, disease_index, positive_index = c("asymptomatic", "symptomatic", "severe")){
  true_positives <- individual::Bitset$new(parameters$population)
  for(disease in disease_index){
    true_positives <- true_positives$or(variables$infection_status[[disease]]$get_index_of(c("asymptomatic", "symptomatic", "severe")))
  }
  true_negatives <- true_positives$not()
  # True positives OR False positives
  diagnosed <- true_positives$sample(sens)$or(true_negatives$sample(1 - spec))$and(target)
  return(diagnosed)
}

#' Diagnostic result for severe disease
#'
#' @inheritParams diagnosis
#'
#' @return  A bitset of those testing positive
severe_diagnosis <- function(target, sens, spec, parameters, variables, disease_index){
  diagnosis(target, sens, spec, parameters, variables, disease_index, positive_index = c("severe"))
}

#' Long symptoms
#'
#' Returns a bitset of those who have had symptoms for a period > the threshold to define illness as severe
#'
#' @param target Children seeking treatment
#' @param disease_index Disease index
#' @param threshold Period after which illness is classified as severe
#' @param timestep Timestep
#' @param variables Model variables
#'
#' @return Bitset
long_symptoms <- function(target, disease_index, threshold, timestep, variables){
  long_symptom_duration <- rep(FALSE, target$size())
  for(disease in disease_index){
    symptom_duration <- timestep - variables$symptom_onset[[disease]]$get_values(target)
    symptom_duration[is.na(symptom_duration)] <- 0
    long_symptom_duration[symptom_duration > threshold] <- TRUE
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
  disease_index = type_index(parameters, "diarrhoea")
  for(disease in disease_index){
    # If severe: -> symptomatic and clear fever
    to_ameliorate_severe <- variables$infection_status[[disease]]$get_index_of("severe")$and(target)
    to_ameliorate_severe <- to_ameliorate_severe$sample(parameters$dx_tx$ors_efficacy_severe)
    variables$infection_status[[disease]]$queue_update("symptomatic",to_ameliorate_severe)
    variables$fever[[disease]]$queue_update("nonfebrile", to_ameliorate_severe)
    # If non-severe: if severe scheduled, cancel and schedule recovery
    to_avert_severe <- variables$infection_status[[disease]]$get_index_of(c("asymptomatic", "symptomatic"))$and(target)
    to_avert_severe <- to_avert_severe$and(events$severe[[disease]]$get_scheduled())
    to_avert_severe <- to_avert_severe$sample(parameters$dx_tx$ors_efficacy)
    events$severe[[disease]]$clear_schedule(to_avert_severe)
    clinical_duration <-  stats::rexp(to_avert_severe$size(), 1 / parameters$disease[[disease]]$clinical_duration)
    events$susceptible[[disease]]$schedule(to_avert_severe, delay = clinical_duration)
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
  disease_index = type_index(parameters, "malaria")
  # Record when ACT was administered (for prophylaxsis)
  variables$time_of_last_act$queue_update(timestep, target)
  for(disease in disease_index){
    # Those treated and with disease targeted by ACT
    to_cure <- variables$infection_status[[disease]]$get_index_of(c("asymptomatic", "symptomatic", "severe"))$and(target)
    # And with successful treatment
    to_cure <- to_cure$sample(parameters$dx_tx$act_efficacy)
    if(to_cure$size() > 0){
      cure(
        target = to_cure,
        disease = disease,
        variables = variables,
        events = events
      )
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
  disease_index = type_index(parameters, "pneumonia")
  # Record when amoxicillin was administered (for prophylaxsis)
  variables$time_of_last_amoxicillin$queue_update(timestep, target)
  for(disease in disease_index){
    # Those treated and with disease targeted by amoxicillin
    to_cure <- variables$infection_status[[disease]]$get_index_of(c("asymptomatic", "symptomatic", "severe"))$and(target)
    # And with successful treatment
    to_cure <- to_cure$sample(parameters$disease[[disease]]$amoxicillin_efficacy)
    if(to_cure$size() > 0){
      cure(
        target = to_cure,
        disease = disease,
        variables = variables,
        events = events)
    }
  }
}

#' Give treatment for severe diarrhoea
#'
#' @param target Target
#' @param efficacy Efficacy
give_severe_treatment_diarrhoea <- function(target, efficacy){
  #target <- target$copy()$sample(parameters$hf$severe_diarrhoea_efficacy)
  #cure(target, "dia", variables, events)
}

#' Give treatment for severe pneumonia
#'
#' @param target Target
#' @param efficacy Efficacy
give_severe_treatment_pneumonia <- function(target, efficacy){
  #target <- target$copy()$sample(parameters$hf$severe_diarrhoea_efficacy)
  #cure(target, "dia", variables, events)
}

#' Give treatment for severe malaria
#'
#' @param target Target
#' @param efficacy Efficacy
give_severe_treatment_malaria <- function(target, efficacy){
  # to_cure <- target$copy()$sample(parameters$hf$severe_malaria_efficacy)
  #variables$malaria_last_tx$queue_update(timestep, target)
  # cure(to_cure, "plasmodium_falciparum", variables, events)
}

#' Cure a condition
#'
#' @param target Target children
#' @param disease Disease index
#' @param variables Model variables
#' @param events Model events
cure <- function(target, disease, variables, events){
  variables$infection_status[[disease]]$queue_update("uninfected", target)
  variables$fever[[disease]]$queue_update("nonfebrile", target)
  variables$symptom_onset[[disease]]$queue_update(as.numeric(NA), target)
  # Clear any future scheduled life course of disease
  events$asymptomatic[[disease]]$clear_schedule(target)
  events$clinical[[disease]]$clear_schedule(target)
  events$severe[[disease]]$clear_schedule(target)
  events$susceptible[[disease]]$clear_schedule(target)
}

#' Treatment prophylaxis modifier
#'
#' @param target Children exposed
#' @param disease Disease index
#' @param parameters Model parameters
#' @param variables Model variables
#' @param timestep Current timestep
#'
#' @return Treatment prophylaxis effect
treatment_prophylaxis <- function(target, disease, parameters, variables, timestep){
  tp <- rep(1, target$size())
  if(names(parameters$disease)[disease] == "plasmodium_falciparum"){
    time_since_treatment <- timestep - variables$time_of_last_act$get_values(target)
    tp <- 1 - exp(-time_since_treatment * (1 / parameters$dx_tx$act_halflife))
    # Individuals who have never received treatment have no prophylaxis
    tp[is.na(tp)] <- 1
  }
  return(tp)
}

#' Identify children with fever of any cause
#'
#' @param parameters Model parameters
#' @param variables Model variables
#'
#' @return A bitset of children with fever
any_fever <- function(parameters, variables){
  has_fever <- individual::Bitset$new(parameters$population)
  for(disease in 1:length(parameters$disease)){
    has_fever <- has_fever$or(variables$fever[[disease]]$get_index_of("febrile"))
  }
  return(has_fever)
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
  if(parameters$chw$introduction_time > 0){
    providers[2] <- 0
  }

  if(all(providers == 0)){
    preference <- rep("none", n)
  } else {
    provider_weights <- parameters$treatment_seeking$provider_preference_weights
    prob <- providers * provider_weights
    prob <- prob / sum(prob)
    preference <- sample(c("hf", "chw", "private"), n, replace = TRUE, prob = prob)
  }

  return(preference)
}


#' Resample provider preference when CHW introduced
#'
#' @param parameters Model parameters
#' @param variables Model variables
resample_preference <- function(parameters, variables){
  function(timestep){
    if(timestep == parameters$chw$introduction_time){
      providers <- c(parameters$hf$hf, parameters$chw$chw, parameters$private$private)
      provider_weights <- parameters$treatment_seeking$provider_preference_weights
      prob <- providers * provider_weights
      chw_prob <- prob[2] / sum(prob)
      target <- individual::Bitset$new(parameters$population)$insert(1:parameters$population)$sample(chw_prob)
      variables$provider_preference$queue_update("chw", target)
    }
  }
}
