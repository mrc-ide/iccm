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
    return(rep("None", n))
  }
  provider_weights <- parameters$treatment_seeking$provider_preference_weights
  prob <- providers * provider_weights
  prob <- prob / sum(prob)
  preference <- sample(c("HF", "CHW", "Private"), n, replace = TRUE, prob = prob)
  return(preference)
}

#' Provider result
#'
#' A function to capture the efficacy of the provider - currently encapsulates all aspects of "human error".
#'
#' @param disease Disease
#' @param efficacy Provider efficacy
px <- function(disease, efficacy){
  prob <- rep(efficacy, length(disease))
  provider_result <- stats::runif(length(prob), 0, 1) < prob
  return(provider_result)
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
    # Clear any duplicate future tx scheduling
    events$hf_treatment$clear_schedule(target)
    # Select children successfully diagnosed and recognised as severely ill:
    dia_severe_dx <- dx(variables$dia_status$get_values(target),
                        sens = parameters$hf$severe_diarrhoea_sensitivity,
                        spec = parameters$hf$severe_diarrhoea_specificity,
                        positive = 3)
    dia_severe_px <- px(variables$dia_disease$get_values(target), efficacy = parameters$hf$efficacy)
    dia_severe_to_treat <- individual::filter_bitset(target, which(dia_severe_dx & dia_severe_px))
    give_severe_treatment_diarrhoea(dia_severe_to_treat, parameters, variables, events, timestep)
    renderer$render("hf_severe_diarrhoea_tx", dia_severe_to_treat$size(), timestep)

    # Remaining children who have not been treated for severe disease
    target <- target$set_difference(dia_severe_to_treat)

    if(target$size() > 0){
      # Fever and malaria
      if(FALSE){
        fever <- individual::filter_bitset(target, which(variables$fever$get_values(target) == 1))
        pf_dx <- dx(variables$malaria_status$get_values(fever),
                    sens = parameters$dx_tx$rdt_sensitivity,
                    spec = parameters$dx_tx$rdt_specificity, positive = 1:3)
        pf_px <- px(variables$malaria_disease$get_values(fever), efficacy = parameters$hf$efficacy)
        malaria_to_treat <- individual::filter_bitset(fever, which(pf_dx & pf_px))
        give_act(malaria_to_treat)

        # Pneumonia
        pneumonia_dx <- dx(variables$pneumonia_status$get_values(target),
                           sens = parameters$hf$pneumonia_sensitivity,
                           spec = parameters$hf$pneumonia_specificity,
                           positive = 1:3)
        pneumonia_px <- px(variables$pneumonia_disease$get_values(target), efficacy = parameters$hf$efficacy)

        pneumonia_to_treat <- individual::filter_bitset(target, which(pneumonia_dx & pneumonia_px))
        give_amoxy(pneumonia_to_treat)
      }

      # Diarrhoea
      dia_dx <- dx(variables$dia_status$get_values(target),
                   sens = parameters$hf$diarrhoea_sensitivity,
                   spec = parameters$hf$diarrhoea_specificity,
                   positive = 1:3)
      dia_px <- px(variables$dia_disease$get_values(target), efficacy = parameters$hf$efficacy)
      dia_to_treat <- individual::filter_bitset(target, which(dia_dx & dia_px))
      give_ors(dia_to_treat, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_to_treat$size()
  }
    renderer$render("hf_ors", n_ors_given, timestep)
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
    # Clear any duplicate future tx scheduling
    events$chw_treatment$clear_schedule(target)

    ### Severe illness #########################################################
    # Select children successfully diagnosed and recognised as severely ill:
    dia_severe_dx <- dx(variables$dia_status$get_values(target),
                        sens = parameters$chw$severe_diarrhoea_sensitivity,
                        spec = parameters$chw$severe_diarrhoea_specificity,
                        positive = 3)
    dia_severe_px <- px(variables$dia_disease$get_values(target), efficacy = parameters$chw$efficacy)
    dia_symptom_length <- time_since_onset(target, "dia", variables, timestep) > parameters$dia$symptom_time_refer
    dia_severe_to_refer <- individual::filter_bitset(target, which((dia_severe_dx & dia_severe_px) | dia_symptom_length))

    if(dia_severe_to_refer$size() > 0){
      give_ors(dia_severe_to_refer, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_severe_to_refer$size()

      # TODO: referral - this will need to be for any severe
      if(parameters$hf$hf){
        renderer$render("chw_referral", dia_severe_to_refer$size(), timestep)
        events$hf_treatment$schedule(dia_severe_to_refer, delay = parameters$hf$travel_time + 1)
      }
    }
    ############################################################################

    ### Non-severe illness #####################################################
    # Remaining children who have not been treated for severe disease
    target <- target$set_difference(dia_severe_to_refer)

    if(target$size() > 0){
      # Fever and malaria
      if(FALSE){
        fever <- individual::filter_bitset(target, which(variables$fever$get_values(target) == 1))
        pf_dx <- dx(variables$malaria_status$get_values(fever),
                    sens = parameters$dx_tx$rdt_sensitivity,
                    spec = parameters$dx_tx$rdt_specificity, positive = 1:3)
        pf_px <- px(variables$malaria_disease$get_values(fever), efficacy = parameters$chw$efficacy)
        malaria_to_treat <- individual::filter_bitset(fever, which(pf_dx & pf_px))
        give_act(malaria_to_treat)

        # Pneumonia
        pneumonia_dx <- dx(variables$pneumonia_status$get_values(target),
                           sens = parameters$chw$pneumonia_sensitivity,
                           spec = parameters$chw$pneumonia_specificity,
                           positive = 1:3)
        pneumonia_px <- px(variables$pneumonia_disease$get_values(target), efficacy = parameters$chw$efficacy)
        pneumonia_to_treat <- individual::filter_bitset(target, which(pneumonia_dx & pneumonia_px))
        give_amoxy(pneumonia_to_treat)
      }

      # Diarrhoea
      dia_dx <- dx(variables$dia_status$get_values(target),
                   sens = parameters$chw$diarrhoea_sensitivity,
                   spec = parameters$chw$diarrhoea_specificity,
                   positive = 1:3)
      dia_px <- px(variables$dia_disease$get_values(target), efficacy = parameters$chw$efficacy)
      dia_to_treat <- individual::filter_bitset(target, which(dia_dx & dia_px))
      give_ors(dia_to_treat, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_to_treat$size()
    }
    ############################################################################

    renderer$render("chw_ors", n_ors_given, timestep)

    ### Follow up ##############################################################
    on_followup <- variables$awaiting_followup$get_index_of(set = 1)$and(target)
    renderer$render("chw_followup", on_followup$size(), timestep)
    to_followup <- variables$awaiting_followup$get_index_of(set = 0)$and(target)
    # Schedule future follow ups
    if(to_followup$size() > 0){
      events$chw_treatment$schedule(to_followup, delay = parameters$chw$followup_period)
      variables$awaiting_followup$queue_update(1, to_followup)
    }
    variables$awaiting_followup$queue_update(0, on_followup)
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
    # Clear any duplicate future tx scheduling
    events$private_treatment$clear_schedule(target)

    if(target$size() > 0){
      # Fever and malaria
      if(FALSE){
        fever <- individual::filter_bitset(target, which(variables$fever$get_values(target) == 1))
        pf_dx <- dx(variables$malaria_status$get_values(fever),
                    sens = parameters$dx_tx$rdt_sensitivity,
                    spec = parameters$dx_tx$rdt_specificity, positive = 1:3)
        pf_px <- px(variables$malaria_disease$get_values(fever), efficacy = parameters$private$efficacy)
        malaria_to_treat <- individual::filter_bitset(fever, which(pf_dx & pf_px))
        give_act(malaria_to_treat)

        # Pneumonia
        pneumonia_dx <- dx(variables$pneumonia_status$get_values(target),
                           sens = parameters$private$pneumonia_sensitivity,
                           spec = parameters$private$pneumonia_specificity,
                           positive = 1:3)
        pneumonia_px <- px(variables$pneumonia_disease$get_values(target), efficacy = parameters$private$efficacy)
        pneumonia_to_treat <- individual::filter_bitset(target, which(pneumonia_dx & pneumonia_px))
        give_amoxy(pneumonia_to_treat)
      }

      # Diarrhoea
      dia_dx <- dx(variables$dia_status$get_values(target),
                   sens = parameters$private$diarrhoea_sensitivity,
                   spec = parameters$private$diarrhoea_specificity,
                   positive = 1:3)
      dia_px <- px(variables$dia_disease$get_values(target), efficacy = parameters$private$efficacy)
      dia_to_treat <- individual::filter_bitset(target, which(dia_dx & dia_px))
      give_ors(dia_to_treat, parameters, variables, events, timestep)
      n_ors_given <- n_ors_given + dia_to_treat$size()
    }
    renderer$render("private_ors", n_ors_given, timestep)
  }
}
