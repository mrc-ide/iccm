
create_events <- function(individuals, variables){
  events <- list()

  # Symptomatic to susceptible
  events$diarrhoea_recover <- recover_event("diarrhoea", individuals, variables)

  return(events)
}

recover_event <- function(condition, individuals, variables){
  event<- individual::Event$new(paste0(condition, "_recover"))
  # Add listener
  event$add_listener(
    function(api, target) {
      # Set status = 0 = susceptible
      api$queue_variable_update(individuals$child, variables[[paste0(condition, "_status")]], 0, target)
       # Set disease indec = 0 = no disease
      api$queue_variable_update(individuals$child, variables[[paste0(condition, "_disease_index")]], 0, target)
    })

  return(event)
}
