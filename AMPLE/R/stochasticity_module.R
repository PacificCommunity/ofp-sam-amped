# Stochasticity module
# Handles the biological and esimtation variability options in the user interface

#' stochParamsSetterUI
#' 
#' stochParamsSetterUI() is the UI part for the stochasticity options.
#' Stochasticity is included in the projections in two areas: biological variability (e.g. recruitment)
#' and estimation error (to represent the difference between the 'true' status of the stock and the estimated status that is used by the HCR).
#' Estimation error includes bias and variability.
#' The arguments to this function allow only some of these elements to be shown.
#'
#' @param id Shiny magic id
#' @param show_var Show the variability options when app opens (default is FALSE). 
#' @param show_biol_sigma Show the biological productivity variability option (default is TRUE).
#' @param show_est_sigma Show the estimation variability option (default is TRUE).
#' @param show_est_bias Show the estimation bias option (default is TRUE).
#' @param init_biol_sigma Default value for biological productivity variability (ignored if not shown).
#' @param init_est_sigma Default value for estimation variability (ignored if not shown).
#' @param init_est_bias Default value for estimation bias (ignored if not shown). 
#' @return A taglist
#' @rdname stoch_module
#' @name Stochasticity module
stochParamsSetterUI <- function(id, show_var=FALSE, show_biol_sigma = TRUE, show_est_sigma = TRUE, show_est_bias = TRUE, init_biol_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0){
  ns <- NS(id) # To save doing stuff like NS(id, "show_var")
  
  # Check box for showing any of the stochasticity options
  show_var <- checkboxInput(ns("show_var"), label = "Show variability options", value = show_var)
  
  # Store which stochasticity options to show in a list
  options <- list()
  if (show_biol_sigma){
    options[[length(options)+1]] <- 
      tags$span(title="Natural variability in the stock biological processes (e.g. recruitment and growth)",
        numericInput(ns("biol_sigma"), label = "Biological variability", value = init_biol_sigma, min=0, max=1, step=0.05))
  }
  if (show_est_sigma){
    options[[length(options)+1]] <- 
      tags$span(title="Simulating the difference between the 'true' biomass and the 'estimated' biomass used by the HCR by applying randomly generated noise",
        numericInput(ns("est_sigma"), label = "Estimation variability", value = init_est_sigma, min=0, max=1, step=0.05))
  }
  if (show_est_bias){
    options[[length(options)+1]] <- 
      tags$span(title="Simulating the difference between the 'true' biomass and the 'estimated' biomass used by the HCR by applying a continuous bias (positive or negative)",
        numericInput(ns("est_bias"), label = "Estimation bias", value = init_est_bias, min=-0.5, max=0.5, step=0.05))
  }
  
  # Put together all of the selected options into a conditionalPanel
  vars <- conditionalPanel(condition="input.show_var == true", ns=ns, options)
  out <- tagList(show_var, vars)
  return(out)
}

#' stochParamsSetterServer
#'
#' stochParamSetterServer() does the server side stuff for the stochasticity options.
#'
#' @param id The id (shiny magic)
#' @return A list of stochasticity options.
#' @rdname stoch_module
#' @name Stochasticity module
stochParamsSetterServer <- function(id){
  moduleServer(id, function(input, output, session){
    reactive({return(set_stoch_params(input))})
  })
}


#' set_stoch_params
#'
#' set_stoch_params() sets up default values for the stochasticity parameters.
#' Defined as a separate function so it can be used for testing outside of a reactive environment.
#' @param input A list of stochasticity parameters.
#' @rdname stoch_module
#' @name Stochasticity module
set_stoch_params <- function(input){
  # Create a list of the stochasticity parameters - if they are not in the input, set to 0
  params <- c("biol_sigma", "est_sigma", "est_bias")
  out <- lapply(params, function(x) ifelse(is.null(input[[x]]), 0.0, input[[x]]))
  names(out) <- params
  return(out)
}

