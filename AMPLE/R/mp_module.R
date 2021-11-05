# Module for setting the MP parameters
# This is used to create a list of MP parameters
# The MP parameters list elements are:



# The UI bit sorts out what input controls should be on the screen depending on the selected HCR
# The server bit scrapes all the inputs into a list of mp params

#' mpParamsSetterUI
#'
#' The interface for the HCR options.
#' The parameter selection inputs shown in the app are conditional on the selected type of HCR.
#' Some of the inputs have initial values that can be set using the function arguments.
#'
#' @param mp_visible Which HCR types to show.
#' @param title The title.
#' @param init_thresh_max_catch Initial value of the maximum catch for the catch threshold HCR.
#' @param init_thresh_belbow Initial value of the belbow for the catch threshold HCR.
#' @param init_constant_catch Initial value of constant catch for the constant catch HCR.
#' @param init_constant_effort Initial value of constant effort for the constant effort HCR.
#'
#' @return A taglist
#' @rdname MP_modules
#' @name MP modules
#' @export
mpParamsSetterUI <- function(id, mp_visible=NULL, title="Select the type of HCR you want to test.", init_thresh_max_catch=140, init_thresh_belbow=0.5, init_constant_catch=50, init_constant_effort=1.0){
  ns <- NS(id)
  # Named vector of currently implemented HCRs
  all_hcrs <- c("Constant catch" = "constant_catch",
                "Constant effort" = "constant_effort",
                "Threshold catch" = "threshold_catch",
                "Threshold effort" = "threshold_effort")
                #"Empirical: CPUE slope only" = "empirical_cpue_slope") # To come later
  # Subset which HCRs you want to be available in the current app
  hcr_choices <- all_hcrs[mp_visible]

  # Drop down menu of HCR types
  hcr_type <- tags$span(title=title, selectInput(ns("hcr_type"), label="HCR Type", choices = hcr_choices))

  # The parameter option inputs shown in the app are conditional on the selected HCR type
  # This is handled by a bunch of conditional Panels
  # Could use a switch statement instead? Not sure it would help write clearer code

  # Constant catch
  ccpars <- conditionalPanel("input.hcr_type == 'constant_catch'", ns=ns,
    tags$span(title="Ignores stock status (e.g. estimated bimoass) and sets a constant catch limit.",
    sliderInput(ns("constant_catch_level"), "Constant catch level:", min = 0, max = 150, value = init_constant_catch, step = 1))
  )

  # Constant effort
  cepars <- conditionalPanel("input.hcr_type == 'constant_effort'", ns=ns,
    tags$span(title="Ignores stock status (e.g. estimated bimoass) and sets a constant effort limit.",
    sliderInput(ns("constant_effort_level"), "Constant relative effort level:", min = 0, max = 3, value = init_constant_effort, step = 0.01))
  )

  # Blim and Belbow (used by threshold catch and threshold effort)
  # Note || JavaScript OR
  tctepars <- conditionalPanel("input.hcr_type == 'threshold_catch' || input.hcr_type == 'threshold_effort' ", ns=ns,
    tags$span(title="The biomass levels that determine the shape of the HCR.",
    sliderInput(ns("blim_belbow"), "Blim and Belbow:", min = 0, max = 1, value = c(0.2, init_thresh_belbow), step = 0.01))
  )

  # Cmin and Cmax (used by threshold catch)
  tcpars <- conditionalPanel("input.hcr_type == 'threshold_catch'", ns=ns,
    tags$span(title="The minimum and maximum catch limit levels output from the HCR.",
    sliderInput(ns("cmin_cmax"), "Cmin and Cmax:", min = 0, max = 250, value = c(10, init_thresh_max_catch), step = 1))
  )

  # Emin and Emax (used by threshold effort)
  tepars <- conditionalPanel("input.hcr_type == 'threshold_effort'", ns=ns,
    tags$span(title="The minimum and maximum effort limit levels output from the HCR.",
    sliderInput(ns("emin_emax"), "Emin and Emax:", min = 0, max = 3, value = c(0.1,1.0), step = 0.01))
  )

  # Empirical still to come in another app
  ## Gain and slope years (used by empirical CPUE slope)
  #empcpueslopepars <- conditionalPanel("input.hcr_type == 'empirical_cpue_slope'", ns=ns,
  #  tags$span(title="Empirical HCR based on slope of recent CPUE (catch rate)",
  #  sliderInput(ns("gain"), "Gain:", min = 0, max = 5, value = 2, step = 0.1),
  #  sliderInput(ns("slope_years"), "Slope years", min=2, max=10, value=3, step = 1)
  #))

  # Put together list to return to the UI function.
  # If the conditionalPanel is not selected, that element is NULL and ignored
  out <- tagList(hcr_type, ccpars, cepars, tctepars, tcpars, tepars)#, empcpueslopepars)
  return(out)
}

#' mpParamsSetterServer
#'
#' Does the setting part of the MP params module.
#' Returns a list of MP params based on the MP inputs.
#'
#' @param id The id (shiny magic)
#' @param get_stoch_params Reactive expression that gets the parameters from the stochasticity setter. Otherwise est_sigma and est_bias are set to 0.
#' @return A list of HCR options.
#' @rdname MP_modules
#' @name MP modules
mpParamsSetterServer <- function(id, get_stoch_params=NULL){
  moduleServer(id, function(input, output, session){
    reactive({
      # Setting the stochasticity parameters in the mp_params using the values in the stochasticity reactive expr.
      stoch_params <- list(est_sigma = 0, est_bias=0)
      if(!is.null(get_stoch_params)){
        stoch_params <-  get_stoch_params()
      }
      return(mp_params_switcheroo(input, est_sigma = stoch_params$est_sigma, est_bias = stoch_params$est_bias))
    })
  })
}

#' mp_params_switcheroo
#'
#' Creates the MP params list based on the MP selection from the Shiny UI.
#' Defined outside of a reactive environment above so we can use it non-reactively (helpful for testing).
#' @param input List of information taken from the Shiny UI (mpParamsSetterUI)
#' @param est_sigma Standard deviation of the estimation variability (default = 0).
#' @param est_bias Estimation bias as a proportion. Can be negative (default = 0).
#'
#' @rdname MP_modules
#' @name MP modules
#' @export
mp_params_switcheroo <- function(input, est_sigma = 0, est_bias = 0){
    out <- switch(input$hcr_type,
      threshold_catch = list(hcr_shape = "threshold", mp_analysis = "assessment",
        mp_type="model", output_type="catch",
        name=paste("Thresh. catch: Blim=",input$blim_belbow[1],",Belbow=",input$blim_belbow[2],",Cmin=",input$cmin_cmax[1],",Cmax=",input$cmin_cmax[2], sep=""),
        params = c(lim = input$blim_belbow[1], elbow = input$blim_belbow[2], min = input$cmin_cmax[1], max = input$cmin_cmax[2])),

      constant_catch = list(hcr_shape = "constant", mp_analysis = "assessment",
        mp_type="model", output_type="catch",
        name=paste("Const. catch: level=",input$constant_catch_level,sep=""),
        params = c(constant_level = input$constant_catch_level)),

      threshold_effort = list(hcr_shape = "threshold", mp_analysis = "assessment",
        mp_type="model", output_type="relative effort",
        name=paste("Thresh. effort: Blim=",input$blim_belbow[1],",Belbow=",input$blim_belbow[2],",Emin=",input$emin_emax[1],",Emax=",input$emin_emax[2], sep=""),
        params = c(lim = input$blim_belbow[1], elbow = input$blim_belbow[2], min = input$emin_emax[1], max = input$emin_emax[2])),

      constant_effort = list(hcr_shape = "constant", mp_analysis = "assessment",
        mp_type="model", output_type="relative effort",
        name=paste("Const. effort: level=",input$constant_effort_level,sep=""),
        params = c(constant_level = input$constant_effort_level)),
        # Could add base year as an extra parameter for the relative effort ones
        # This would need an input
        # If do this, last_historical_timestep is not needed in the stock?

      # Empirical not yet implemented - to come but needs separate app
      # mp_analysis - name of function to generate HCR ip, mp_type - not sure it does anything
      #empirical_cpue_slope = list(hcr_shape = "empirical_cpue_slope_op", mp_analysis = "empirical_cpue_slope_ip",
      #  mp_type="empirical", output_type="catch multiplier",
      #  name=paste("Empirical CPUE slope: Gain=",input$gain,",Slope years=",input$slope_years, sep=""),
      #  params = c(gain = input$gain, slope_years = input$slope_years)),

      stop("In mp_params_setter. Unrecognised hcr_type.")
    )  # End of switch
    # Add in the stochasticity options and timelag
    out$est_bias = est_bias
    out$est_sigma = est_sigma
    out$timelag <- 0 # 2
    return(out)
}
