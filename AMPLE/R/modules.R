# Common Shiny module stuff for AMPED and PIMPLE
# 
# Copyright 2019 Finlay Scott. Distributed under the GPL 3 or later
# Maintainer: Finlay Scott, SPC

# Option of separating the stochasticity and stock parameters into two modules
# Just means combining the two lists of values before dispatching them to the functions

# Module for the stochasticity options 

#' Modules for common features of the AMPED Shiny apps.
#'
#' stoch_params_setterUI() is the interface for the stochasticity options.
#'
#' @param id Shiny magic id
#' @param show_biol_prod_sigma Show the biological productivity variability option.
#' @param show_biol_est_sigma Show the estimation variability option.
#' @param show_biol_est_bias Show the estimation bias option.
#' @param init_prod_sigma Default value for biological productivity variability.
#' @param init_est_sigma Default value for estimation variability.
#' @param init_est_bias Default value for estimation bias. 
#' @param show_var Show the variability options.
#' @examples
#' \dontrun{
#'  # Put something like this in the Shiny apps UI code
#'  mp_params_setterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch",
#'  "Threshold effort", "Constant effort"))
#'  # And then something like this in the Shiny server code
#'  get_mp_params <- callModule(mp_params_setter, "mpparams") 
#' }
#' @return A taglist
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
stoch_params_setterUI <- function(id, show_biol_prod_sigma = TRUE, show_biol_est_sigma = TRUE, show_biol_est_bias = TRUE, init_prod_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=TRUE){
  ns <- NS(id)
  show_var <- checkboxInput(ns("show_var"), label = "Show variability options", value = show_var)
  # Decide which stochasticity options to show
  options <- list()
  if (show_biol_prod_sigma){
    options[[length(options)+1]] <- 
    tags$span(title="Natural variability in the stock biological processes (e.g. recruitment and growth)", numericInput(ns("biol_prod_sigma"), label = "Biological variability", value = init_prod_sigma, min=0, max=1, step=0.05))
  }
  if (show_biol_est_sigma){
    options[[length(options)+1]] <- 
    tags$span(title="Simulating the difference between the 'true' biomass and the 'estimated' biomass used by the HCR by applying randomly generated noise", numericInput(ns("biol_est_sigma"), label = "Estimation variability", value = init_est_sigma, min=0, max=1, step=0.05))
  }
  if (show_biol_est_bias){
    options[[length(options)+1]] <- 
    tags$span(title="Simulating the difference between the 'true' biomass and the 'estimated' biomass used by the HCR by applying a continuous bias (positive or negative)", numericInput(ns("biol_est_bias"), label = "Estimation bias", value = init_est_bias, min=-0.5, max=0.5, step=0.05))
  }

  vars <- conditionalPanel(condition="input.show_var == true", ns=ns, options)
  out <- tagList(show_var, vars)
  return(out)
}

#' stoch_params_setter
#'
#' stoch_params_setter() does the setting.
#'
#' @param input The UI input.
#' @param output Shiny magic.
#' @param session Shiny magic.
#' 
#' @return A list of stochasticity options.
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
stoch_params_setter<- function(input, output, session){
  get_stoch_params <- reactive({
    return(set_stoch_params(input))
  })
  return(get_stoch_params)
}

# Defined outside of reactive so we can use it in tests
#' mp_params_setter
#'
#' set_stoch_params() Sets up default values. Only exported to get the examples to work.
#'
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
set_stoch_params <- function(input){
    # Check which stochasticity options we are using
    # If null, we're not using them
    if(is.null(input$biol_prod_sigma)){
      biol_prod_sigma <- 0.0
    }
    else{
      biol_prod_sigma=input$biol_prod_sigma
    }
    if(is.null(input$biol_est_sigma)){
      biol_est_sigma <- 0.0
    }
    else{
      biol_est_sigma=input$biol_est_sigma
    }
    if(is.null(input$biol_est_bias)){
      biol_est_bias <- 0.0
    }
    else{
      biol_est_bias=input$biol_est_bias
    }
    out <- list(
      biol_prod_sigma=biol_prod_sigma,
      biol_est_sigma=biol_est_sigma,
      biol_est_bias=biol_est_bias
    )
    return(out)
}

#' stock_params_setterUI
#'
#' stock_params_setterUI() is the interface for the stock options (e.g. life history and exploitation status).
#'
#' @return A taglist
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
stock_params_setterUI <- function(id){
  ns <- NS(id)
    stock_lh <- tags$span(title="Choose the life history of the stock: slow, medium or fast growing. Some HCRs are more appropriate for different life histories.", 
      radioButtons(ns("stock_lh"), label= "Stock life history",
        choices = list("Slow growth" = "slow", "Medium growth" = "medium",
          "Fast growth" = "fast"), selected = "medium")
    )
    stock_history <- tags$span(title="Choose the history of the stock. Underexploited means that the stock could potentially be fished harder. Overexploited means that a recovery plan may be needed.", 
      radioButtons(ns("stock_history"), label= "Stock history",
                        choices = list("Underexploited" = "under", "Fully exploited" = "fully",
                                       "Overexploited" = "over"), selected = "fully")
      )
  return(tagList(stock_lh, stock_history))
}

#' stock_params_setter
#'
#' stock_params_setter() does the setting.
#'
#' @return A list of stock options.
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
stock_params_setter <- function(input, output, session){
  get_stock_params <- reactive({
    return(get_lh_params(input))
  })
  return(get_stock_params)
}

# Defined outside of above reactive so we can call it elsewhere
#' mp_params_setter
#'
#' get_lh_params() Sets up default values. Only exported to get the examples to work.
#'
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
get_lh_params <- function(input){
    # Set r and k depending on the stock choice radio button
    # Set MSY to be a 100 for each stock
    msy <- 100
    # MSY = rk/4
    r <- switch(input$stock_lh,
      "slow" =  0.2,
      "medium" = 0.6,
      "fast" = 1.0)
    k <- 4 * msy / r
    out <- list(
      r = r, k = k, b0 = k * 2/3, p = 1,
      stock_history = input$stock_history, # to set up the initial trajectory
      q = 1, # q is catchability so that CPUE = q * B. With effort creep this can change with time
      lrp=0.2, trp=0.5 # These are SB/SBF=0 based reference points - inline with WCPFC tuna stocks
      )
    return(out)
}

# Modules for setting the MP parameters
# Bit fiddly with the conditional panels inside a module

#' mp_params_setterUI
#'
#' mp_params_setterUI() is the interface for the HCR options
#'
#' @param mp_visible Which HCR types to show.
#' @param title The title.
#' @param init_thresh_max_catch Initial value of the maximum catch for the catch threshold HCR.
#' @param init_thresh_belbow Initial value of the belbow for the catch threshold HCR.
#' @param init_constant_catch Initial value of constant catch for the constant catch HCR.
#' @param init_constant_effort Initial value of constant effort for the constant effort HCR.
#' @param input_label The label of the menu.
#' 
#' @return A taglist
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
mp_params_setterUI <- function(id, mp_visible=NULL, title="Select the type of HCR you want to test.", init_thresh_max_catch=140, init_thresh_belbow=0.5, init_constant_catch=50, init_constant_effort=1.0, input_label="HCR Type"){
  ns <- NS(id)
  all_hcrs <- c("Constant catch" = "constant_catch",
                "Constant effort" = "constant_effort",
                "Threshold catch" = "threshold_catch",
                "Threshold effort" = "threshold_effort",
                "Empirical: CPUE slope only" = "empirical_cpue_slope")
    hcr_choices <- all_hcrs[mp_visible]

  # HCR options
  hcr_type <- tags$span(title=title,   
    selectInput(ns("hcr_type"), label=input_label, choices = hcr_choices))
  # The various parameter options
  ccpars <- conditionalPanel("input.hcr_type == 'constant_catch'", ns=ns,
    tags$span(title="Ignores stock status (e.g. estimated bimoass) and sets a constant catch limit.",
      sliderInput(ns("constant_catch_level"), "Constant catch level:", min = 0, max = 150, value = init_constant_catch, step = 1))
  )
  cepars <- conditionalPanel("input.hcr_type == 'constant_effort'", ns=ns,
    tags$span(title="Ignores stock status (e.g. estimated bimoass) and sets a constant effort limit.",
      sliderInput(ns("constant_effort_level"), "Constant relative effort level:", min = 0, max = 3, value = init_constant_effort, step = 0.01))
  )
  # Blim and Belbow used by threshold catch and threshold effort # || JavaScript OR
  tctepars <- conditionalPanel("input.hcr_type == 'threshold_catch' || input.hcr_type == 'threshold_effort' ", ns=ns,
    tags$span(title="The biomass levels that determine the shape of the HCR.",
      sliderInput(ns("blim_belbow"), "Blim and Belbow:", min = 0, max = 1, value = c(0.2, init_thresh_belbow), step = 0.01))
  )
  tcpars <- conditionalPanel("input.hcr_type == 'threshold_catch'", ns=ns,
    tags$span(title="The minimum and maximum catch limit levels output from the HCR.",
      sliderInput(ns("cmin_cmax"), "Cmin and Cmax:", min = 0, max = 250, value = c(10, init_thresh_max_catch), step = 1))
  )
  tepars <- conditionalPanel("input.hcr_type == 'threshold_effort'", ns=ns,
    tags$span(title="The minimum and maximum effort limit levels output from the HCR.",
      sliderInput(ns("emin_emax"), "Emin and Emax:", min = 0, max = 3, value = c(0.1,1.0), step = 0.01))
  )
  empcpueslopepars <- conditionalPanel("input.hcr_type == 'empirical_cpue_slope'", ns=ns,
    tags$span(title="Empirical HCR based on slope of recent CPUE (catch rate)",
      sliderInput(ns("gain"), "Gain:", min = 0, max = 5, value = 2, step = 0.1),
      sliderInput(ns("slope_years"), "Slope years", min=2, max=10, value=3, step = 1)
    ))
  out <- tagList(hcr_type, ccpars, cepars, tctepars, tcpars, tepars, empcpueslopepars)
  return(out)
}

#' mp_params_setter
#'
#' mp_params_setter() does the setting.
#'
#' @return A list of HCR options.
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
mp_params_setter <- function(input, output, session){
  get_mp_params <- reactive({
    # Messy setting of the parameters
    return(mp_params_switcheroo(input))
  })
  return(get_mp_params)
}

#Defined outside of the reactive above so we can use it non-reactively

#' mp_params_setter
#'
#' mp_params_switcheroo() Sets up default values. Only exported to get the examples to work.
#'
#' @rdname AMPED_modules
#' @name AMPED modules
#' @export
mp_params_switcheroo <- function(input){
    out <- switch(input$hcr_type,
      threshold_catch = list(hcr_shape = "threshold", mp_analysis = "assessment",
        mp_type="model", output_type="catch",
        name=paste("Thresh. catch: Blim=",input$blim_belbow[1],",Belbow=",input$blim_belbow[2],",Cmin=",input$cmin_cmax[1],",Cmax=",input$cmin_cmax[2], sep=""), 
        params = c(lim = input$blim_belbow[1], elbow = input$blim_belbow[2],
          min = input$cmin_cmax[1], max = input$cmin_cmax[2])),
      
      constant_catch = list(hcr_shape = "constant", mp_analysis = "assessment",
        mp_type="model", output_type="catch",
        name=paste("Const. catch: level=",input$constant_catch_level,sep=""),
        params = c(constant_level = input$constant_catch_level)),
      
      threshold_effort = list(hcr_shape = "threshold", mp_analysis = "assessment",
        mp_type="model", output_type="relative effort",
        name=paste("Thresh. effort: Blim=",input$blim_belbow[1],",Belbow=",input$blim_belbow[2],",Emin=",input$emin_emax[1],",Emax=",input$emin_emax[2], sep=""), 
        params = c(lim = input$blim_belbow[1], elbow = input$blim_belbow[2],
          min = input$emin_emax[1], max = input$emin_emax[2])),
      
      constant_effort = list(hcr_shape = "constant", mp_analysis = "assessment", mp_type="model",
         output_type="relative effort",
         name=paste("Const. effort: level=",input$constant_effort_level,sep=""),
         params = c(constant_level = input$constant_effort_level)),
                  
      # mp_analysis - name of function to generate HCR ip, mp_type - not sure it does anything
      empirical_cpue_slope = list(hcr_shape = "empirical_cpue_slope_op", mp_analysis = "empirical_cpue_slope_ip",
        mp_type="empirical", output_type="catch multiplier",
        name=paste("Empirical CPUE slope: Gain=",input$gain,",Slope years=",input$slope_years, sep=""), 
        params = c(gain = input$gain, slope_years = input$slope_years)),
      
      stop("In mp_params_setter. Unrecognised hcr_type.")) 
      # End of switch
    
    out$timelag <- 0 # 2
    return(out)
}
