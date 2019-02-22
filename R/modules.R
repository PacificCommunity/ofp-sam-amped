# Copyright 2018 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC

# Shiny modules and other reusable functions
# Some of these should be moved to funcs - split between them is not clear at the moment

# Option of separating the stochasticity and stock parameters into two modules
# Just means combining the two lists of values before dispatching them to the functions

# Module for the stochasticity options 
stoch_params_setterUI <- function(id, init_prod_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=TRUE){
  ns <- NS(id)
  show_var <- checkboxInput(ns("show_var"), label = "Show variability options", value = show_var)
  vars <- conditionalPanel(condition="input.show_var == true", ns=ns,
      tags$span(title="Natural variability in the productivity of the stock through changes in growth and natural mortality", 
        numericInput(ns("biol_prod_sigma"), label = "Biological productivity variability", value = init_prod_sigma, min=0, max=1, step=0.05)
      ),
      tags$span(title="Simulating the difference between the 'true' biomass and the 'estimated' biomass used by the HCR by applying randomly generated noise", 
        numericInput(ns("biol_est_sigma"), label = "Estimation error variability", value = init_est_sigma, min=0, max=1, step=0.05)
      ),
      tags$span(title="Simulating the difference between the 'true' biomass and the 'estimated' biomass used by the HCR by applying a continuous bias (positive or negative)", 
        numericInput(ns("biol_est_bias"), label = "Estimation error bias", value = init_est_bias, min=-0.5, max=0.5, step=0.05)
      )
    )
  out <- tagList(show_var, vars)
  return(out)
}

stoch_params_setter<- function(input, output, session){
  get_stoch_params <- reactive({
    out <- list(biol_prod_sigma=input$biol_prod_sigma,
      biol_est_sigma=input$biol_est_sigma,
      biol_est_bias=input$biol_est_bias
    )
    return(out)
  })
  return(get_stoch_params)
}

# Choose if we want fast or slow growth etc
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

# From Andre's original code
#Population <- list(
#	list(K = 5964.506,r=0.6929,dmsy2=0.36788*2,PType="Fox"),
#	list(K = 4868.650,r=1.1621,dmsy2=0.36788*2,PType="Fox"),
#	list(K = 4241.026,r=1.5753,dmsy2=0.36788*2,PType="Fox")
# )

# Or use the stocks from Polacheck
# Fitting Surplus Production Models: Comparing Methods and Measuring Uncertainty
# Article in Canadian Journal of Fisheries and Aquatic Sciences · December 1993
#Fitting Surplus Production Models: Comparing Methods and Measuring Uncertainty
#Tom Polacheck, , Ray Hilborn, and , Andre E. Punt
#Canadian Journal of Fisheries and Aquatic Sciences, 1993, 50(12): 2597-2607, https://doi.org/10.1139/f93-284
# SA Albacore: r=0.328, k=239.6
# New Zealand Rock Lobster: r=0.0659 k=129
# Northern Namibian Hake: r=0.379 k= 2772.6

stock_params_setter <- function(input, output, session){
  get_stock_params <- reactive({
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
  })
  return(get_stock_params)
}

# Modules for setting the MP parameters
# Bit fiddly with the conditional panels inside a module
mp_params_setterUI <- function(id, mp_visible=NULL, init_thresh_max_catch=140, init_thresh_belbow=0.5){
  ns <- NS(id)
  all_hcrs <- c("Constant catch" = "constant_catch",
                "Constant effort" = "constant_effort",
                "Threshold catch" = "threshold_catch",
                "Threshold effort" = "threshold_effort")
  # The MPs that are visible in the UI are controlled by mp_visible argument
  # If not null it should have the names of the HCRs, matching those in the all_hcrs object
  #if (is.null(mp_visible)){
  #  hcr_choices <- all_hcrs
  #}
  #else {
  
  
    # Same order as mp_visible
    #hcr_choice_names <- names(all_hcrs)[names(all_hcrs) %in% mp_visible]
    #hcr_choices <- all_hcrs[hcr_choice_names[order(mp_visible)]]
    hcr_choices <- all_hcrs[mp_visible]
    #hcr_choices <- all_hcrs[names(all_hcrs) %in% mp_visible]
  #}

  # HCR options
  hcr_type <- tags$span(title="Select the type of HCR you want to test.",   
    selectInput(ns("hcr_type"), "HCR type", choices = hcr_choices))
  # The various parameter options
  ccpars <- conditionalPanel("input.hcr_type == 'constant_catch'", ns=ns,
    tags$span(title="Ignores stock status (e.g. estimated bimoass) and sets a constant catch limit.",
      sliderInput(ns("constant_catch_level"), "Constant catch level:", min = 0, max = 150, value = 100, step = 1))
  )
  cepars <- conditionalPanel("input.hcr_type == 'constant_effort'", ns=ns,
    tags$span(title="Ignores stock status (e.g. estimated bimoass) and sets a constant effort limit.",
      sliderInput(ns("constant_effort_level"), "Constant relative effort level:", min = 0, max = 3, value = 1, step = 0.01))
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
  out <- tagList(hcr_type, ccpars, cepars, tctepars, tcpars, tepars)
  return(out)
}

mp_params_setter <- function(input, output, session){
  get_mp_params <- reactive({
    # Messy setting of the parameters
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
                          stop("In mp_params_setter. Unrecognised hcr_type.")) 
    out$timelag <- 0 # 2
    return(out)
  })
  return(get_mp_params)
}

