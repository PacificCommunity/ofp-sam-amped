# The Introduction to HCRs app
# intro_hcr.R 

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: Disintegration Dubs by G36 vs JK Flesh
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' Introduction to HCRs app launcher
#' 
#' Launches the introduction to HCRs Shiny app.
#' @param ... Not used
#' @export
intro_hcr <- function(...){

  # User interface ----
  # Use navbarPage (fluidPage has a problem with title argument and tabs)
  ui <- navbarPage(
    title="Introduction to HCRs",
      tabPanel(title = "What is a Harvest Control Rule?",
        # Initiate sidebarLayout - fixed sidebar for all tabs (but we can turn the interaction options on and off)
        sidebarLayout(
          sidebar_setup(
            # MP selector
            # HCR options
            mpParamsSetterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch")),# "Threshold effort", "Constant effort")),
            br(), # Could add br() automatically to side bar set up to separate each component?

            # Buttons
            tags$span(title="Go forward one year",
              actionButton("advance", "Advance")),
            tags$span(title="Reset current projection",
              actionButton("reset", "Reset")),
            br(),
            # Stochasticity module
            stochParamsSetterUI("stoch", init_biol_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE),
            br()
          ),


          mainPanel(
            textOutput("printtimestep"),
            # Set up the main panel as before
            # Main plot for the Intro to HCR app
            # 2 x 2 panel
            # Catch  |  HCR
            # ----------------
            # B/K    |  connecting arrow
            fluidRow(
              column(6,
                tags$span(title="Plot of the total catch. The blue, dashed horizontal line is next years catch limit that has been set by the HCR. The grey, dashed horizontal lines are the catch limits that were set by the HCR in the past.",
                plotOutput("plot_catch", width="auto"))
              ),
              column(6,
                tags$span(title="The HCR. The blue, dashed vertical line shows the current estimated biomass that is used as the input. The blue, dashed horizontal line shows the resulting catch limit that will be set for the following year",
                plotOutput("plot_hcr", width="auto"))
              )
            ), # End of fluid Row
            fluidRow(
              column(6,
                tags$span(title="The biomass of the stock (scaled by the unfished biomass). When the estimation variability options are switched on, the black line is the 'true' biomass and the blue line is the 'estimated' biomass. The HCR uses the estimated biomass for the input.",
                plotOutput("plot_biomass", width="auto"))
              ),
              column(6,
                tags$span(title="The current estimated biomass is used as the input to the HCR.",
                plotOutput("plot_arrow", width="auto"))
              )
            ),
            
            # Just put here for testing right now
            tableOutput("printstock")



          ) # End of mainPanel
        ) # End of sidebarLayout
      ), # End of tabPanel 1

      tabPanel(title = "Settings",
        # Initiate sidebarLayout - this is annoying having to do this for each tab - but could use functions to reduce amount of boilerplate code
        sidebarLayout(
          sidebar_setup(
            br()
          ),
          mainPanel(
            # Life history parameters projection options
            stockParamsSetterUI("stock"),
          ) # End of mainPanel
        ) # End of sidebarLayout
      ), # End of Settings tabPanel 

    tabPanel(title = "About",
      sidebarLayout(
        sidebar_setup(
          ample_maintainer_and_licence()
        ),
        mainPanel(
          spc_about()
        ) # End of mainPanel
      ) # End of sidebarLayout
    ) # End of About tabPanel

  ) # End of navbarPage
  
  #--------------------------------------------------------------------------

  # Start of server function
  server <- function(input, output,session) {

    # Notes for myself:
    # get_mp_params is a "reactiveExpr"
    # Use: get_mp_params() to evaluates the reactiveExpr (evaluates the switcheroo function and returns the parameters)
    # Evaluating reactiveExpr can only be done inside a reactive consumer (like an observer or reactive)
    get_stoch_params <- stochParamsSetterServer("stoch")
    get_mp_params <- mpParamsSetterServer("mpparams", get_stoch_params)
    #get_mp_params <- mpParamsSetterServer("mpparams") # Test without get_stoch_params - doesn't update mp_params as stochasticity parameters change
    get_stock_params <- stockParamsSetterServer("stock", get_stoch_params)
    niters <- 1 # Always 1 for this app

    # Notes on the reactive stock
    
    # stock is a reactiveExpr. Evaluating stock() calls the Stock$reactive() method.
    # This method accesses a reactiveVal (called reactiveDep) inside Stock and then returns self.
    # This effectively makes stock a reactive version of a Stock as each call to it accesses reactiveDep.
    # When reactiveDep changes, the stock object invalidates.
    # Changing reativeDep is added to what method you want to invalidate the stock.
    # Here we only add it to project(). When project() is called, the last thing the method does is change the
    # value of reactiveDep. stock() therefore becomes invalid and can triggers things in the Shiny app.
    
    # As stock is a reactiveExpr, you cannot create it insides a reactive environment.
    # This means you have to isolate() get_stock_params() and get_mp_params() when you create stock.
    # A Srock$reset() method is used to reset the stock if the params change.
    # Construction split into two parts
    # 1.  Make an instance of the stock with new()
    # 2.  Include a reset method that changes / resets the members of that instance
    
    # Make instance of the stock
    # (could this be improved - make reactive a boolean option? Not sure, the reactive stock() has to be a 
    # reactiveExpr that has the reactiveDep and returns self - could set up a maker function?)
    # This is just a normal stock that can be used outside of shiny purposes
    stock_noreactive <- Stock$new(stock_params = isolate(get_stock_params()), mp_params = isolate(get_mp_params()), niters = niters)
    # Make a reactive version by calling the reactive() method (or do it all at once)
    stock <- stock_noreactive$reactive()
    #stock <- Stock$new(stock_params = isolate(get_stock_params()), mp_params = isolate(get_mp_params()), niters = niters)$reactive()
    
    # Set up the timestep - initial value is last historical timestep
    timestep <- reactiveVal(isolate(get_stock_params()$last_historical_timestep))
    
    # Reset observer - clears out the stock information
    # Any invalid reactive objects in this observer will trigger this, including an invalid stock.
    # For example, when stock() becomes invalid because project() method has been called.
    # To avoid resetting the stock when stock becomes invalid, wrap the reset() method in isolate.
    # This gets called straight away - bit wasteful - possible delay it? Observe are 'eager', reactive are 'lazy'
    # What can trigger the reset:
    # - changing stock and MP params
    # - pressing the reset button
    observe({
      message("In reset observer")
      input$reset
      stock_params <- get_stock_params()
      mp_params <- get_mp_params()
      # Reset the timestep
      timestep(get_stock_params()$last_historical_timestep)
      # Use isolate else this is triggered when stock becomes invalid (i.e. after project) 
      isolate(stock()$reset(stock_params = stock_params, mp_params = mp_params, niters=niters))
    }, label="resetter")
    
    
    # Apparently bindEvent is now recommende over observeEvent but there are no clear examples of how to use it
    observeEvent(input$advance, {
      # Advance the timestep if able
      if(timestep() < get_stock_params()$nyears){
        timestep(timestep()+1)
      }
      # Call the project() method. This invalidates the stock() object
      # (by internally changing the reactiveDep field).
      # The invalidated stock can then trigger other stuff
      stock()$project(timesteps=timestep(), mp_params=get_mp_params())
    })
    
    #---------------------------------------------------------------
    # Output stuff
    
    output$printstock <- renderTable({
      # This output is triggered if stock is invalidated, i.e. through the project() method
      stock_temp <- stock()
      stock_temp$as_data_frame()
    })
    
    output$printtimestep <- renderText({
      return(paste("Time step: ", timestep(), sep=""))
    })
    
    output$plot_catch <- renderPlot({
      plot_catch_hcr(stock=stock(), mp_params=get_mp_params(), timestep=timestep(), cex.axis=1.1, cex.lab=1.3, main="Catch")
    })
    
    output$plot_biomass <- renderPlot({
      plot_biomass(stock=stock(), mp_params=get_mp_params(), cex.axis=1.1, cex.lab=1.3, main="SB/SBF=0")
    })
    
    output$plot_hcr <- renderPlot({
      plot_model_based_hcr(stock=stock(), mp_params=get_mp_params(), timestep=timestep()+1, cex.axis=1.1, cex.lab=1.3, main="The HCR")
    })
    
    output$plot_arrow <- renderPlot({
      plot_hcr_intro_arrow(stock=stock(), timestep=timestep()+1-get_mp_params()$timelag) # Watch the timelag here
    })
    

  } # End of server function

  # Run the app
  shinyApp(ui, server)
}

