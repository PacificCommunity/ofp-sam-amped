# Put together the Introduction to Indicators app

#' Introduction to indicators app launcher
#' 
#' Launches the introduction to indicators Shiny app.
#' @param ... Not used
#' @export
introHCR <- function(...){

  # User interface ----
  ui <- navbarPage(
    title="navbarPage title",
    tabPanel(title = "tabPanel 1 title",
      # Initiate sidebarLayout
      sidebarLayout(
        intro_hcr_sidebar_setup(
          # MP selector
          mpParamsSetterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch")),# "Threshold effort", "Constant effort")),
          br(), # Could add br() automatically to side bar set up to separate each component?
          # Buttons
          tags$span(title="Run the full projection",
            actionButton("project", "Run projection")),
          tags$span(title="Reset all projections",
            actionButton("reset", "Reset")),
          br(),
          # Stochasticity module
          stochParamsSetterUI("stoch", init_biol_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE)
        ),

        mainPanel(
          # Just put here for testing right now
          tableOutput("printstock")
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of tabPanel 1

    tabPanel(title = "Settings",
      sidebarLayout(
        intro_hcr_sidebar_setup(
          br()
        ),
        mainPanel(
          br(),
          # Life history parameters projection options
          stockParamsSetterUI("stock")
        ) # End of mainPanel
      ) # End of sidebarLayout
    ) # End of Settings tabPanel
  ) # End of navbarPage
  
  #--------------------------------------------------------------------------

  # Start of server function
  server <- function(input, output,session) {

    # Get the modules
    get_stoch_params <- stochParamsSetterServer("stoch")
    get_mp_params <- mpParamsSetterServer("mpparams", get_stoch_params)
    get_stock_params <- stockParamsSetterServer("stock", get_stoch_params)
    max_iters <- 3#200 # If you click more than this I'd be surprised... This could be an app option?

    # Make instance of the stock
    stock_noreactive <- Stock$new(stock_params = isolate(get_stock_params()), mp_params = isolate(get_mp_params()), niters = max_iters)
    # Make a reactive version by calling the reactive() method (or do it all at once)
    stock <- stock_noreactive$reactive()
    
    # Set up counter for iters
    iter <- reactiveVal(0)
    
    # Reset observer - clears out the stock information
    observe({
      message("In reset observer")
      input$reset
      stock_params <- get_stock_params()
      mp_params <- get_mp_params()
      # Reset the iter counter
      iter(0)
      # Use isolate else this is triggered when stock becomes invalid (i.e. after project) 
      isolate(stock()$reset(stock_params = stock_params, mp_params = mp_params, niters=max_iters))
    }, label="resetter")
    
    
    observeEvent(input$project, {
      # Advance the iter 
      if(iter() < max_iters){
        iter(iter()+1)
      }
      timesteps <- c(stock()$last_historical_timestep+1,dim(stock()$biomass)[2])
      # Call the project() method. This invalidates the stock() object
      # (by internally changing the reactiveDep field).
      # The invalidated stock can then trigger other stuff
      stock()$project(timesteps=timesteps, mp_params=get_mp_params(), iters=iter())
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
      plot_catch(stock=stock(), mp_params=get_mp_params(), timestep=timestep(), cex.axis=1.1, cex.lab=1.3)
    })
    
    output$plot_biomass <- renderPlot({
      plot_biomass(stock=stock(), mp_params=get_mp_params(), cex.axis=1.1, cex.lab=1.3) # Other args sent to plot function
    })
    
    output$plot_hcr <- renderPlot({
      plot_model_based_hcr(stock=stock(), mp_params=get_mp_params(), timestep=timestep()+1, cex.axis=1.1, cex.lab=1.3)
    })
    
    output$plot_arrow <- renderPlot({
      plot_hcr_intro_arrow(stock=stock(), timestep=timestep()+1-get_mp_params()$timelag) # Watch the timelag here
    })
    

  } # End of server function

  # Run the app
  shinyApp(ui, server)
}

