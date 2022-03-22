# The Introduction to HCRs app
# intro_hcr.R 

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: Disintegration Dubs by G36 vs JK Flesh
# Distributed under the terms of the GNU General Public License GPL (>= 3)

# Note use of spurious importFrom to remove spurious note in R CMD check --as-cran

#' Introduction to HCRs app launcher
#' 
#' Launches the introduction to HCRs Shiny app.
#' See the 'Information' tab in the app for more information.
#' Also see the package vignette (\code{vignette("intro_hcr", package="AMPLE")}) for a tutorial.
#' @param ... Not used
#' @importFrom markdown "renderMarkdown"
#' @examples
#' \dontrun{intro_hcr()}
#' @export
intro_hcr <- function(...){

  # User interface ----
  ui <- navbarPage(
    title="Introduction to HCRs",
    tabPanel(title = "What is a Harvest Control Rule?",
      # Initiate sidebarLayout - fixed sidebar for all tabs (but we can turn the interaction options on and off)
      sidebarLayout(
        sidebar_setup(
          # HCR options
          mpParamsSetterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch")),# "Threshold effort", "Constant effort")),
          br(),
          # Buttons
          tags$span(title="Go forward one year",
            actionButton("advance", "Advance")),
          tags$span(title="Reset current projection",
            actionButton("reset", "Reset")),
          br(),
          # Stochasticity module
          stochParamsSetterUI("stoch", init_biol_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE),
          br(),
          shinyscreenshot::screenshotButton(label="Take a screenshot", filename="intro_hcr", scale=2)
        ),


        mainPanel(
          # Set up the main panel
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
          )
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of main Panel
    
    tabPanel(title = "Information",
      sidebarLayout(
        sidebar_setup(
          br()
        ),
        mainPanel(
          # Should work with devtools and after installation 
          shiny::includeMarkdown(system.file("introtext", "intro_hcr.md", package="AMPLE")),
          h1("Tutorial"),
          p("A more detailed tutorial can be found at this link:"),
          # For deployment premake the vignette and drop it into the inst/www folder
          a("Tutorial (html version)", target="_blank", href="img/intro_hcr.html"),
          # For CRAN submission
          br(),
          br()
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of Settings tabPanel 

    tabPanel(title = "Settings",
      sidebarLayout(
        sidebar_setup(
          br()
        ),
        mainPanel(
          # Life history parameters projection options
          stockParamsSetterUI("stock")
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
    # Use: get_mp_params() to evaluate the reactiveExpr (evaluates switcheroo function and returns the parameters)
    # Evaluating reactiveExpr can only be done inside a reactive consumer (like an observer or reactive)
    get_stoch_params <- stochParamsSetterServer("stoch")
    get_mp_params <- mpParamsSetterServer("mpparams", get_stoch_params) # Pass in stoch params so they're together
    get_stock_params <- stockParamsSetterServer("stock", get_stoch_params)
    niters <- 1 # Always 1 for this app

    # Make instance of the stock
    # This is just a normal stock that can be used outside of shiny purposes
    # Need isolate() as not a reactive context
    stock_noreactive <- Stock$new(stock_params = isolate(get_stock_params()), mp_params = isolate(get_mp_params()), niters = niters)
    # Make a reactive version by calling the reactive() method
    # Note that they are linked! If you change a value in stock_noreactive - you also change it in stock
    # stock is method reactive. Evaluate method through the stock() gets you stock_reactive + the reactive var
    stock <- stock_noreactive$reactive()
    # Alternative - do it all at once have a non reactive version is quite useful for getting dims etc
    #stock <- Stock$new(stock_params = isolate(get_stock_params()), mp_params = isolate(get_mp_params()), niters = niters)$reactive()
    
    # Set up the timestep - initial value is last historical timestep
    timestep <- reactiveVal(isolate(get_stock_params()$last_historical_timestep))
    
    # Reset observer - clears out the stock information
    # Any invalid reactive objects in this observer will trigger this, including an invalid stock.
    # For example, when stock() becomes invalid because project() method has been called.
    # What can trigger the reset:
    # - changing stock and MP params
    # - pressing the reset button
    observe({
      #message("In reset observer")
      input$reset
      stock_params <- get_stock_params()
      mp_params <- get_mp_params()
      # Reset the timestep
      timestep(get_stock_params()$last_historical_timestep)
      # Use isolate else this is triggered when stock becomes invalid (i.e. after project) 
      isolate(stock()$reset(stock_params = stock_params, mp_params = mp_params, niters=niters))
    }, label="resetter")
    
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
    
    output$plot_catch <- renderPlot({
      plot_catch_hcr(stock=stock(), mp_params=get_mp_params(), timestep=timestep(), cex.axis=1.1, cex.lab=1.3, main="Catch")
    })
    
    output$plot_biomass <- renderPlot({
      plot_biomass(stock=stock(), mp_params=get_mp_params(), cex.axis=1.1, cex.lab=1.3, main="Biomass", ylab="Biomass")
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

