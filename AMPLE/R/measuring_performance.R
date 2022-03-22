# The Measuring Performance app
# measuring_performance.R

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: Bad Magic by Motorhead
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' Measuring performance app launcher
#' 
#' Launches the 'Measuring Performance' Shiny app.
#' See the 'Information' tab in the app for more information.
#' Also see the package vignette (\code{vignette("measuring_performance", package="AMPLE")}) for a tutorial.
#' @param ... Not used
#' @examples
#' \dontrun{measuring_performance()}
#' @export
measuring_performance <- function(...){

  # User interface ----
  ui <- navbarPage(
    title="Measuring HCR performance",
    tabPanel(title = "How to measure performance",
      # Initiate sidebarLayout
      sidebarLayout(
        sidebar_setup(
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
          stochParamsSetterUI("stoch", init_biol_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE),
          br(),
          shinyscreenshot::screenshotButton(label="Take a screenshot", filename="meas_perf", scale=2)
        ),

        mainPanel(
          column(6,
            fluidRow(
              plotOutput("plot_catch",height="300px"),
            ), 
            fluidRow(
              plotOutput("plot_biomass",height="300px"),
            ), 
            fluidRow(
              plotOutput("plot_cpue",height="300px")
            ) 
          ),
          column(6,
            fluidRow(plotOutput("plot_hcr")),
            fluidRow(  
              textOutput("print_iter"),
              radioButtons(inputId="table_choice", label="Table selection", inline=TRUE, choiceNames=c("Each replicate", "Performance indicators"), choiceValues=c("reps", "pis"), selected="reps"),
              conditionalPanel(condition="input.table_choice == 'reps'",
                tags$span(title="The final values of SB/SBF=0, catch and relative CPUE of each replicate. The final row shows the median of the final values and the values in the brackets are the 10-90 percentiles respectively.",
                tableOutput("rep_table"))),#, style = "font-size:100%")),
              conditionalPanel(condition="input.table_choice == 'pis'",
                tags$span(title="A table of various performance indicators calculated over the short-, medium- and long- term. The value is the median. The values in the brackets are the 10-90 percentiles respectively. See the information tab for more details",
                tableOutput("pi_table")))#, style = "font-size:100%"))
            )
          )
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of tabPanel 1
    
    tabPanel(title = "Information",
      sidebarLayout(
        sidebar_setup(
          br()
        ),
        mainPanel(
          # Should work with devtools and after installation 
          shiny::includeMarkdown(system.file("introtext", "measuring_performance.md", package="AMPLE")),
          # For deployment premake the vignette and drop it into the inst/www folder
          h1("Tutorial"),
          p("A more detailed tutorial can be found at this link:"),
          a("Tutorial (html version)", target="_blank", href="img/measuring_performance.html"),
          br(),
          br()
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of Settings tabPanel 

    tabPanel(title = "Settings",
      sidebarLayout(
        sidebar_setup(
        ),
        mainPanel(
          br(),
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

    # Get the modules
    get_stoch_params <- stochParamsSetterServer("stoch")
    get_mp_params <- mpParamsSetterServer("mpparams", get_stoch_params)
    get_stock_params <- stockParamsSetterServer("stock", get_stoch_params)
    max_iters <- 100 # If you click more than this I'd be surprised... This could be an app option?

    # Make instance of the stock
    stock_noreactive <- Stock$new(stock_params = isolate(get_stock_params()), mp_params = isolate(get_mp_params()), niters = max_iters)
    # Make a reactive version by calling the reactive() method (or do it all at once)
    stock <- stock_noreactive$reactive()
    
    # Set up counter for iters
    iter <- reactiveVal(0)
    
    # Reset observer - clears out the stock information
    observe({
      #message("In reset observer")
      input$reset
      stock_params <- get_stock_params()
      mp_params <- get_mp_params()
      # Reset the iter counter
      iter(0)
      # Use isolate else this is triggered when stock becomes invalid (i.e. after project) 
      isolate(stock()$reset(stock_params = stock_params, mp_params = mp_params, niters=max_iters))
    }, label="resetter")
    
    
    observeEvent(input$project, {
      # If space advance the iter and project
      if(iter() < max_iters){
        iter(iter()+1)
        # If max out the number of iters - the stock for the last iter keeps changing as it keeps being projected
        timesteps <- c(stock()$last_historical_timestep+1,dim(stock()$biomass)[2])
        # Call the project() method. This invalidates the stock() object
        # (by internally changing the reactiveDep field).
        # The invalidated stock can then trigger other stuff
        stock()$project(timesteps=timesteps, mp_params=get_mp_params(), iters=iter())
      }
    })
    
    #---------------------------------------------------------------
    # Output stuff
    
    quantiles <- c(0.05, 0.95)
    max_spaghetti_iters <- 50
    lhs_mar <- c(5.1,4.1,1,2.1) # Margins for the LHS plots - reduced top
    cex_axis <- 1.1
    cex_lab <- 1.3
    
    
    output$print_stock <- renderTable({
      # This output is triggered if stock is invalidated, i.e. through the project() method
      stock_temp <- stock()
      stock_temp$as_data_frame()
    })
    
    output$print_iter <- renderText({
      return(paste("Replicate: ", iter(), sep=""))
    })
    
    output$plot_catch <- renderPlot({
      # Par reset
      parmar <- par()$mar
      opar <- par(mar=lhs_mar)
      on.exit(par(opar))
      
      iter_range <- 1:max(iter(),1) # When we start iter() = 0 - and we just to show the catch history
      plot_catch_iters(stock=stock(), mp_params=get_mp_params(), iters=iter_range, max_spaghetti_iters = max_spaghetti_iters, quantiles=quantiles, show_time_periods = TRUE, cex.axis=1.1, cex.lab=1.3)
    })
    
    output$plot_biomass <- renderPlot({
      # Par reset
      parmar <- par()$mar
      opar <- par(mar=lhs_mar)
      on.exit(par(opar))
      
      iter_range <- 1:max(iter(),1) # When we start iter() = 0 - and we just to show the catch history
      plot_biomass(stock=stock(), mp_params=get_mp_params(), ylab="True biomass", iters=iter_range, max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, show_time_periods = TRUE, cex.axis=1.1, cex.lab=1.3) # Other args sent to plot function
    })
    
    output$plot_cpue <- renderPlot({
      # Par reset
      parmar <- par()$mar
      opar <- par(mar=lhs_mar)
      on.exit(par(opar))
      
      iter_range <- 1:max(iter(),1) # When we start iter() = 0 - and we just to show the catch history
      plot_cpue(stock=stock(), mp_params=get_mp_params(), iters=iter_range, max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, show_time_periods = TRUE, cex.axis=1.1, cex.lab=1.3)
    })
    
    output$plot_hcr <- renderPlot({
      # Par reset
      parmar <- par()$mar
      opar <- par(mar=lhs_mar)
      on.exit(par(opar))
      
      plot_model_based_hcr(stock=stock(), mp_params=get_mp_params(), iter=iter(), cex.axis=1.1, cex.lab=1.3)
    })
    
    # Tables
    output$rep_table<- renderTable({
      if(iter() < 1){
        return(NULL)
      }
      iter_range <- 1:max(iter(),1) # When we start iter() = 0 - and we just to show the catch history
      stock()$replicate_table(iters=iter_range, quantiles=quantiles)
    },
    bordered = TRUE,
    rownames = FALSE,
    caption= "Performance of each replicate",
    auto=TRUE)
    
    output$pi_table<- renderTable({
      if(iter() < 1){
        return(NULL)
      }
      iter_range <- 1:max(iter(),1) # When we start iter() = 0 - and we just to show the catch history
      stock()$pi_table(iters=iter_range, quantiles=quantiles)
    },
    bordered = TRUE,
    rownames = TRUE,
    caption= "Performance indicators",
    auto=TRUE)
    
    

  } # End of server function

  # Run the app
  shinyApp(ui, server)
}

