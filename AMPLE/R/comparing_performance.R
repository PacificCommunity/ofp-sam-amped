# The Comparing Performance app
# comparing_performance.R 

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: Fire by The Bug
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' 'Comparing HCR Performance' app launcher
#' 
#' Launches the Comparing Performance Shiny app.
#' @param ... Not used
#' @export
comparing_performance <- function(...){
  # User interface ----
  ui <- navbarPage(
    title="Comparing HCR performance",
    tabPanel(title = "HCR selection",
      shinyjs::useShinyjs(), # So we can enable / disable buttons
      sidebarLayout(
        sidebar_setup(
          # HCR options
          mpParamsSetterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch")),# "Threshold effort", "Constant effort")),
          br(), # Could add br() automatically to side bar set up to separate each component?
          actionButton("project", "Project", icon=icon("fish")), 
          br(),
          br(),
          textInput(inputId="user_hcr_name", label="HCR Display Name (optional)", value=as.character(NA), placeholder="Name of HCR", width='50%'), 
          actionButton("add_basket", "Add HCR to basket", icon=icon("shopping-basket")),
          br(),
          # How many HCRs do we have in the store
          textOutput("print_number_hcrs"),
          br(),
          # This should reset everything - empty the stores
          actionButton("empty_basket", "Empty basket"),
          br()
        ), # End sidebar set up
        mainPanel(
          column(6,
            fluidRow(
              plotOutput("plot_catch"),#,height="250px"),
            ), 
            fluidRow(
              plotOutput("plot_biomass"),#,height="250px"),
            ), 
            fluidRow(
              plotOutput("plot_cpue"),#,height="250px")
            ) 
          ),
          column(6,
            fluidRow(plotOutput("plot_hcr")),
            fluidRow(tableOutput("pi_table"))
          )
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of front page tabPanel
    
    tabPanel(title = "Compare results",
      # Set up sub tabs
      sidebarLayout(
        sidebar_setup(
          # PI choice
          checkboxGroupInput(inputId = "pi_choice", label="PI selection", inline=TRUE, 
                             # character(0) means no choice is available - updated in server function
                             choices = character(0)),
          br(),
          # Dynamic HCR choice
          checkboxGroupInput(inputId = "hcr_choice", label="HCR selection",
                             # character(0) means no choice is available - updated in server function
                             choiceNames = character(0), choiceValues = character(0)),
          br()
        ),# End of sidebar_setup()
        mainPanel(
          tabsetPanel(id="comparisontabs",
            tabPanel(title="Performance indicators - medians", value="PImeds",
              column(12, fluidRow( 
                tags$span(title="Bar plot of the median values of the performance indicators over the three time periods. Note that the lower the PI for relative effort is, the better the HCR is thought to be performing. Also, a high value for SB/SBF=0 may not indicate that the HCR is performing well - it depends on your objectives.",
                plotOutput("plot_bar_comparison"))
              ))
            ), # End of median bar chart tab panel
            tabPanel(title="Performance indicators - boxplots", value="PIbox",
              column(12, fluidRow(
                tags$span(title="Box plot of the values of the performance indicators over the three time periods. Note that the lower the PI for relative effort is, the better the HCR is thought to be performing. Also, a high value for SB/SBF=0 may not indicate that the HCR is performing well - it depends on your objectives. The box contains the 20-80 percentiles, the tails the 5-95 percentiles.",
                plotOutput("plot_box_comparison"))
              ))
            ), # End of box plot panel
            tabPanel(title="Performance indicators - table", value="PItable",
              column(12, fluidRow(
                br(),
                "Performance indicators in the short-, medium- and long-term. The value is the median, the values in the brackets are the 5 and 95 percentiles (i.e. cover 90% of the range of values).",
                br(),
                tags$span(title="Peformance indicators in the short-term.", tableOutput("pi_table_all_hcrs_short")),
                tags$span(title="Peformance indicators in the medium-term.", tableOutput("pi_table_all_hcrs_medium")),
                tags$span(title="Peformance indicators in the long-term.", tableOutput("pi_table_all_hcrs_long"))
              ))
            ) # End of PI table panel
          ) # End of tabsetPanel
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of Compare results tabPanel

    tabPanel(title = "Settings",
      sidebarLayout(
        sidebar_setup(
        ),
        mainPanel(
          br(),
          # Life history parameters projection options
          stockParamsSetterUI("stock"),
          br(),
          # Number of iteration
          numericInput("niters", "Number of iterations", value = 100, min=10, max=1000, step=10),
          # Stochasticity module 
          stochParamsSetterUI("stoch", init_biol_sigma=0.2, init_est_sigma=0.0, init_est_bias=0.0, show_var=TRUE),
          br()
        ) # End of mainPanel
      ) # End of sidebarLayout
    ) # End of Settings tabPanel



  ) # End of navbarPage
  
  #--------------------------------------------------------------------------

  # Start of server function
  server <- function(input, output,session) {
    
    pi_quantiles <- c(0.05, 0.10, 0.90, 0.95)

    # Evaluating reactiveExpr can only be done inside a reactive consumer (like an observer or reactive)
    get_stoch_params <- stochParamsSetterServer("stoch")
    get_mp_params <- mpParamsSetterServer("mpparams", get_stoch_params)
    get_stock_params <- stockParamsSetterServer("stock", get_stoch_params)
    hcr_no <- reactiveVal(0)
    all_pis <- reactiveVal(data.frame())
    
    
    

    # Make a normal stock that can be used outside of shiny purposes
    stock_noreactive <- Stock$new(stock_params = isolate(get_stock_params()), mp_params = isolate(get_mp_params()), niters = isolate(input$niters))
    # Make a reactive version by calling the reactive() method (or do it all at once)
    stock <- stock_noreactive$reactive()
    
    # Update the available PIs checkboxes - although this doesn't really dynamically change
    # It just saves having to maintain a list in the UI at the top AND in the PI calculation function
    # Because the options come from the pistore and if no pistore yet, no names
    # Drop F/FMSY and others from list
    
    # Get available PI names generated by the performance_indicators() method
    all_pi_names <- unique(stock_noreactive$performance_indicators()$pi)
    pi_choices <- all_pi_names # Could drop some here if you want
    updateCheckboxGroupInput(session, "pi_choice",
                             choices = pi_choices,
                             selected = pi_choices
    )
    
    # Reset observer
    # What can trigger the reset:
    # - changing stock and MP params
    # - emptying the basket
    observe({
      message("In stock reset observer")
      input$confirm_empty
      stock_params <- get_stock_params()
      mp_params <- get_mp_params()
      niters <- input$niters
      # Use isolate else these are triggered when they become invalid (i.e. after project) 
      isolate(stock()$reset(stock_params = stock_params, mp_params = mp_params, niters=niters))
      
      # Turn off add_basket button after stock has been cleared
      # Activates again when project has been called
      shinyjs::disable(id = "add_basket")
      shinyjs::disable(id = "user_hcr_name")
    }, label="stock_resetter")
    
    
    # Are you sure you want to empty the basket?
    observeEvent(input$empty_basket, {
      showModal(modalDialog(
        title="Are you sure you want to empty the basket?",
        footer = tagList(actionButton("confirm_empty", "I am sure - empty it!"), modalButton("Ooops, no. Keep my beautiful HCRs")),
        fade=FALSE, easyClose=FALSE
      ))
    })
    
    # Remove the modal dialogue after confirming to empty the basket
    observeEvent(input$confirm_empty,
    {
      message("Confirming empty basket")
      removeModal() 
    })
    
    
    # Reset the basket if user confirms or if stock_params are updated.
    observe({
      message("In PIs reset observer")
      input$confirm_empty # After confirming that to empty the basket
      stock_params <- get_stock_params()
      isolate(hcr_no(0))
      isolate(all_pis(data.frame()))
    }, label="pis_resetter")
    
    
    # After projecting, alloq users to add it to the basket with a name
    observeEvent(input$project, {
      timesteps <- c(stock()$last_historical_timestep+1,dim(stock()$biomass)[2])
      stock()$project(timesteps=timesteps, mp_params=get_mp_params())
      shinyjs::enable("add_basket")
      shinyjs::enable("user_hcr_name")
    })
    
    # If you like the results, add them to the basket
    observeEvent(input$add_basket, {
      # Update the numbers of HCRs in basket
      hcr_no(hcr_no() + 1)
      # Extract and save results from each model for plots - just PIs? Or time series too?
      # Depends if we want to plot the time series? Maybe not. Look like shite anyway!
      pis <- stock()$performance_indicators(quantiles=pi_quantiles)
      # Reshape here to make wide - spread out the quantiles
      pis <- reshape(data = pis, direction = "wide", timevar = "quantiles", idvar = c("pi", "time_period"), v.names = "value", sep="_")
      # Add in column of HCR number
      pis$hcr_no <- hcr_no()
      # Also HCR name and details
      hcr_ref <- input$user_hcr_name
      # If no name given by user make one
      if(hcr_ref== "" || is.na(hcr_ref)){
        hcr_ref <- paste("HCR ", hcr_no(), sep="") # Used for legends
      } else {
        hcr_ref <- paste(hcr_no(), hcr_ref, sep=" ") 
        # Clear out any user defined name
        updateTextInput(session,"user_hcr_name",value="")
      }
      pis$hcr_ref <- hcr_ref
      pis$hcr_details <- paste(hcr_ref, ".<br>",get_mp_params()$name,sep="") # Use <br> for html linebreak
      
      
      all_pis(rbind(all_pis(), pis))
      # You can't store again until you project again
      shinyjs::disable("add_basket")
      shinyjs::disable("user_hcr_name")
      
      
    })
    
    # Update the HCR selector when all_pis() gets updated (after adding to basket, or emptying basket)
    observeEvent(all_pis(),{
      # If no HCRs in list
      if(nrow(all_pis()) < 1){
        hcr_details <- character(0)
        hcr_nos <- character(0)
        selected <- NULL
      } else {
        # Three columns of HCR info:
        # hcr_no (numeric 1 - X),
        # hcr_ref (user name - use for labelling plots), hcr_details (used for HCR selector)
        # hcr_details (Used for labelling the selector)
        hcr_details <- unique(as.character(all_pis()$hcr_details))
        # Need to turn details into HTML so we can use the <br> as a line break
        hcr_details <- lapply(hcr_details, HTML) # To use <br> line break
        hcr_nos <- unique(all_pis()$hcr_no)
        selected <- c(input$hcr_choice, max(hcr_nos))
      }
      # Figure which HCRs are selected
      # Update HCR choice in selector
      updateCheckboxGroupInput(session, "hcr_choice",
                               selected = selected,
                               choiceNames = hcr_details,
                               choiceValues = hcr_nos 
      )
    })
    
    #---------------------------------------------------------------
    # Output stuff
    max_spaghetti_iters <- 50
    quantiles <- c(0.05, 0.95)
    
    
    output$print_number_hcrs<- renderText({
      return(paste("Number of HCRs in basket: ", hcr_no(), sep=""))
    })
    
    output$pi_table<- renderTable({
      # Is there future data in the stock? Ugly check
      if(is.na(stock()$catch[1,stock()$last_historical_timestep+1])){
        return(NULL)
      }
      stock()$pi_table(quantiles=quantiles)
    },
    bordered = TRUE,
    rownames = TRUE,
    caption= "Performance indicators",
    auto=TRUE)
    
    output$plot_catch <- renderPlot({
      plot_catch_iters(stock=stock(), mp_params=get_mp_params(), max_spaghetti_iters = max_spaghetti_iters, quantiles=quantiles, show_time_periods = TRUE, cex.axis=1.1, cex.lab=1.3)
    })
    
    output$plot_biomass <- renderPlot({
      plot_biomass(stock=stock(), mp_params=get_mp_params(), ylab="True SB/SBF=0", max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, show_time_periods = TRUE, cex.axis=1.1, cex.lab=1.3) # Other args sent to plot function
    })
    
    output$plot_cpue <- renderPlot({
      plot_cpue(stock=stock(), mp_params=get_mp_params(), max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, show_time_periods = TRUE, cex.axis=1.1, cex.lab=1.3)
    })
    
    output$plot_hcr <- renderPlot({
      plot_model_based_hcr(stock=stock(), mp_params=get_mp_params(), iter=1:input$niters, cex.axis=1.1, cex.lab=1.3)
    })
    
    # Plotting the comparison bar and box plots
    # Comparison plots
    no_cols <- 2
    height_per_pi <- 200 # Could be adjustable depending on screen size?
    # Alt.
    total_height <- 800
    max_height_per_row <- total_height / 2
    
    plot_barbox_comparison <- function(plot_type, quantiles=NULL, no_cols=2){
      out <- renderPlot({
        dat <- all_pis()
        # Subset out the PIs
        dat <- subset(dat, pi %in% input$pi_choice)
        # Pass all HCRs in, as we we need to keep colours
        hcr_nos <- input$hcr_choice
        barboxplot(dat, hcr_nos, plot_type=plot_type, quantiles=quantiles, no_cols=no_cols)
      },
      height=function(){
        # Each row the same height as PIs drop out
        #return(max(height_per_pi*1.5, (height_per_pi * ceiling(length(input$pi_choice) / no_cols))))
        # Fill space
        return({
          nrows <- ceiling(length(input$pi_choice) / no_cols)
          height_per_row <- min(total_height / nrows, max_height_per_row)
          return(height_per_row * nrows)
        })
      })
      return(out)
    }
      
    output$plot_bar_comparison <- plot_barbox_comparison(plot_type="median_bar", no_cols=no_cols)
    output$plot_box_comparison <- plot_barbox_comparison(plot_type="box", quantiles = pi_quantiles, no_cols=no_cols)
    
    # Fix labeling for table captions - use the non-reactive version of the stock
    time_periods <- stock_noreactive$time_periods()
    time_period_text <- lapply(strsplit(names(time_periods), " "), function(x) {
      paste(x[1], "-term ", x[2], sep="")
    })
    
    # period is 1, 2, or 3 - for ST, MT, LT
    render_pi_table_all_hcrs <- function(period){
      out <- renderTable({
        dat <- all_pis()
        # Which time period
        tp <- sort(unique(dat$time_period))[period] # Short term is 1, MT = 2 etc
        # Subset out the PIs
        dat <- subset(dat, pi %in% input$pi_choice & hcr_no %in% input$hcr_choice & time_period == tp)
        tab <- pi_table_all_hcrs(pis = dat, quantiles = quantiles)
        return(tab)
      }, caption = time_period_text[[period]])
      return(out)
    }
    
    output$pi_table_all_hcrs_short <- render_pi_table_all_hcrs(period = 1)
    output$pi_table_all_hcrs_medium <- render_pi_table_all_hcrs(period = 2)
    output$pi_table_all_hcrs_long <- render_pi_table_all_hcrs(period = 3)
    
  } # End of server function
  

  # Run the app
  shinyApp(ui, server)
}

