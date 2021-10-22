# The Comparing Performance app
# comparing_performance.R 

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Sountrack: Fire by The Bug
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' 'Comparing HCR Performance' app launcher
#' 
#' Launches the Comparing Performance Shiny app.
#' @param ... Not used
#' @export
comparing_performance <- function(...){

  # User interface ----
  # Use navbarPage (fluidPage has a problem with title argument and tabs)
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
          textInput(inputId="user_hcr_name", label="HCR Display Name (optional)", value=as.character(NA), placeholder="Name of HCR", width='50%'), 
          actionButton("add_basket", "Add HCR to basket", icon=icon("shopping-basket")),
          br(),
          # How many HCRs do we have in the store
          textOutput("nstoredstocks"),
          br(),
          # This should reset everything - empty the stores
          actionButton("empty_basket", "Empty basket"),
          br()
        ), # End sidebar set up
        mainPanel(
          textOutput("print_number_hcrs"),
          tableOutput("pi_table")
        ) # End of mainPanel
      ) # End of sidebarLayout
    ), # End of front page tabPanel
    
    tabPanel(title = "Compare results",
      sidebarLayout(
        sidebar_setup(
        ),
        mainPanel(
          br(),
          br()
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
      pis <- stock()$performance_indicators(quantile_range=c(0.05, 0.10, 0.90, 0.95))
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
      all_pis(rbind(all_pis(), pis))
      # You can't store again until you project again
      shinyjs::disable("add_basket")
      shinyjs::disable("user_hcr_name")
    })
    
    #---------------------------------------------------------------
    # Output stuff
    
    
    output$print_number_hcrs<- renderText({
      return(paste("Number of HCRs in basket: ", hcr_no(), sep=""))
    })
    
    output$pi_table<- renderTable({
      #if(hcr_no() < 1){
      #  return()
      #}
      # Only show this after a project
      stock()$pi_table(quantile_range=c(0.05, 0.95))
    },
    bordered = TRUE,
    rownames = TRUE,
    caption= "Performance indicators",
    auto=TRUE)
    
    

  } # End of server function

  # Run the app
  shinyApp(ui, server)
}

