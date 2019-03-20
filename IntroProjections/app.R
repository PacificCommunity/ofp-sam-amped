# Copyright 2018 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC

# Load packages ----
library(shiny)
library(tidyr) # Could Try to avoid it and reduce weight of packages
library(dplyr) # Just used for bind_rows() at the moment - change data structure of PIs to avoid this
library(ggplot2)
library(RColorBrewer)

# Source helpers ----
source("../R/funcs.R")
source("../R/plots.R")
source("../R/modules.R")

# UI
ui <- navbarPage(
  title="Introducing Projections",
  tabPanel("Projections",
    # Controls and what not
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        # HCR is either constant catch or constant effort
        mp_params_setterUI("mpparams", mp_visible=c("Constant effort", "Constant catch"), title="Select projection type: catch or effort", input_label="Projection type", init_constant_catch=50, init_constant_effort=1.0),
        br(),
        # Controls for management cycle length
        tags$span(title="Step length",
          numericInput("projlen", label="Step length", value=3, min = 1, step = 1)
        ),
        br(),
        # Buttons:
        # Advance one management cycle
        # New projection - unless this is triggered automagically when we reach the end of the current projection?
        # Reset all projections
        tags$span(title="Project one management cycle.",
          actionButton("project", "Project")
        ),
        tags$span(title="Set up next projection",
          actionButton("setnext", "Set up next projection")
        ),
        tags$span(title="Reset all projections.",
          actionButton("reset", "Reset all")
        ),
        br(),
        br(),
        # Majuro / Kobe option
        selectInput("kobemajuro", "Plot selection", c("Kobe" = "kobe", "Majuro" = "majuro", "Yield curve" = "yieldcurve")),
        br(),


        # Stochasticity options
        # ***********************************************************************
        # Can we edit this so that it only shows only the biological variability?
        stoch_params_setterUI("stoch", init_prod_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE)
        # ***********************************************************************
      ),
      #
      mainPanel(width=9,
        fluidRow(
          # Top left - 3 stacked plots
          # SB/SBF=0
          # Catch
          # Effort (relative to something)
          column(6,
            plotOutput("plotall",height="500px")
          ),
          # Top right
          # Kobe / Majuro (with switch)
          column(6,
            plotOutput("MajKobeplot",height="500px")
          )
        ),
        # PI Table underneath
        fluidRow(
          column(12,
            textOutput("currentts"),
            tableOutput("pis")#,
            #dataTableOutput("pisdt")
          )
        )
      )
    )
  ),
  tabPanel("Settings",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br()
      ),
      mainPanel(width=9,
        fluidRow(column=12,
          #stoch_params_setterUI("stoch", init_prod_sigma=0.15, init_est_sigma=0.03, init_est_bias=0.0),
          stock_params_setterUI("stock"),
          # Total number of years (including historical years)
          numericInput("nyears", "Number of years", value = 50, min=20, max=100, step=1)
        )
      )
    )
  ),
  tabPanel("Information",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        maintainer_and_licence()
      ),
      mainPanel(width=9,
        h1("Instructions"),
        p("Notes")
      )
    )
  )
)

# Do studd
server <- function(input, output,session) {

  # Global settings - year range and quantiles
  app_params <- list(initial_year = 2009, last_historical_timestep = 10)
  app_params$historical_timesteps <- 1:app_params$last_historical_timestep
  #app_params$current_timestep <- app_params$last_historical_timestep + 1
  quantiles <- c(0.20, 0.80)

  # Getting the stochasticity parameters
  # These are located in the Projections tab
  # We only want biological variability - is this still OK?
  get_stoch_params <- callModule(stoch_params_setter, "stoch") 
  # Getting the life history parameters
  # These are located in the Settings tab
  get_lh_params <- callModule(stock_params_setter, "stock") 
  # Join these together into a single object to be passed to the funcs - bit clumsy as I will have to do this in all the servers
  get_stock_params <- reactive({
    sp <- get_stoch_params()
    lh <- get_lh_params()
    out <- c(sp, lh)
    return(out)
  })

  # Get the management procedure parameters
  # These are located in the Projections tab
  get_mp_params <- callModule(mp_params_setter, "mpparams") 

  # Make the stock - empty and with no dims
  stock <- create_stock()

  # We have 1 initial iter. More are added to it but this is accounted for using the iter() function
  # iter is really a separate projection - called iter for use with existing code
  niters <- 1 

  # Initialise the stock with a appropriate history
  # Use isolate else error (function tries to be reactive to changes in get_stock_params() and stock itself
  # Do we need this here? Isn't it already handled in the reset procedure?
  isolate(reset_stock(stock=stock, stock_params = get_stock_params(), mp_params=get_mp_params(), app_params=app_params, initial_biomass=get_stock_params()$b0, nyears=input$nyears, niters=niters))
  
  # Set iter as a reactive value. If this changes, it triggers other stuff
  iter <- reactiveVal(1) 
  current_timestep <- reactiveVal(app_params$last_historical_timestep + 1)

  # PIs for a single projection
  pitemp <- reactiveVal(NULL)

  # Reset procedure
  # Clear out stock history for all iterations
  observe({
    # ***************************************************
    # req is a check for required values - if not about - fail
    # not sure why this check is here - if we remove is it problem
    # nyears is set in the Settings tab
    req(input$nyears)
    # ***************************************************

    # If HCR changes, stock LH changes or reset button is pressed the reset procedure gets triggered
    #mp_params <- get_mp_params()
    stock_params <- get_stock_params() 
    input$reset # reset above on Projections tab
    # Reset iterations and current_timestep and PIs
    iter(1)
    current_timestep(app_params$last_historical_timestep + 1)
    pitemp(NULL)
    # Need isolate here because calling reset_stock() causes stock to change which triggers this observe() resulting
    # in an infinite loop
    isolate(reset_stock(stock=stock, stock_params=stock_params, mp_params=get_mp_params(), app_params=app_params, initial_biomass=stock_params$b0, nyears=input$nyears, niters=niters))
  })
  

  observeEvent(input$setnext, {
    # What to do if we haven't finished the current one?
    # Button does not nothing - you have to complete the current projection
    if(current_timestep() < dim(stock$biomass)[2]){
      return()
    }

    # Reset time, add another iter and add another row to the stock
    current_timestep(app_params$last_historical_timestep + 1)
    iter(iter() + 1)

    # If nrow stock < iter, add a row biomass, biomass_obs and catch
    # This row will contain the values for the new iteration
    if (nrow(stock$biomass) < iter()){
      # rbind is dropping dimnames - really annoying - so hack
      dnames <-names(dimnames(stock$biomass)) 
      stock$biomass <- rbind(stock$biomass,stock$biomass[1,])
      stock$hcr_ip<- rbind(stock$hcr_ip,stock$hcr_ip[1,])
      stock$hcr_op<- rbind(stock$hcr_op,stock$hcr_op[1,])
      stock$catch <- rbind(stock$catch,stock$catch[1,])
      stock$effort <- rbind(stock$effort,stock$effort[1,])
      # Empty out future years
      stock$biomass[iter(),(app_params$last_historical_timestep+2):dim(stock$biomass)[2]] <- NA
      stock$hcr_ip[iter(),(app_params$last_historical_timestep+2):dim(stock$hcr_ip)[2]] <- NA
      stock$hcr_op[iter(),(app_params$last_historical_timestep+2):dim(stock$hcr_op)[2]] <- NA
      stock$catch[iter(),(app_params$last_historical_timestep+1):dim(stock$catch)[2]] <- NA
      stock$effort[iter(),(app_params$last_historical_timestep+1):dim(stock$effort)[2]] <- NA
      # Bit hacky but rbind is dropping the dimnames names - annoying
      names(dimnames(stock$biomass)) <- dnames 
      names(dimnames(stock$effort)) <- dnames 
      names(dimnames(stock$hcr_ip)) <- dnames 
      names(dimnames(stock$hcr_op)) <- dnames 
      names(dimnames(stock$catch)) <- dnames 
    }
  })


  # Projection procedure
  # The main event
  observeEvent(input$project, {
    # If we have reached end of simulation, don't do anything
    if (current_timestep() > dim(stock$biomass)[2]){
      return()
    }
    # Pass current iter to project()
    # project() takes entire stock
    # Here we only want the current iteration
    # (subset out using fancy lapply with [ operator)
    stock_iter <- lapply(reactiveValuesToList(stock), '[', iter(),,drop=FALSE) 
    # Update hcr_op  - # But if noise added here it will result in different value?
    mp_params <- get_mp_params()
    stock_iter$hcr_op[,current_timestep()] <- get_hcr_op(stock=stock_iter, stock_params=get_stock_params(), mp_params, yr=current_timestep()-mp_params$timelag)
    max_timestep <- min(dim(stock$biomass)[2],current_timestep() + input$projlen - 1)
    out <-  project(stock_iter,
                    timesteps=c(current_timestep(), max_timestep),
                    stock_params=get_stock_params(),
                    mp_params=mp_params,
                    app_params=app_params)
    # Update whole timeseries or just new elements?
    stock$biomass[iter(),] <- out$biomass
    stock$effort[iter(),] <- out$effort
    stock$hcr_ip[iter(),] <- out$hcr_ip
    stock$hcr_op[iter(),] <- out$hcr_op
    stock$catch[iter(),] <- out$catch
    # Update the timestep
    current_timestep(max_timestep+1)
  })

  # ************************************************
  # Output stuff
  output$plotall <- renderPlot({
    plot_projection(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, show_last=TRUE, max_spaghetti_iters=500, quantiles=quantiles, cex.lab=1.5, cex.axis=1.5)
  })

  output$MajKobeplot  <- renderPlot({
    if(input$kobemajuro == "kobe"){
      plot_kobe_projections(stock=stock, stock_params=get_stock_params())
    }
    if(input$kobemajuro == "majuro"){
      plot_majuro_projections(stock=stock, stock_params=get_stock_params())
    }
    if(input$kobemajuro == "yieldcurve"){
      plot_yieldcurve_projections(stock=stock, stock_params=get_stock_params(), app_params=app_params)
    }
  })

  output$pis <- renderTable({
    get_projection_pis(stock=stock, stock_params=get_stock_params(), app_params=app_params, current_timestep=current_timestep())
    },
    caption= "Performance indicators",
    auto=TRUE
  )

  #output$pisdt <- renderDataTable({
  #  get_projection_pis(stock=stock, stock_params=get_stock_params(), app_params=app_params, current_timestep=current_timestep())
  #  }
  #)

  output$currentts  <- renderText({
    paste("Start of year: ", min(current_timestep(), dim(stock$biomass[2])) + app_params$initial_year-1, sep="")
  })
}

# Run the app
shinyApp(ui, server)
