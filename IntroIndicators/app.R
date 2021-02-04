# Copyright 2018 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC

devtools::load_all("../AMPLE")
#library(AMPLE)

# User interface ----
ui <- navbarPage(title="Introducing Performance Indicators", id="main",
  tabPanel(title="Projections", value="projections",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        mp_params_setterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch", "Threshold effort", "Constant effort"), init_thresh_max_catch=140, init_thresh_belbow=0.5),
        br(),
        tags$span(title="Run the projection for one more replicate",
          actionButton("project", "Run projection")
        ),
        tags$span(title="Reset the projection.",
          actionButton("reset", "Reset")
        ),
        br(),
        # Stochasticity options
        stoch_params_setterUI("stoch", init_prod_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE)
      ),
      mainPanel(width=9,
        # Left hand side
        column(6,
          # The HCR plot
          fluidRow(
            tags$span(title="The HCR. The blue points show the inputs and outputs from all years of the the last replicate. The grey points show the inputs and outputs from all years from all replicates This enables you to see which parts of the HCR shape are most used.", plotOutput("plothcr")
            )
          ),
          fluidRow(
            tags$span(title="The number of replicates run so far.",
            textOutput("itercount")),
            #radioButtons(inputId="table_choice", label="Table selection", inline=TRUE, choiceNames=c("None", "Each replicate", "Performance indicators"), choiceValues=c("none", "reps", "pis"), selected="none"),
            radioButtons(inputId="table_choice", label="Table selection", inline=TRUE, choiceNames=c("Each replicate", "Performance indicators"), choiceValues=c("reps", "pis"), selected="reps"),
            conditionalPanel(condition="input.table_choice == 'reps'",
              tags$span(title="The final values of SB/SBF=0, catch and relative CPUE of each replicate. The final row shows the median of the final values and the values in the brackets are the 10-90 percentiles respectively.", tableOutput("reptable"), style = "font-size:100%")
            ),
            conditionalPanel(condition="input.table_choice == 'pis'",
              tags$span(title="A table of various performance indicators calculated over the short-, medium- and long- term. The value is the median. The values in the brackets are the 10-90 percentiles respectively. See the information tab for more details", tableOutput("hcrpis"), style = "font-size:100%")
            )
          )
        ),
        # Right hand side
        column(6,
          fluidRow(
            tags$span(title="Plot of SB/SBF=0. The black line shows the 'true' biomass in the current replicate. The blue line shows the estimated biomass in the current replicate. The grey lines show the previous replicates. The histogram shows the range of values in the final year. When enough replicates have been performed, the grey envelope contains the 10-90 percentiles.",
              plotOutput("plotbiomasshisto",height="250px")
            )
          ),
          fluidRow(
            tags$span(title="Plot of the catch. The black line shows the current replciate. The grey lines show the previous replicates. The histogram shows the range of values in the final year. When enough replicates have been performed, the grey envelope contains the 10-90 percentiles.",
              plotOutput("plotcatchhisto",height="250px")
            )
          ),
          fluidRow(
            tags$span(title="Plot of the CPUE relative to the CPUE in the year 2000. The black line shows the current replicate. The grey lines show the previous replicates. The histogram shows the range of values in the final year. When enough replicates have been performed, the grey envelope contains the 10-90 percentiles.",
              plotOutput("plotrelcpuehisto",height="250px")
            )
          )
        )
      )
    )), 
  # Tab for choosing stock parameters, stock history, no. iterations etc
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
          stock_params_setterUI("stock"),
          # Total number of years (including historical years)
          numericInput("nyears", "Number of years", value = 30, min=20, max=100, step=1)
        )
      )
    )
  ),
  tabPanel("Notes",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        amped_maintainer_and_licence()
      ),
      mainPanel(width=9,
        h1("Measuring performance"),
        p("Before a HCR is adopted its performance is tested and evaluated using computer simulations (known as Management Strategy Evaluation - MSE)."), 
        p("During these evaluations the performance of a proposed HCR is measured by using a collection of indicators, known as performance indicators. These indicators should relate to the management objectives of the fishery, e.g. stock sustainability, good economic performance etc."), 
        p("As uncertainty can affect the performance of a HCR, the evaluations are performed many hundreds of times. Each evaluation is known as a replicate. The indicators are calculated for reach replicate and summaries, such as average or median values and ranges are presented."),
        p("In this app, it is possible to see the final value of some of these indicators in the replicate table. It is also possible to see the performance indicators summarised over all the replicates."),
        h1("Including uncertainty"),
        p("Variability can be included in the projection in two ways: through variability in the stock productivity and through the estimated level of stock biomass being different to the true level of the stock biomass. These options are initially turned off. The options can be seen by clicking on the ", strong("Show variability options"), "box."),
        p("Biological productivity variability represents the variability of the natural procesess of the stock, for example growth and natural mortality. Increasing the variability will increase the 'bumpiness' of the stock trajectory. As biological variability is always encountered in fisheries it is essential that a selected HCR is robust to the variability."),
        p("Estimation error simulates the difference between the true level of the stock biomass and the estimated level. Unfortunately, the true abundance of a fish stock is never known. Instead, estimates of abundance are made, for example using stock assessment models. The HCR uses the estimated biomass, not the true biomass. This means that the catch limit that is set by the HCR is based on estimated biomass. If the biomass is estimated poorly the resulting catch limit set by the HCR may not be appropriate."),
        p("Here, estimation error is modelled using two different processes: random error and consistent bias (positive or negative). The bias represents situations where the biomass is consistently over or under estimated."),
        p("When estimation error is active the biomass plot shows two lines. The black line shows the true biomass, the blue line shwows the estimated biomass. It is the blue line that feeds the HCR. Increasing the estiamation bias and variability will increase the difference between these lines."),
        h1("Tutorial"),
        p("A more detailed tutorial can be found at these links:"),
        a("Tutorial (pdf)",target="_blank",href= "introIndicators.pdf"), 
        br(),
        a("Tutorial (html)",target="_blank",href="introIndicators.html") 
      )
    )
  ),
  tabPanel("About",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        amped_maintainer_and_licence()
      ), 
      mainPanel(width=9,
        spc_about()
      )
    )
  )
)

server <- function(input, output,session) {
  
  # Here niters is the initial number of iters in a new stock object when reset is triggered
  # App parameters
  app_params <- list(initial_year = 2009, last_historical_timestep = 10)
  app_params$historical_timesteps = 1:app_params$last_historical_timestep
  # Object to store the performance indicators - gets updated through time
  pitemp <- reactiveVal(NULL)
  get_mp_params <- callModule(mp_params_setter, "mpparams") 
  get_stoch_params <- callModule(stoch_params_setter, "stoch") 
  get_lh_params <- callModule(stock_params_setter, "stock") 
  # Join these together into a single object to be passed to the funcs
  pi_percentiles <- c(10,90)
  get_stock_params <- reactive({
    sp <- get_stoch_params()
    lh <- get_lh_params()
    out <- c(sp, lh)
    return(out)
  })
  
  # Create the stock
  stock <- create_stock()
  initial_niters <- 1 # We have 1 initial iter. More are added to it but this is accounted for using the iter() function
  isolate(reset_stock(stock = stock, stock_params = get_stock_params(), mp_params = get_mp_params(), app_params = app_params, initial_biomass = get_stock_params()$b0, nyears = input$nyears, niters = initial_niters))
  
  # Set iter as a reactive value. If this changes, it triggers other stuff
  iter <- reactiveVal(0) 
  
  # Reset 
  observe({
    req(input$nyears)
    # If any of the following change the observer gets triggered
    input$reset
    stock_params <- get_stock_params() # The output is stored here because we need it for the reset_stock() function
    mp_params <- get_mp_params()
    # Reset iterations and inidicators
    iter(0)
    pitemp(NULL)
    # Isolate here because calling reset_stock() causes stock to change which triggers this observe() resulting in infinite loop
    isolate(reset_stock(stock = stock, stock_params = stock_params, mp_params = mp_params, app_params = app_params, initial_biomass = stock_params$b0, nyears = input$nyears, niters = initial_niters))
  })
  
  # Project works by binding new rows (iterations) to the existing ones
  # Project all timesteps
  # Preallocate instead of adding rows - inefficient
  observeEvent(input$project, {
    iter(iter() + 1)
    # If nrow stock < iter, add new rows for biomass, effort, catch etc.
    # This new will contain the values for the new iteration
    if (nrow(stock$biomass) < iter()){
      # rbind is dropping names of dimnames - really annoying - so hack
      dnames <-names(dimnames(stock$biomass)) 
      stock$biomass <- rbind(stock$biomass,stock$biomass[1,])
      #stock$biomass_obs <- rbind(stock$biomass_obs,stock$biomass_obs[1,])
      stock$hcr_ip<- rbind(stock$hcr_ip,stock$hcr_ip[1,])
      stock$hcr_op<- rbind(stock$hcr_op,stock$hcr_op[1,])
      stock$catch <- rbind(stock$catch,stock$catch[1,])
      stock$effort <- rbind(stock$effort,stock$effort[1,])
      stock$estimated_cpue <- rbind(stock$estimated_cpue, stock$estimated_cpue[1,])
      # Bit hacky but rbind is dropping the dimnames names
      names(dimnames(stock$biomass)) <- dnames 
      names(dimnames(stock$effort)) <- dnames 
      names(dimnames(stock$hcr_ip)) <- dnames 
      names(dimnames(stock$hcr_op)) <- dnames 
      names(dimnames(stock$catch)) <- dnames 
      names(dimnames(stock$estimated_cpue)) <- dnames 
    }

    # Pass in current iter
    # project() takes entire stock
    # Here we only want one iteration - subset out using fancy lapply with [ operator
    stock_iter <- lapply(reactiveValuesToList(stock), '[', iter(),,drop=FALSE) 
    out <-  project(stock_iter,
                    timesteps=c((app_params$last_historical_timestep+1),dim(stock$biomass)[2]),
                    stock_params=get_stock_params(),
                    mp_params=get_mp_params(),
                    app_params=app_params)
    stock$biomass[iter(),] <- out$biomass
    stock$effort[iter(),] <- out$effort
    stock$hcr_ip[iter(),] <- out$hcr_ip
    stock$hcr_op[iter(),] <- out$hcr_op
    stock$catch[iter(),] <- out$catch

    # Get summary PIs 
    piout <- get_summaries(stock=stock, stock_params=get_stock_params(), app_params=app_params, quantiles=c(pi_percentiles[1], 50, pi_percentiles[2])/100)
    pitemp(piout)
  })
  
  # Call the HCR plot function
  output$plothcr <- renderPlot({
    plot_hcr(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, cex.axis=1.1, cex.lab=1.3)
  })
  
  output$plotbiomasshisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="biomass", percentile_range = pi_percentiles, cex.axis=1.1, cex.lab=1.3)
  })

  output$plotcatchhisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="catch", percentile_range = pi_percentiles, cex.axis=1.1, cex.lab=1.3)
  })

  output$plotrelcpuehisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="relcpue", app_params=app_params, percentile_range = pi_percentiles, cex.axis=1.1, cex.lab=1.3)
  })

  output$itercount <- renderText({
    paste("Number of replicates: ", iter(), sep="")
  })


  output$hcrpis <- renderTable({
    # Don't print table unless project has been pressed
    if (is.null(pitemp())){
      return()
    }
    # Use pitemp() to fill table
    piname_choice <- c("SB/SBF=0", "Catch", "Relative CPUE", "Prob. SB>LRP", "Catch stability", "Proximity to TRP")
    years <- dimnames(stock$biomass)$year
    out <- current_pi_table(pitemp()$periodqs, app_params=app_params, years=years, percentile_range = pi_percentiles, piname_choice=piname_choice)
    return(out)
    },
    bordered = TRUE,
    sanitize.text.function=identity,
    rownames = TRUE,
    caption= "Performance indicators",
    auto=TRUE
  )

  output$reptable<- renderTable({
    if (is.null(pitemp())){
      return()
    }
    replicate_table(stock=stock, stock_params=get_stock_params(), app_params=app_params, percentile_range=pi_percentiles)
  },
    bordered = TRUE,
    rownames = FALSE,
    caption= "Performance of each replicate",
    auto=TRUE)

  # Termination script - needed when running from bat file
  session$onSessionEnded(function() {
      stopApp()
  })
  
}

# Run the app
shinyApp(ui, server)
