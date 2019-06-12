# Copyright 2018 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC

# Load packages ----
library(AMPLE)

# User interface ----
ui <- navbarPage(
  title="What is a Harvest Control Rule?",
  tabPanel("What is a Harvest Control Rule?",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        mp_params_setterUI("mpparams", mp_visible=c("Threshold catch","Constant catch"), init_thresh_max_catch=140, init_thresh_belbow=0.5),
        br(),
        # Buttons
        tags$span(title="Project forward one year", 
          actionButton("advance", "Advance")
        ),
        tags$span(title="Reset current projection", 
          actionButton("reset", "Reset")
        ),
        br(),
        # Stochasticity module
        stoch_params_setterUI("stoch", init_prod_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE)
      ),
      mainPanel(
        # Main plot for the Intro to HCR app
        # 2 x 2 panel
        # Catch  |  HCR
        # ----------------
        # B/K    |  connecting arrow
        fluidRow(
          column(6, tags$span(title="Plot of the total catch. The blue, dashed horizontal line is next years catch limit that has been set by the HCR. The grey, dashed horizontal lines are the catch limits that were set by the HCR in the past.",
            plotOutput("plotcatch", width="auto"))
          ),
          column(6, tags$span(title="The HCR. The blue, dashed vertical line shows the current estimated biomass that is used as the input. The blue, dashed horizontal line shows the resulting catch limit that will be set for the following year",
            plotOutput("plothcr", width="auto"))
          )
        ),
        fluidRow(
          column(6, tags$span(title="The biomass of the stock (scaled by the unfished biomass). When the variability options are switched on, the black line is the 'true' biomass and the blue line is the 'estimated' biomass. The HCR uses the estimated biomass for the input.",
            plotOutput("plotbiomass", width="auto"))
          ),
          column(6, tags$span(title="The current estimated biomass is used as the input to the HCR.",
            plotOutput("plotarrow", width="auto"))
          )
        )
      )
    )
  ),
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
          # Stock LH setter
          stock_params_setterUI("stock"),
          # Total number of years (including historical years)
          tags$span(title="The total number of years in the projection",
            numericInput("nyears", "Number of years", value = 30, min=20, max=100, step=1)
          )
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
        amped_maintainer_and_licence()
        #tags$footer("test")
      ),
      mainPanel(width=9,
        h1("Instructions"),
        p("This application attempts to introduce the fundamental idea behind a Harvest Control Rule (HCR)."),
        p("By default a ", em("Threshold catch"), " style of HCR is in operation. The parameters of the HCR are set using the sliders on the left-hand slide ", em("(Blim, Belbow, Cmin and Cmax)"), ". Changing the parameters will reset the current projection"),
        p("The ", em("Threshold catch"), " HCR takes the ", em("estimated"), "value of biomass as the input.  This can be seen by following the blue arrow from the biomass plot at the bottom left to the HCR plot at the top right.  The HCR uses the biomass input to set the catch limit in the next timestep. The vertical, blue dashed line on the HCR plot shows the current estimated value of the stock biomass. The horizontal, blue dashed line on the HCR plot is the catch limit set for the next timestep. The horizontal, blue dashed line is also shown on the catch plot at the top. It shows where the catch will be in the next timestep. The process moves anti-clockwise."),
        p("Pressing the ", strong("Advance"), " button steps the projection forward by one timestep. The projection is performed by fishing the stock at the level set by the HCR and calculating the response of the stock.  The response of the stock is a combination of the stock dynamics and the impacts of fishing. Pressing the ", strong("Reset"), "button resets the projection"),
          p("When the ", strong("Advance"), " button is pressed you should see that the next catch goes to where the blue dashed line was."),
        p("The ghosts of catch limits from the past are shown as grey dashed lines on the catch plot and as grey dots on the HCR plot. These allow you to see which parts of the HCR have been active."), 
        h2("Variability"),
        p("Variability can be included in the projection in two ways: through variability in the stock productivity and through the estimated level of stock biomass being different to the true level of the stock biomass. These options are initially turned off. The options can be seen by clicking on the ", strong("Show variability options"), "box."),
        p("Biological productivity variability represents the variability of the natural procesess of the stock, for example growth and natural mortality. Increasing the variability will increase the 'bumpiness' of the stock trajectory. As biological variability is always encountered in fisheries it is essential that a selected HCR is robust to the variability."),
        p("Estimation error simulates the difference between the true level of the stock biomass and the estimated level. Unfortunately, the true abundance of a fish stock is never known. Instead, estimates of abundance are made, for example using stock assessment models. The HCR uses the estimated biomass, not the true biomass. This means that the catch limit that is set by the HCR is based on estimated biomass. If the biomass is estimated poorly the resulting catch limit set by the HCR may not be appropriate."),
        p("Here, estimation error is modelled using two different processes: random error and consistent bias (positive or negative). The bias represents situations where the biomass is consistently over or under estimated."),
        p("When estimation error is active the biomass plot shows two lines. The black line shows the true biomass, the blue line shwows the estimated biomass. It is the blue line that feeds the HCR. Increasing the estiamation bias and variability will increase the difference between these lines.")
      )
    )
  )
)

server <- function(input, output,session) {

  # App parameters, year range etc
  app_params <- list(initial_year = 2009, last_historical_timestep = 10)
  app_params$historical_timesteps = 1:app_params$last_historical_timestep

  # Modules for the stochasticity and MP parameters!
  get_mp_params <- callModule(mp_params_setter, "mpparams") 
  get_stoch_params <- callModule(stoch_params_setter, "stoch") 
  get_lh_params <- callModule(stock_params_setter, "stock") 
  # Join these together into a single object to be passed to the funcs - bit clumsy as I will have to do this in all the servers
  get_stock_params <- reactive({
    sp <- get_stoch_params()
    lh <- get_lh_params()
    out <- c(sp, lh)
    return(out)
  })

  # Make the reactive stock object and fill it up with initial values
  stock <- create_stock()

  # Make the initial stock 
  # Use isolate as we don't want this triggered by changes to stock or stock_params
  niters <- 1 # We only have 1 iteration for this stock
  isolate(reset_stock(stock = stock, stock_params = get_stock_params(), mp_params = get_mp_params(), app_params = app_params, initial_biomass = get_stock_params()$b0, nyears = input$nyears, niters = niters))
  
  # To keep track of current timestep
  timestep <- reactiveVal(app_params$last_historical_timestep) 
  
  # Reset the stock if any of the controls are fiddled with
  # Just side effects so use an observer
  observe({
    # req checks that nyears is available (if leave input box empty it won't be) - if not, don't do this observe
    req(input$nyears)
    # If any of the following change the observer gets triggered
    nyears <- input$nyears
    input$reset
    timestep(app_params$last_historical_timestep) # sets the timestep to last historical timestep
    mp_params <- get_mp_params()
    stock_params <- get_stock_params()
    # Need isolate here because calling reset_stock() causes stock to change which triggers this observe() resulting
    # in an infinite loop
    isolate(reset_stock(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, initial_biomass=get_stock_params()$b0, nyears=nyears, niters=niters))
  })

  # Advance timestep by 1 if the action button is pressed
  observeEvent(input$advance, {
    if(timestep() < dim(stock$catch)[2]){
      timestep(timestep() + 1)
      out <-  project(stock,
                      timesteps=c(timestep(),timestep()),
                      stock_params=get_stock_params(),
                      mp_params=get_mp_params(),
                      app_params=app_params)
      stock$biomass <- out$biomass
      stock$effort <- out$effort
      stock$hcr_ip <- out$hcr_ip
      stock$hcr_op <- out$hcr_op
      stock$catch <- out$catch
    }
  })
  
  # Plot the 4 panels separately
  output$intro_hcr_plot <- renderPlot({
    plot_hcr_intro(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, timestep = timestep())
  })

  output$plotcatch <- renderPlot({
    plot_catch(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, timestep=timestep(), main="Catch", add_grid=TRUE)
  })

  output$plothcr <- renderPlot({
    plot_hcr(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, timestep=timestep()+1)
  })

  output$plotbiomass <- renderPlot({
    plot_biomass(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), timestep=timestep()+1, main="SB / SBF=0", add_grid=TRUE)
  })

  output$plotarrow <- renderPlot({
    plot_hcr_intro_arrow(stock=stock, timestep=timestep()+1-get_mp_params()$timelag)
  })

  # Termination script - needed when running from bat file
  session$onSessionEnded(function() {
      stopApp()
  })
}

# Run the app
shinyApp(ui, server)
