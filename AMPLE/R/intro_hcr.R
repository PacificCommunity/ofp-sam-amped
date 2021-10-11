# Put together the Introduction to HCR app
#library(shiny)

#source("stock_class.R")
#source("intro_hcr_ui_funcs.R")
#source("mp_module.R")
#source("stochasticity_module.R")
#source("stock_module.R")
#source("hcr_funcs.R")

# Each Panel has it's sidebar and mainPanel
# Annoying - extra boilerplate but looks better

introHCR2 <- function(...){

  # User interface ----
  # Use navbarPage (fluidPage has a problem with title argument and tabs)
  ui <- navbarPage(
    title="navbarPage title",
    #title="          ",
    #tabsetPanel(
      tabPanel(title = "tabPanel 1 title",
        # Initiate sidebarLayout - fixed sidebar for all tabs (but we can turn the interaction options on and off)
        sidebarLayout(
          intro_hcr_sidebar_setup(
            # MP selector
            # HCR options
            mpParamsSetterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch", "Threshold effort", "Constant effort")),
            br(), # Could add br() automatically to side bar set up

            # Buttons
            tags$span(title="Project forward one year",
              actionButton("advance", "Advance")),
            tags$span(title="Reset current projection",
              actionButton("reset", "Reset")),
            br(),
            stockParamsSetterUI("stock"),
            br(),
            # Stochasticity module
            stochParamsSetterUI("stoch", init_biol_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE),
            br()
          ),


          mainPanel(
            textOutput("testtext"),
            #textOutput(renderText({"arse"}))
            tableOutput("printstock")



          ) # End of mainPanel
        ) # End of sidebarLayout
      ), # End of tabPanel 1

      tabPanel(title = "tabPanel 2 title",
        # Initiate sidebarLayout - this is annoying having to do this for each tab - but could use functions to reduce amount of boilerplate code
        sidebarLayout(
          intro_hcr_sidebar_setup(
            br()
          ),
          mainPanel(
          ) # End of mainPanel
        ) # End of sidebarLayout
      ) # End of tabPanel 2



    #) # End of tabsetPanel1
  ) # End of navbarPage

  # Start of server function
  server <- function(input, output,session) {

    # Notes for myself:
    # get_mp_params is a "reactiveExpr"
    # Use: get_mp_params() which evaluates it (evaluates the switcheroo function and returns the parameters)
    # This can only be done inside a reactive consumer
    get_stoch_params <- stochParamsSetterServer("stoch")
    get_mp_params <- mpParamsSetterServer("mpparams", get_stoch_params)
    #get_mp_params <- mpParamsSetterServer("mpparams") # Test without get_stoch_params - doesn't update mp_params as stochasticity parameters change
    get_stock_params <- stockParamsSetterServer("stock", get_stoch_params)

    # Make a stock object using the get_stock_params()
    # If the stock params inputs change, get_stock_params() gets triggered, triggering this observe event and
    # making the stock object.
    # This event gets triggered when the app opens to make the new stock.
    #reactive(stock <- Stock$new(stock_params=get_stock_params())) # Calls initialize method
    #observe({
    #  x <- 5
    #  # We need biol sigma from the stochasticity parameters as part of the stock
    #  stock_params <- get_stock_params()
    #  mp_params <- get_mp_params()
    #  stock <- Stock$new(stock_params=stock_params, mp_params=mp_params)
    #  x <- 5
    #})

    # Make the stock - has to be in a reactive environment to be able to get the stock and mp params
    # returns a reactive stock
    stock <- reactive({
      x <- 5
      stock_params <- get_stock_params()
      mp_params <- get_mp_params()
      stock <- Stock$new(stock_params=stock_params, mp_params=mp_params)
    #  x <- 5
      return(stock)
      #return(5)
    })

    output$printstock <- renderTable({
      stock()$as_data_frame()
    })



    #browser()
    #stock$biomass
    #x <- 5



    # Test how it works
    # First time the app opens up and everytime the MP inputs are changed, get_mp_params() changes and this observe() gets triggered. mpp is just a normal list
    # Anytime the parameters change, a reactive environment, like observe() will also trigger.
    #observe({
    #  browser()
    #  #mpp <- get_mp_params()
    #  #sp <- get_stoch_params()
    #  x <- 5
    #  sp <- get_stock_params()
    #  x <- 5
    #})

    # Modules for the stochasticity and MP parameters!
    #get_mp_params <- callModule(mp_params_setter, "mpparams")
    #get_stoch_params <- callModule(stoch_params_setter, "stoch")
    #get_lh_params <- callModule(stock_params_setter, "stock")
    ## Join these together into a single object to be passed to the funcs - bit clumsy as I will have to do this in all the servers
    #get_stock_params <- reactive({
    #  sp <- get_stoch_params()
    #  lh <- get_lh_params()
    #  out <- c(sp, lh)
    #  return(out)
    #})

    # App parameters, year range etc
    app_params <- list(initial_year = 2009, last_historical_timestep = 10)
    # Make the reactive stock object and fill it up with initial values
    # stock <- create_stock()
  } # End of server function

  # Run the app
  shinyApp(ui, server)
}

