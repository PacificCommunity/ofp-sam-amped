# Module for setting up the stock, including life history parameters

#' stockParamsSetterUI
#'
#' stockParamsSetterUI() is the interface for the stock options (e.g. life history and exploitation status).
#'
#' @return A taglist
#' @rdname stock_module
#' @name Stock module
stockParamsSetterUI <- function(id){
  ns <- NS(id)
  header <- tags$p("Here you can adjust the settings for the app, including the life history of the stock and length of the projections. Note that changing these settings will reset the app.")
  # Life history parameters - used to determine the r and K in the production model
  stock_lh <- tags$span(title="Choose the life history of the stock: slow, medium or fast growing. Some HCRs are more appropriate for different life histories.",
    radioButtons(ns("stock_lh"), label= "Stock life history", choices = list("Slow growth" = "slow", "Medium growth" = "medium", "Fast growth" = "fast"), selected = "medium"))

  # Has the stock been over, under or fully exploited
  stock_history <- tags$span(title="Choose the history of the stock. Underexploited means that the stock could potentially be fished harder. Overexploited means that a recovery plan may be needed.",
    radioButtons(ns("stock_history"), label= "Stock history", choices = list("Underexploited" = "under", "Fully exploited" = "fully", "Overexploited" = "over"), selected = "fully")
  )

  # Length of projection, number of historical years and initial year
  initial_year <- tags$span(title="First year of the stock history. It has no effect, it just changes the time axis labels.",
    numericInput(ns("initial_year"), label="First year", value=2010, min=2000, max=2020, step=1))

  nyears <- tags$span(title="Total number of years in the projection, including historical period.",
    numericInput(ns("nyears"), label="Number of years", value=30, min=20, max=50, step=1))

  last_historical_timestep <- tags$span(title="The number of years that make up the historical period.",
    numericInput(ns("last_historical_timestep"), label="Length of historical period", value=10, min=2, max=19, step=1))

  return(tagList(header, stock_lh, stock_history, initial_year, nyears, last_historical_timestep))
}

#' stockParamsSetterServer
#'
#' stockParamsSetterServer() does the setting of the stock parameters in the server.
#'
#' @param id Shiny magic
#' @param get_stoch_params Reactive expression that accesses the stochasticity module server.
#' @return A list of stock options.
#' @rdname stock_module
#' @name Stock module
#' @export
stockParamsSetterServer <- function(id, get_stoch_params=NULL){
  moduleServer(id, function(input, output, session){
    reactive({
      stoch_params <- list(biol_sigma = 0)
      if(!is.null(get_stoch_params)){
        stoch_params <-  get_stoch_params()
      }
      return(get_stock_params(input, biol_sigma=stoch_params$biol_sigma))
      })
  })
}


# Defined outside of above reactive so we can call it elsewhere
#' get_stock_params
#'
#' get_stock_params() Sets up default values for the stock, including year range.
#' It's a separate function so it can be used and tested outside of a reactive environment.
#' @param input List of stock parameters taken from the shiny UI (stockParamsSetterUI()).
#' @param biol_sigma Standard deviation of the biological variability (default = 0).
#'
#' @rdname stock_module
#' @name Stock module
#' @export
get_stock_params <- function(input, biol_sigma=0){
  # Set r and k depending on the stock choice radio button
  # Set MSY to be a 100 for each stock
  msy <- 100
  # MSY = rk/4
  r <- switch(input$stock_lh,
              "slow" =  0.2,
              "medium" = 0.6,
              "fast" = 1.0)
  # Could add check year to ensure that nyears and last_historical_timestep don't clash
  # Input selectors should ensure it is OK, but additional check here to make sure.
  
  # Trying to prevent NAs in the numericInputs
  req(input$initial_year, input$last_historical_timestep, input$nyears) 
  
  out <- list(
    r = r,
    stock_history = input$stock_history, # to set up the initial trajectory
    nyears = input$nyears,
    initial_year = input$initial_year,
    last_historical_timestep = input$last_historical_timestep,
    biol_sigma = biol_sigma
  )
  return(out)
}

