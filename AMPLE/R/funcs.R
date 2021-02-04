# Common functions for AMPED and PIMPLE
# 
# Copyright 2019 Finlay Scott. Distributed under the GPL 3
# Maintainer: Finlay Scott, SPC
#

# A metric fuckton of imports
#' @importFrom grDevices "colorRampPalette"
#' @importFrom graphics  "arrows" "barplot" "grid" "hist" "layout" "legend" "par" "points" "rect"
#' @importFrom stats "dnorm" "quantile" "rlnorm" "rnorm" "sd"
#' @importFrom utils "data"
#' @import shiny

# A metric fuckton of global variables because the non standard evaluation is absolute cack
globalVariables(c("X20.", "X5.", "X50.", "X80.", "X95.", "bin", "ffmsymin", "ffmsymed", "ffmsymax",
                  "hcr", "hcrref", "iter", "level", "metric", "msectrl", "period", "piname", "piname_wrap", "pix", "prop",
                  "biomassmin", "biomassmed", "biomassmax", "upsidedown", "value", "wormid", "x", "xmax", "xmin", "y", "year", "ymin", "."))


# This looks pretty crummy

#' Maintainer, licence and SPC about information.
#'
#' Show the maintainer and licence for use in the AMPED PIMPLE applications.
#' Also show the 'About SPC' information.
#' 
#' @return A shiny.tag for use in Shiny apps
#' @rdname maintainer_and_licence
#' @examples
#' amped_maintainer_and_licence()
#' pimple_maintainer_and_licence()
#' spc_about()
#' @export
amped_maintainer_and_licence <- function(){
  out <- tags$html(
    tags$h1("AMPED"),
    tags$p("Amazing Management Procedures Exploration Device"),
    tags$footer(
      tags$p("version 0.3.0 My Jampandy"),
      tags$p("Copyright 2019 OFP SPC MSE Team."),
      tags$p("Distributed under the GPL 3"),
      tags$a("Soure code", href="https://github.com/PacificCommunity/ofp-sam-mse-popmodel-Shiny/tree/master/AMPED")
    )
  )
  return(out)
}

#' @rdname maintainer_and_licence
#' @export
pimple_maintainer_and_licence <- function(){
  out <- tags$html(
    tags$h1("PIMPLE"),
    tags$p("Performance Indicators and Management Procedures Explorer"),
    tags$footer(
      tags$p("version 0.2.0 Rupert Trousers"),
      tags$p("Copyright 2019 OFP SPC MSE Team."),
      tags$p("Distributed under the GPL 3")
    )
  )
  return(out)
}

#' @rdname maintainer_and_licence
#' @export
spc_about <- function(){
  out <- tags$html(
    tags$p(style="opacity: 0.5", class="caption", align="center", HTML("&copy"), "Pacific Community, 2019"),
    tags$h1("About us:"),
    tags$p(align="justify", "The Pacific Community (SPC) is the principal scientific and technical organisation in the Pacific region, proudly supporting development since 1947. It is an international development organisation owned and governed by its 26 country and territory members. The members are: American Samoa, Australia, Cook Islands, Federated States of Micronesia, Fiji, France, French Polynesia, Guam, Kiribati, Marshall Islands, Nauru, New Caledonia, New Zealand, Niue, Northern Mariana Islands, Palau, Papua New Guinea, Pitcairn Islands, Samoa, Solomon Islands, Tokelau, Tonga, Tuvalu, United States of America, Vanuatu, and Wallis and Futuna."),
    tags$p(align="justify", "In pursuit of sustainable development to benefit Pacific people, this unique organisation works across more than 25 sectors. SPC is renowned for its knowledge and innovation in such areas as fisheries science, public health surveillance, geoscience and conservation of plant genetic resources for food and agriculture."),
    tags$p(align="justify", "Much of SPC's focus is on major cross-cutting issues, such as climate change, disaster risk management, food security, gender equality, human rights, non-communicable diseases and youth employment. Using a multi-sector approach in responding to its members' development priorities, SPC draws on skills and capabilities from around the region and internationally, and supports the empowerment of Pacific communities and sharing of expertise and skills between countries and territories."),
    tags$p(align="justify", "With over 600 staff, SPC has its headquarters in Noumea, regional offices in Suva and Pohnpei, a country office in Honiara and field staff in other Pacific locations. Its working languages are English and French. See: ", a("https://www.spc.int", href="www.spc.int"))
  )
}


# Make the empty reactive stock object
# The stock object is not reactive, but the elements inside it are (it's like a list)
# stock cannot just be a calculated value returned from a reactive() as it needs to persist
# i.e. here the next timestep depends on the previous timestep
# it's empty but has structure

# Stock is sort of passed by reference as when this function is called the return value is not stored but stock is updated
# But stock does need to be passed in - i.e. it is not found in the calling environment
# Make an empty stock with the right dimensions - niters is the number of rows
clear_stock <- function(stock, app_params, nyears, niters){
  # nyears must be greater than app_params$last_historical_timestep and an integer
  nyears <- max(app_params$last_historical_timestep+1, round(nyears))
  initial_array <- array(NA, dim=c(niters, nyears), dimnames=list(iter=1:niters, year=app_params$initial_year:(app_params$initial_year+nyears-1)))
  stock$biomass <- initial_array 
  stock$hcr_ip <- initial_array
  stock$hcr_op <- initial_array
  stock$catch <- initial_array
  stock$effort <- initial_array
  stock$estimated_cpue <- initial_array
  # Not sure this return statement is needed but means we can call this function from tests
  return(stock)
}


# Return an array of catches to drive the initial dynamics of the stock
get_catch_history <- function(stock_params, app_params, niters){
  # It will depend on the stock_history parameter, r and k
  # MSY = r K / 4 # BMSY = K / 2 # FMSY = r/2
  # Make one trajectory, then repeat with a load of noise on it
  msy <- stock_params$r * stock_params$k / 4
  catch <- switch(stock_params$stock_history,
         "under" = rep(2*msy/3, app_params$last_historical_timestep),
         "fully" = rep(msy, app_params$last_historical_timestep),
         "over" = seq(from=3*msy/4, to=4*msy/3, length=app_params$last_historical_timestep)
         )
  out <- array(NA,dim=c(niters, app_params$last_historical_timestep))
  out[] <- rep(catch, each=niters)
  # Sling a load of noise on it
  # Set seed so that the initial noise is always the same
  set.seed(666)
  out <- out * rlnorm(prod(dim(out)),meanlog=0,sdlog=0.1)
  # Set a proper random seed
  set.seed(as.numeric(Sys.time()))
  # Or don't include noise in initial period
  return(out)
}

fill_initial_stock <- function(stock, stock_params, mp_params, initial_biomass, app_params){
  stock$biomass[,1] <- initial_biomass 
  # First X years have history
  # Need some way of setting the stock history - a choice
  catch_history <- get_catch_history(stock_params, app_params, niters=dim(stock$biomass)[1])
  stock$catch[,1:app_params$last_historical_timestep] <- catch_history

  hcr_ip_yrs <- 1:(app_params$last_historical_timestep - mp_params$timelag + 1) # + 1 to match the biomass
  hcr_op_yrs <- (1+mp_params$timelag):(app_params$last_historical_timestep + 1) # + 1 to match the biomass
  # Need to set an initial HCR IP?
  for (yr in 1:app_params$last_historical_timestep){
    # Should be able to handle multiple iters
    next_biomass <- isolate(get_next_biomass(stock$biomass[,yr], stock$catch[,yr], stock_params))
    stock$biomass[,yr+1] <- next_biomass
  }
  # Fill up the easy ones
  stock$effort <- stock$catch / (stock$biomass * stock_params[["q"]])
  true_cpue <- stock$catch / stock$effort
  stock$estimated_cpue <- estimation_error(input =  true_cpue, sigma = stock_params$biol_est_sigma, bias = stock_params$biol_est_bias)
  # The IP and OP functions must work for iters
  # yr argument is the timestep range of the HCR IP - i.e. lagging behind catch
  # Should be able to handle multiple iters
  stock$hcr_ip[,hcr_ip_yrs] <- get_hcr_ip(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=hcr_ip_yrs)
  stock$hcr_op[,hcr_op_yrs] <- get_hcr_op(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=hcr_ip_yrs)
  # Not sure this return statement is needed as stock seems to be used by reference here
  # but means we can call this function from tests
  return(stock)
}

#' Functions for creating and filling stock objects.
#'
#' A stock is made up of biomass, hcr_ip, hcr_op, effort and catch.
#' create_stock() makes the empty reactive stock object for AMPED apps.
#' It has the elements but they are empty.
#'
#' @return A reactiveValues object with bits for the stock.
#' @rdname stock_creators
#' @export
create_stock <- function(){
  stock <- reactiveValues(
    biomass = NULL,
    hcr_ip = NULL,
    hcr_op = NULL,
    effort = NULL,
    catch = NULL,
    estimated_cpue = NULL
  )
  return(stock)
}

#' Functions for creating and filling stock objects.
#'
#' reset_stock() clears out an existing stock and refills the initial period depending on the stock status option (over, fully, underexploited).
#'
#' @param stock A list with elements biomass, hcr_ip, hcr_op, effort and catch.
#' @param stock_params A vector of life history and stochasticy parameters.
#' @param mp_params A vector of management procedure parameters.
#' @param app_params A vector of application parameters.
#' @param initial_biomass The initial biomass of the population.
#' @param nyears The numbers of years to set the stock up for.
#' @param niters The numbers of iters to set the stock up for.
#' 
#' @return A filled stock object (a reactiveValues object with bits for the stock).
#' @rdname stock_creators
#' @examples
#' # Making a NULL stock
#' create_stock()
#'
#' # Resetting a stock
#' # Managment procedure bits - should come from Shiny app inputs
#' input_mp <- list(
#'   blim_belbow = c(0.2, 0.5),
#'   cmin_cmax = c(10, 140), 
#'   constant_catch_level = 50,
#'   constant_effort_level = 1,
#'   emin_emax = c(0.1, 0.5),
#'   hcr_type = "threshold_catch")
#' mp_params <- mp_params_switcheroo(input_mp)
#'
#' # Stochasticity bits - should come from Shiny app inputs
#' input_stoch <- list(
#'   biol_est_bias = 0,
#'   biol_est_sigma = 0,
#'   biol_prod_sigma = 0, 
#'   show_var <- TRUE)
#' stoch_params <- set_stoch_params(input_stoch)
#'
#' # Life history bits - should come from Shiny app inputs
#' input_lh <- list(
#'   stock_history = "fully",
#'   stock_lh = "medium")
#' lh_params <- get_lh_params(input_lh)
#'
#' # Stitch together and make other parameters - should be inside an Shiny app 
#' stock_params <- c(stoch_params, lh_params)
#' app_params <- list(initial_year = 2009, last_historical_timestep = 10)
#'
#' # Make the null stock and fill it up
#' # In a Shiny app use the create_stock() function but cannot do here so make an equivalent object
#' stock <- list(biomass = NULL, hcr_ip = NULL, hcr_op = NULL, effort = NULL, catch = NULL)
#'
#' # Reset the stock
#' stock <- reset_stock(stock = stock, stock_params = stock_params, mp_params = mp_params,
#'   app_params = app_params, initial_biomass = stock_params$b0, nyears = 20, niters = 10)
#' @export
reset_stock <- function(stock, stock_params, mp_params, app_params, initial_biomass, nyears, niters){
  # Set up current_corrnoise object to store the current noise value for each iteration
  # Stored in the package global environment
  pkg_env$current_corrnoise <- rep(0, niters)
  #pkg_env$current_corrnoise <- rnorm(niters, sd=0.01)
  #
  stock <- clear_stock(stock=stock, app_params=app_params, nyears, niters)
  stock <- fill_initial_stock(stock=stock, stock_params=stock_params, mp_params=mp_params, initial_biomass=initial_biomass, app_params=app_params)
  # Not sure this return statement is needed but means we can call this function from tests
  return(stock)
}

# Observation error is a combination of bias and lognormally distributed noise
estimation_error <- function(input, sigma, bias){
  # Transform sigma - it means we can set the parameter as a similar scale to the biol var sigma
  sigma <- sigma / 5
  est_variability <- rlnorm(length(input),meanlog=0,sdlog=sigma) 
  output <- input * est_variability * (1 + bias)
  return(output)
  
}

# The main projection function
# Projects the stock over the timesteps given and updates the biomass, observed biomass and catches
# params - named vector with r, k, p, q (catchability)
# timesteps - vector of length 2, the timesteps to project over (catch or biomass as we always get biomass in t+1?)
# biomass - array: niter * timesteps
# catch- array: niter * timesteps
# Project the population and catch over the timesteps
# We can always get the next biomass given last years catch (if there is space)
# Could just pass in whole stock object and an iteration number instead the bits
# biomass, biomass_obs and catch can have multiple iterations
# biomass, biomass_obs and catch are arrays [niters, nyears]
# timelag is lag between observed ip timing and HCR op timing, or between i.e. op in time t = f(ip in time t - timelag)
# A lag of 0 means that the catch in year Y is given by the biomass at the start of year Y (same element in the array)
# Biomass at the start of the timestep
# Biomass is one year ahead of catch
# Relative effort is capped

#' Projects the stock over the timesteps given and updates the biomass, HCR ip / op and catches
#'
#' project() uses a simple biomass dynamic model where the catches or fishing effort are set every timestep by the harvest control rule.
#'
#' @param stock A list with elements biomass, hcr_ip, hcr_op, effort and catch.
#' @param timesteps The timesteps to project over. A vector of length 2 (start and end).
#' @param stock_params A vector of life history and stochasticy parameters.
#' @param mp_params A vector of management procedure parameters.
#' @param app_params A vector of application parameters.
#' @param max_releffort The maximum relative effort.
#' @return A stock object (a reactiveValues object with bits for the stock)
#' @rdname project_functions
#' @examples
#' # Set up all the bits for a projection - should be done inside a Shiny app
#' # Managment procedure bits - should come from Shiny app inputs
#' input_mp <- list(
#'   blim_belbow = c(0.2, 0.5),
#'   cmin_cmax = c(10, 140), 
#'   constant_catch_level = 50,
#'   constant_effort_level = 1,
#'   emin_emax = c(0.1, 0.5),
#'   hcr_type = "threshold_catch")
#' mp_params <- mp_params_switcheroo(input_mp)
#' 
#' # Stochasticity bits - should come from Shiny app inputs
#' input_stoch <- list(
#'   biol_est_bias = 0,
#'   biol_est_sigma = 0,
#'   biol_prod_sigma = 0, 
#'   show_var <- TRUE)
#' stoch_params <- set_stoch_params(input_stoch)
#' 
#' # Life history bits - should come from Shiny app inputs
#' input_lh <- list(
#'   stock_history = "fully",
#'   stock_lh = "medium")
#' lh_params <- get_lh_params(input_lh)
#' 
#' # Stitch together and make other parameters - should be inside an Shiny app 
#' stock_params <- c(stoch_params, lh_params)
#' app_params <- list(initial_year = 2009, last_historical_timestep = 10)
#' 
#' # Make the null stock and fill it up
#' # In a Shiny app use the create_stock() function but cannot do here so make an equivalent object
#' stock <- list(biomass = NULL, hcr_ip = NULL, hcr_op = NULL, effort = NULL, catch = NULL)
#' stock <- reset_stock(stock = stock, stock_params = stock_params, mp_params = mp_params,
#'   app_params = app_params, initial_biomass = stock_params$b0, nyears = 20, niters = 10)
#'
#' # Finally project over the timesteps
#' out <- project(stock, timesteps = c(11,20), stock_params = stock_params, mp_params = mp_params,
#'   app_params = app_params)
#'
#' # Or just get the HCR op in a single timestep
#' hcr_op <- get_hcr_op(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=10)
#' @export
project <- function(stock, timesteps, stock_params, mp_params, app_params, max_releffort = 10){
  # Check timesteps - should be a range of two values
  if (length(timesteps) == 1){
    timesteps <- rep(timesteps,2)
  }
  if (length(timesteps) != 2){
    stop("In project(). timesteps argument should be of length 2.")
  }
  if((timesteps[1] - mp_params$timelag) < 1){
    stop("In project(). Trying to access element in yr less than 1")
  }
  # Loop over the timesteps and update catch and biomass
  # yr is the year we get catch for
  # yr + 1 is the  year we get true biomass and HCR op for
  for (yr in timesteps[1]:timesteps[2]){

    # Get HCR OP in yr instead of at end?
    # But should already have been set given biomass which is always y+1
    base_effort <- stock$effort[,app_params$last_historical_timestep]
    # Implementation function stuff
    # Test this to death with different yr ranges etc
    if (mp_params$output_type == "catch"){
      stock$catch[,yr] <- stock$hcr_op[,yr]
      stock$effort[,yr] <- stock$catch[,yr] / (stock_params$q * stock$biomass[,yr]) 
    }
    if (mp_params$output_type == "catch multiplier"){
      # Careful - yr may be 1, so yr-1 may be null
      new_catch <- stock$catch[,yr-1] * stock$hcr_op[,yr]
      new_catch[new_catch < 10] <- 10 # A minimum catch
      stock$catch[,yr]<- new_catch
      stock$effort[,yr] <- stock$catch[,yr] / (stock_params$q * stock$biomass[,yr]) 
    }
    if (mp_params$output_type == "relative effort"){
      # HCR OP is relative to last historical effort
      stock$effort[,yr] <- base_effort * stock$hcr_op[,yr]
      stock$catch[,yr] <- stock$effort[,yr] * stock_params$q * stock$biomass[,yr]
    }
    # Apply relative effort cap and recalc max. realisable catch
    rel_effort <- stock$effort[,yr] / base_effort
    max_rel_effort <- pmin(rel_effort, max_releffort)
    stock$effort[,yr] <- max_rel_effort * base_effort
    stock$catch[,yr] <- stock$effort[,yr] * stock_params$q * stock$biomass[,yr]
    # Update estimated cpue too
    true_cpue <- stock$catch[,yr] / stock$effort[,yr]
    stock$estimated_cpue[,yr] <- estimation_error(input =  true_cpue, sigma = stock_params$biol_est_sigma, bias = stock_params$biol_est_bias)

    # If room get the biomass in the next timestep (using previously set catch)
    # Bt+1 = Bt + f(Bt) - Ct
    # f(Bt) = r/p Bt * (1 - (Bt/k)^p)
    if (yr < dim(stock$biomass)[2]){
      next_biomass <- get_next_biomass(biomass=stock$biomass[,yr], catch=stock$catch[,yr], stock_params=stock_params)
      stock$biomass[,yr+1] <- next_biomass
      stock$hcr_ip[,yr+1-mp_params$timelag] <- get_hcr_ip(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=yr+1-mp_params$timelag)
      stock$hcr_op[,yr+1] <- get_hcr_op(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=yr+1-mp_params$timelag)
    }
  }
  return(stock)
}

#' Projects the stock over the timesteps given and updates the biomass, HCR ip / op and catches
#'
#' get_hcr_op() evaluates the harvest control rule in a single year (timestep) for multiple iterations.
#'
#' @param yr Evaluate the HCR in a particular year (timestep).
#' @return A vector of outputs from the HCR.
#' @rdname project_functions
#' @export
get_hcr_op <- function(stock, stock_params, mp_params, yr){
  # Shape is not NA
  # Call HCR with the lagged input
  #hcr_op <- do.call(mp_params$hcr_shape, args=list(stock=stock, mp_params=mp_params, stock_params=stock_params, yr=yr))
  hcr_op <- do.call(mp_params$hcr_shape, args=list(input=stock$hcr_ip[,yr,drop=FALSE], mp_params=mp_params, stock_params=stock_params, yr=yr))
  return(hcr_op)
}


# The surplus production model
# General form:
# Bt+1 = Bt + f(Bt) - Ct
# Pella & Tomlinson
# f(Bt) r/p Bt * (1 - (Bt/k)^p
# set p = 1 to get Schaefer
# cpue = Ct / Et = qBt
# biomass,  catch can have multiple iterations
# but this is one just timstep so we are dealing a with a vector of iterations

get_next_biomass<- function(biomass, catch, stock_params){
  fB <- (stock_params[["r"]] / stock_params[["p"]]) * biomass * (1 - (biomass / stock_params[["k"]])^stock_params[["p"]])
  # Apply lognormal noise to r
  #process_variability <- rlnorm(length(biomass),meanlog=0,sdlog=stock_params[["biol_prod_sigma"]]) 
  #fB <- fB * process_variability
  # A value of b = 0.5 is red noise, make redder by increasing (< 1)
  # Currently not an input but could be
  b <- 0.5
  pkg_env$current_corrnoise <- next_corrnoise(pkg_env$current_corrnoise, b=b, sd=stock_params[["biol_prod_sigma"]])
  # Subsetting current_corrnoise in we case we have set it up to have multiple iterations but only use 1 iter at a time
  fB <- fB * (pkg_env$current_corrnoise[1:length(fB)] + 1)
  next_biomass <- biomass + fB - catch
  # Biomass cannot be less than 1e-6
  next_biomass <- pmax(next_biomass,1e-6)
  return(next_biomass)
}

# Run the MP analyses function to generate the input to the HCR
# i.e. observed data
# e.g. observed biomass from an assessment
# analysis functions: "assessment" or NA
# yr is the timestep that use to generate the HCR IP in the current year
# e.g. look at biomass / k in year yr
get_hcr_ip <- function(stock, stock_params, mp_params, yr){
  # Check for NA in mp_analysis, if so return NA
  if (is.na(mp_params$mp_analysis)){
    hcr_ip <- NA
  }
  # If not NA call the analysis function
  else {
    # Call HCR with correct inputs:
    hcr_ip <- do.call(mp_params$mp_analysis, args=list(stock=stock, mp_params=mp_params, stock_params=stock_params, yr=yr))
  }
  return(hcr_ip)
}


# Evaluates the HCR using do.call() and returns a catch for the biomass dynamic model
# Inputs to the HCRs have already been calculated and are stored in stock$hcr_ip
# Outputs from the HCR can be different (e.g. catch, catch multiplier, effort, effort multiplier)
# Evaluates one timestep (yr), multiple iterations (chance of multiple timesteps? maybe - need to be careful with dims)
# yr is the yr of the stock$hcr$input to be used

#-------------------------------------------------------------------
# HCR functions

# Analysis functions:
# Called by get_hcr_ip()

# Get slope of last x years
get_slope <- function(input, year_range){
  # Slope using yr - 1, as catch only up to yr - 1
  # i.e. biomass is up to 2020, but catch only up 2019
  slope <- apply(input, 1, function(est_cpue_row){
  # y <- est_cpue_row[(max(yr-1) - mp_params$params["slope_years"] + 1):max(yr-1)]
    y <- est_cpue_row[year_range[1]:year_range[2]]
    est_cpue_row_slope <- lm(y ~ c(1:length(y)))$coefficients[2]
    return(est_cpue_row_slope)
  })
  return(slope)
}


# Alternate HCR:
# grad. * (CPUE / CPUEref) + (1 - grad.)
# Rearrange to:
# grad. ((CPUE / CPUEref) - 1) + 1 - should this be mean CPUE over X years? Same years as the gradient?
# Then x gain?
# Takes into account current CPUE (relative to reference) AND the gradient


# Analysis function - called by get_hcr_ip() to fill the hcr_ip slot
# This is then used by the get_hcr_op() function to fill the hcr_op slot
# That is then used by project() to update catch and effort in the next time step
# Here the hcr_ip is the slope of the relative estimated CPUE to be used in the HCR
# Current HCR - just the gradient of the relative CPUE
# multiplier = grad_relcpue * gain + 1
empirical_cpue_slope_ip <- function(stock, mp_params, stock_params, yr){
  #stock$estimated_cpue
  # Get CPUE relative to CPUE in last historical year
  #rel_est_cpue <- sweep(est_cpue, 1, est_cpue[,app_params$last_historical_timestep], "/")
  # Hard wired at 11
  rel_est_cpue <- sweep(stock$estimated_cpue, 1, stock$estimated_cpue[,10], "/")
  # Get the slope
  # yr up to 11, but stock$catch and effort only up to so use max(yr)-1
  year_range <- c((max(yr)-mp_params$params["slope_years"]),(max(yr)-1))
  est_cpue_slope <- get_slope(rel_est_cpue, year_range = year_range)
  # Pad out the early years with NA
  early_yrs <- matrix(NA,nrow=nrow(stock$hcr_ip), ncol=length(yr)-1)
  est_cpue_slope <- cbind(early_yrs, unname(est_cpue_slope))
  return(est_cpue_slope)
}

# Empirical shape function
# Stored as the hcr_shape in the MP switcheroo
# Called by get_hcr_op()
# Applies gain to the CPUE slope to produce catch output
# But is it catch scalar or real catch
empirical_cpue_slope_op <- function(input, mp_params, ...){
  output <- array(NA, dim=dim(input))
  
  # HCR function is a straight line from bottom-left to top-right
  # Input is slope
  # When slope < 0, output is < 1 (with a hard stop at 0)
  # When slope >= 0, output is >= 1 up to...
  # Gradient of HCR function is the 'gain' parameter
  # out = gain . slope + 1
  # If slope = 0, i.e. cpue is stable, output multiplier = 1
  # If slope is -ve, out is < 1 (min = 0)
  # If slope is +ve, put is > 1 (max = 10?)
  
  # To get to a particular CPUE, make this could be relative to target CPUE?
  output <- input * mp_params$params["gain"] + 1
  # Set some limits
  output[output < 0] <- 0
  # Limit on max increase?
  #output[output >= 10] <- 0
  
  
  return(output)
}



# asessment: a stock assessment that estimates biomass / k
# The analysis assessment function
# Called by get_hcr_ip()
assessment <- function(stock, mp_params, stock_params, yr){
  #browser()
  # Return observed biomass
  true_ip <- stock$biomass[,yr] / stock_params$k
  est_ip <- estimation_error(input =  true_ip, sigma = stock_params$biol_est_sigma, bias = stock_params$biol_est_bias)
  # Max depletion is 1.0
  est_ip <- pmin(est_ip, 1.0)
  return(est_ip)
}

# HCR shape functions
# Called by get_hcr_op()
threshold <- function(input, mp_params, ...){
  output <- array(NA, dim=dim(input))
  # Below lim
  output[input <= mp_params$params["lim"]] <- mp_params$params["min"]
  # On the slope
  grad <-  (mp_params$params["max"] - mp_params$params["min"]) / (mp_params$params["elbow"] - mp_params$params["lim"])
  on_slope <- (input > mp_params$params["lim"]) &  (input <= mp_params$params["elbow"])
  output[on_slope] <-   ((input - mp_params$params["lim"]) * grad  + mp_params$params["min"])[on_slope]
  # Past the elbow
  output[input > mp_params$params["elbow"]] <- mp_params$params["max"]
  return(output)
}

constant <- function(mp_params, ...){
  return(mp_params$params["constant_level"])
}

#-------------------------------------------------------------------
# Performance indicators

#' Get timesteps for the short-, medium- and long term time periods.
#'
#' Used when calculating the performance indicators over the three time periods.
#'
#' @param app_params A vector of application parameters.
#' @param nyears The number of years.
#' 
#' @return A list of the timesteps that make up each time period.
#' @examples
#' app_params <- list(initial_year = 2009, last_historical_timestep = 10)
#' nyears <- 40
#' get_time_periods(app_params=app_params, nyears=nyears)
#' @export
get_time_periods <- function(app_params, nyears){
  nyears <- nyears - app_params$last_historical_timestep
  term_length <- floor(nyears / 3)
  short_term_length <- term_length
  medium_term_length <- term_length
  long_term_length <- term_length
  extra_bit <- nyears - (term_length * 3)
  if(extra_bit == 1){
    long_term_length <- long_term_length + 1
  }
  if(extra_bit == 2){
    long_term_length <- long_term_length + 1
    medium_term_length <- medium_term_length + 1
  }
  short_term <- (app_params$last_historical_timestep + 1):(app_params$last_historical_timestep + short_term_length)
  medium_term <- (short_term[length(short_term)] + 1):(short_term[length(short_term)] + medium_term_length)
  long_term <- (medium_term[length(medium_term)] + 1):(medium_term[length(medium_term)] + medium_term_length)
  return(list(short_term=short_term, medium_term=medium_term, long_term=long_term))
}


#' Routines for calculating and displaying various performance indicators.
#'
#' current_pi_table() takes the processed indicators and formats them into a table.
#'
#' @param years A character vector of the years in the simulation.
#' @param piname_choice A character vector of the indicator names to be included in the table.
#' @param signif Significant digits for table. Default is 3.
#' @rdname performance_indicators
#' @export
current_pi_table <- function(dat, app_params, years, percentile_range = c(20,80), piname_choice=c("SB/SBF=0", "Prob. SB>LRP", "Catch", "Relative CPUE", "Catch variability", "Catch stability", "Relative effort", "Relative effort variability", "Relative effort stability", "Proximity to TRP"), signif=3){
  out <- subset(dat, period != "Rest" & piname %in% piname_choice)
  perc1 <- out[,paste("X",percentile_range[1],".",sep="")]
  perc2 <- out[,paste("X",percentile_range[2],".",sep="")]
  out$value <- paste(signif(out$X50., signif), " (", signif(perc1, signif), ",", signif(perc2, signif),")", sep="")
  # Except pi1 as it is a probability
  pi1value <- signif(out$X50., signif)
  out[out$pi=="pi1","value"] <- pi1value[out$pi=="pi1"]
  out <- out[,c("piname","period","value")]
  out <- tidyr::spread(out, key="period", value="value")
  # Rorder by piname_choice 
  out <- out[order(match(out$piname, piname_choice)),]
  rownames(out) <- out$piname
  # Drop rownames column
  out <- out[,colnames(out) != "piname"]
  # Mess about with colnames by adding year range
  time_periods <- get_time_periods(app_params, nyears=length(years))
  period_yrs <- lapply(time_periods, function(x){paste("<br>(",paste(years[x][c(1,length(x))], collapse="-"),")",sep="")})
  colnames(out)[1] <- paste(colnames(out)[1], " term", period_yrs$short_term, sep="")
  colnames(out)[2] <- paste(colnames(out)[2], " term", period_yrs$medium_term, sep="")
  colnames(out)[3] <- paste(colnames(out)[3], " term", period_yrs$long_term, sep="")
  return(out)
}

#' Routines for calculating and displaying various performance indicators.
#'
#' replicate_table() shows the final values for SB/SBF=0, catch and relative CPUE for each replicate.
#'
#' @param dat A data.frame with the 20th, 80th and 50th percentile value of each indicator.
#' @param percentile_range A vector of length with minimum and maximum percentile range to plot.
#' @rdname performance_indicators
#' @export
replicate_table <- function(stock, app_params, stock_params, percentile_range = c(20,80)){
  final_year <- max(as.numeric(dimnames(stock$biomass)$year))

  # Replicates some of the the get_summaries() function - inefficient
  sbsbf0 <- stock$biomass[,ncol(stock$biomass) ] / stock_params$k
  catch <- stock$catch[,ncol(stock$catch)]
  cpue <- stock$catch / stock$effort
  rel_cpue <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
  rel_cpue <- rel_cpue[,ncol(rel_cpue)]

  # Get median and percentiles
  signif <- 2
  sbsbf0_qs <- signif(quantile(sbsbf0, probs=c(percentile_range[1], 50, percentile_range[2])/100),signif)
  sbsbf0_summary <- paste(sbsbf0_qs[2], " (", sbsbf0_qs[1], ",", sbsbf0_qs[3], ")", sep="")
  catch_qs <- signif(quantile(catch, probs=c(percentile_range[1], 50, percentile_range[2])/100), signif)
  catch_summary <- paste(catch_qs[2], " (", catch_qs[1], ",", catch_qs[3], ")", sep="")
  rel_cpue_qs <- signif(quantile(rel_cpue, probs=c(percentile_range[1], 50, percentile_range[2])/100), signif)
  rel_cpue_summary <- paste(rel_cpue_qs[2], " (", rel_cpue_qs[1], ",", rel_cpue_qs[3], ")", sep="")

  # Put together
  out <- data.frame(Replicate= c(1:length(sbsbf0),"Median and range"), sbsbf0=c(signif(sbsbf0, signif), sbsbf0_summary), Catch=c(signif(catch, signif), catch_summary), rel_cpue=c(signif(rel_cpue, signif), rel_cpue_summary))
  colnames(out)[2] <- "Final SB/SBF=0"
  colnames(out)[3] <- "Final catch"
  colnames(out)[4] <- "Final relative CPUE"
  return(out)
}

# The big PI tables for comparing HCRs
#' pitable
#'
#' pitable() is not a plot but a table comparing PIs across HCRs and periods. Only pass in 1 time period at a time.
#'
#' @param signif Number of significent digits for table. Default is 3.
#'
#' @return A data.frame to be shown as a table.
#' @rdname comparison_plots
#' @name Comparison plots
#' @export
# Added signif argument to be added to AMPLE
pitable <- function(dat, percentile_range = c(20,80), signif=3){
    # Rows are the PIs, columns are the HCRs
    percentile_min <- dat[,paste("X",percentile_range[1],".",sep="")]
    percentile_max <- dat[,paste("X",percentile_range[2],".",sep="")]
    dat$value <- paste(signif(dat$X50.,signif), " (", signif(percentile_min, signif), ",", signif(percentile_max, signif), ")", sep="")
    # Fix pi1
    dat[dat$pi=="pi1", "value"] <- signif(dat[dat$pi=="pi1", "X50."],signif)
    tabdat <- dat[,c("hcrref", "piname", "value")]
    tabdat[tabdat$name=="Biomass","piname"] <- "SB/SBF=0"
    tabdat <- as.data.frame(tidyr::spread(tabdat, key="hcrref", value="value"))
    # Have rownames?
    #rnames <- tabdat[,1]
    #tabdat <- tabdat[,-1]
    #rownames(tabdat) <- rnames
    colnames(tabdat)[1] <- "Indicator"
    return(tabdat)
}


#-------------------------------------------------------------------
## @references Ranta and Kaitala 2001 Proc. R. Soc.
## vt = b * vt-1 + s * sqrt(1 - b^2)
## s is normally distributed random variable with mean = 0
## b is the autocorrelation parameter > -1 and < 1
## e.g. -0.8 very blue, 0.8 red

# New package environment for keeping the current value of the autocorrelated noise
# This is a pretty ugly solution - else have create at start and pass around
# We create current_corrnoise object in this environment in the reset_stock() function
pkg_env <- new.env()

next_corrnoise <- function(x, b, sd=0.1){
  # Each iter needs a separate noise (i.e. not correlated across iters)
  s <- rnorm(length(x),mean=0,sd=sd)
  nextx <- b * x + s * sqrt(1 - b^2)
  return(nextx)
}

#----------------------------------------------------
# Function to get the performance indicators in same format as PIMPLE

#' peformance_indicators
#'
#' get_summaries() gets the current indicators including SB/SBF=0, catch and the probability of being above the limit reference point.
#'
#' @param quantiles The quantiles to calculate the indicators over.
#' @return Various data.frames with summaries in different formats.
#' @rdname performance_indicators
#' @examples
#' # Set up all the bits for a projection - should be done inside a Shiny app
#' # Managment procedure bits - should come from Shiny app inputs
#' input_mp <- list(
#'   blim_belbow = c(0.2, 0.5),
#'   cmin_cmax = c(10, 140), 
#'   constant_catch_level = 50,
#'   constant_effort_level = 1,
#'   emin_emax = c(0.1, 0.5),
#'   hcr_type = "threshold_catch")
#' mp_params <- mp_params_switcheroo(input_mp)
#' 
#' # Stochasticity bits - should come from Shiny app inputs
#' input_stoch <- list(
#'   biol_est_bias = 0,
#'   biol_est_sigma = 0,
#'   biol_prod_sigma = 0, 
#'   show_var <- TRUE)
#' stoch_params <- set_stoch_params(input_stoch)
#' 
#' # Life history bits - should come from Shiny app inputs
#' input_lh <- list(
#'   stock_history = "fully",
#'   stock_lh = "medium")
#' lh_params <- get_lh_params(input_lh)
#' 
#' # Stitch together and make other parameters - should be inside an Shiny app 
#' stock_params <- c(stoch_params, lh_params)
#' app_params <- list(initial_year = 2009, last_historical_timestep = 10)
#' 
#' # Make the null stock and fill it up
#' # In a Shiny app use the create_stock() function but cannot do here so make an equivalent stock
#' stock <- list(biomass = NULL, hcr_ip = NULL, hcr_op = NULL, effort = NULL, catch = NULL)
#' stock <- reset_stock(stock = stock, stock_params = stock_params, mp_params = mp_params,
#'   app_params = app_params, initial_biomass = stock_params$b0, nyears = 20, niters = 10)
#' # Finally project over the timesteps
#' out <- project(stock, timesteps = c(11,20), stock_params = stock_params, mp_params = mp_params,
#'   app_params = app_params)
#' # Get the summaries
#' pisums <- get_summaries(stock=out, stock_params=stock_params, app_params=app_params,
#'   quantiles=c(0.01,0.05,0.20,0.5,0.80,0.95,0.99))
#' # Get the current PI table in a neat format from one of the summary tables
#' current_pi_table(dat=pisums$periodqs, app_params=app_params,
#' years=dimnames(stock$biomass)$year,
#' piname_choice=c("SB/SBF=0", "Prob. SB>LRP", "Catch"))
#'
#' @export
get_summaries <- function(stock, stock_params, app_params, quantiles){
  # worms - a sample of iters by year
  # yearqs - the quantiles by year
  # periodqs - average over the periods and take quantiles
  # Make these match the PIMPLE indicators
  # SBSBF0
  sbsbf0 <- as.data.frame(stock$biomass / stock_params$k)
  sbsbf0$iter <- 1:nrow(sbsbf0)
  sbsbf0 <- tidyr::gather(sbsbf0, key="year", value="data", -iter, convert=TRUE)
  sbsbf0 <- cbind(sbsbf0, pi="biomass", piname="SB/SBF=0", upsidedown=FALSE)

  # Prob SBSBF0 > LRP
  problrp <- dplyr::group_by(sbsbf0, year)
  problrp <- dplyr::summarise(problrp, data=mean(data > stock_params$lrp, na.rm=TRUE))
  problrp <- cbind(problrp, pi="pi1", piname="Prob. SB>LRP", upsidedown=FALSE, iter=1) # Called pi1 inline with PIMPLE

  # Catch
  catch <- as.data.frame(stock$catch)
  catch$iter <- 1:nrow(catch)
  catch <- tidyr::gather(catch, key="year", value="data", -iter, convert=TRUE)
  catch <- cbind(catch, pi="pi3", piname="Catch", upsidedown=FALSE)


  # CPUE - pi4
  cpue <- stock$catch / stock$effort
  rel_cpue <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
  rel_cpue <- as.data.frame(rel_cpue)
  rel_cpue$iter <- 1:nrow(rel_cpue)
  rel_cpue <- tidyr::gather(rel_cpue, key="year", value="data", -iter, convert=TRUE)
  rel_cpue <- cbind(rel_cpue, pi="pi4", piname="Relative CPUE", upsidedown=FALSE)

  # Catch var and stab - need drop=FALSE if iter == 1
  catch_diff <- abs(stock$catch[,2:ncol(stock$catch), drop=FALSE] - stock$catch[,1:(ncol(stock$catch)-1), drop=FALSE])
  max_catch_diff <- stock_params$k / 10 # For rescale - Has to be same for all stocks - could base it on Cmax-Cmin from HCR control?
  catch_stab <- (max_catch_diff - catch_diff) / max_catch_diff # rescale
  catch_stab[catch_stab < 0] <- 0
  #temp <- cbind(rep(NA, nrow(catch_diff)), catch_diff) # Add extra year?
  catch_diff <- as.data.frame(catch_diff)
  catch_diff$iter <- 1:nrow(catch_diff)
  catch_diff <- tidyr::gather(catch_diff, key="year", value="data", -iter, convert=TRUE)
  catch_diff <- cbind(catch_diff, pi="pi6", piname="Catch variability", upsidedown=TRUE)
  catch_stab <- as.data.frame(catch_stab)
  catch_stab$iter <- 1:nrow(catch_stab)
  catch_stab <- tidyr::gather(catch_stab, key="year", value="data", -iter, convert=TRUE)
  catch_stab <- cbind(catch_stab, pi="pi6", piname="Catch stability", upsidedown=FALSE)

  # Relative effort
  rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  # Relative effort var
  rel_effort_diff <- abs(rel_effort[,2:ncol(rel_effort), drop=FALSE] - rel_effort[,1:(ncol(rel_effort)-1), drop=FALSE])
  # Relative effort stab
  max_rel_effort_diff <- 1
  rel_effort_stab <- (max_rel_effort_diff - rel_effort_diff) / max_rel_effort_diff # rescale
  rel_effort_stab[rel_effort_stab < 0] <- 0

  rel_effort <- as.data.frame(rel_effort)
  rel_effort$iter <- 1:nrow(rel_effort)
  rel_effort <- tidyr::gather(rel_effort, key="year", value="data", -iter, convert=TRUE)
  rel_effort <- cbind(rel_effort, pi="pi20", piname="Relative effort", upsidedown=TRUE)

  rel_effort_diff <- as.data.frame(rel_effort_diff)
  rel_effort_diff$iter <- 1:nrow(rel_effort_diff)
  rel_effort_diff <- tidyr::gather(rel_effort_diff, key="year", value="data", -iter, convert=TRUE)
  rel_effort_diff <- cbind(rel_effort_diff, pi="pi7", piname="Relative effort variability", upsidedown=TRUE)

  rel_effort_stab <- as.data.frame(rel_effort_stab)
  rel_effort_stab$iter <- 1:nrow(rel_effort_stab)
  rel_effort_stab <- tidyr::gather(rel_effort_stab, key="year", value="data", -iter, convert=TRUE)
  rel_effort_stab <- cbind(rel_effort_stab, pi="pi7", piname="Relative effort stability", upsidedown=FALSE)

  # Proximity to TRP
  sbsbf02 <- as.data.frame(stock$biomass / stock_params$k) # calced again - maybe rethink?
  max_distance_from_trp <- max(stock_params$trp, 1 - stock_params$trp)
  prox_trp <- pmax(1.0 - (abs(sbsbf02 - stock_params$trp) / max_distance_from_trp), 0.0)
  prox_trp <- as.data.frame(prox_trp)
  prox_trp$iter <- 1:nrow(prox_trp)
  prox_trp <- tidyr::gather(prox_trp, key="year", value="data", -iter, convert=TRUE)
  prox_trp <- cbind(prox_trp, pi="pi8", piname="Proximity to TRP", upsidedown=FALSE)

  # F / FMSY
  fmsy <- stock_params$r / 2
  f <- stock$catch / stock$biomass 
  ffmsy <- as.data.frame(f / fmsy)
  ffmsy$iter <- 1:nrow(ffmsy)
  ffmsy <- tidyr::gather(ffmsy, key="year", value="data", -iter, convert=TRUE)
  ffmsy <- cbind(ffmsy, pi="ffmsy", piname="F/FMSY", upsidedown=TRUE)


  # Combine all 
  dat <- rbind(problrp, sbsbf0, catch, rel_cpue, catch_diff, catch_stab, rel_effort, rel_effort_diff, rel_effort_stab, prox_trp, ffmsy)

  # Period table and add to the dat
  years <- dimnames(stock$biomass)$year
  time_periods <- get_time_periods(app_params, nyears=dim(stock$biomass)[2])
  short_term <- years[time_periods[["short_term"]]]
  medium_term <- years[time_periods[["medium_term"]]]
  long_term <- years[time_periods[["long_term"]]]
  rest <- years[1:(time_periods$short_term[1]-1)]
  periods <- rbind(data.frame(period = "Rest", year=rest),
      data.frame(period = "Short", year=short_term),
      data.frame(period = "Medium", year=medium_term),
      data.frame(period = "Long", year=long_term))
  
  # Add period
  dat <- merge(dat, periods)

  # Get summaries etc
  # Get averages by periods, then quantiles
  periodqs <- dplyr::group_by(dat, pi, piname, upsidedown, period, iter)
  periodqs <- dplyr::summarise(periodqs, data=mean(data, na.rm=TRUE))
  periodqs <- dplyr::do(periodqs, data.frame(t(quantile(.$data, probs=quantiles, na.rm=TRUE))))
  # Get quantiles of each year
  yearqs <- dplyr::group_by(dat, pi, piname, upsidedown, year)
  yearqs <- dplyr::do(yearqs, data.frame(t(quantile(.$data, probs=quantiles, na.rm=TRUE))))
  # Get worms
  nworms <- 5
  if (length(unique(dat$iter)) < nworms){
    wormiters <- unique(dat$iter)
  }
  else {
    wormiters <- sample(unique(dat$iter),nworms)
  }
  worms <- subset(dat, iter %in% wormiters)
  # rename iter column to wormid
  worms <- dplyr::rename(worms, wormid = iter)

  # Data.frames only
  return(list(worms=as.data.frame(worms), yearqs=as.data.frame(yearqs), periodqs=as.data.frame(periodqs)))
}
