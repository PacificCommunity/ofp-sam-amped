# Common functions for AMPED and PIMPLE
# 
# Copyright 2019 Finlay Scott. Distributed under the GPL 3
# Maintainer: Finlay Scott, SPC
#

# This looks pretty crummy
#' @export
amped_maintainer_and_licence <- function(){
  out <- tags$html(
    tags$h1("AMPED"),
    tags$p("Amazing Management Procedures Exploration Device"),
    tags$footer(
      tags$p("version 0.2.0 Drayton Manored"),
      tags$p("Copyright 2019 OFP SPC MSE Team."),
      tags$p("Distributed under the GPL 3"),
      tags$a("Soure code", href="https://github.com/PacificCommunity/ofp-sam-mse-popmodel-Shiny/tree/master/AMPED")
    )
  )
  return(out)
}


# Make the empty reactive stock object
# The stock object is not reactive, but the elements inside it are (it's like a list)
# stock cannot just be a calculated value returned from a reactive() as it needs to persist
# i.e. here the next timestep depends on the previous timestep
# it's empty but has structure
#' @export
create_stock <- function(){
  stock <- reactiveValues(
    biomass = NULL,
    hcr_ip = NULL,
    hcr_op = NULL,
    effort = NULL,
    catch = NULL
  )
  return(stock)
}

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
  # The IP and OP functions must work for iters
  # yr argument is the timestep range of the HCR IP - i.e. lagging behind catch
  # Should be able to handle multiple iters
  stock$hcr_ip[,hcr_ip_yrs] <- get_hcr_ip(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=hcr_ip_yrs)
  stock$hcr_op[,hcr_op_yrs] <- get_hcr_op(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=hcr_ip_yrs)
  stock$effort <- stock$catch / (stock$biomass * stock_params[["q"]])
  # Not sure this return statement is needed but means we can call this function from tests
  return(stock)
}

# Clear out stock and refill initial period
#' @export
reset_stock <- function(stock, stock_params, mp_params, app_params, initial_biomass, nyears, niters){
  # Set up current_corrnoise object to store the current noise value for each iteration
  # Stored in the package global environment
  pkg_env$current_corrnoise <- rep(0, niters)
  #
  stock <- clear_stock(stock=stock, app_params=app_params, nyears, niters)
  stock <- fill_initial_stock(stock=stock, stock_params=stock_params, mp_params=mp_params, initial_biomass=initial_biomass, app_params=app_params)
  # Not sure this return statement is needed but means we can call this function from tests
  return(stock)
}

# quantiles is of length 2, lower and upper
#' @export
get_timeseries <- function(stock=stock, stock_params=stock_params, app_params=app_params, quantiles=quantiles, nspaghetti=5){
  # Make a data.frame with year, metric, quantile, value
  # quantile are based on the lower and upper quantile
  # Do we also want spaghetti? could do
  # metrics are B/K, Catch, Rel. CPUE, F/FMSY 
  bk <- stock$biomass / stock_params$k
  bkq <- get_quantiles(bk, quantiles)
  bkq <- cbind(metric="bk", name="SB/SBF=0", level=c("lower","median","upper"),as.data.frame(bkq))

  catchq <- get_quantiles(stock$catch, quantiles)
  catchq <- cbind(metric="catch", name="Catch", level=c("lower","median","upper"),as.data.frame(catchq))

  cpue <- stock$catch / stock$effort
  rel_cpue <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
  rel_cpueq <- get_quantiles(rel_cpue, quantiles)
  rel_cpueq <- cbind(metric="relcpue", name="Rel. CPUE", level=c("lower","median","upper"),as.data.frame(rel_cpueq))

  fmsy <- stock_params$r / 2
  f <- stock$catch / stock$biomass 
  ffmsy <- f / fmsy
  ffmsyq <- get_quantiles(ffmsy, quantiles)
  ffmsyq <- cbind(metric="ffmsy", name="F / FMSY", level=c("lower","median","upper"),as.data.frame(ffmsyq))

  qout <- rbind(bkq, catchq, rel_cpueq, ffmsyq)
  qout <- tidyr::gather(qout, key="year", value="value",-level, -metric, -name)
  qout <- cbind(qout, type="quantile")

  # Spaghetti
  # Pick 5 iters at random
  nspaghetti <- min(nspaghetti, dim(bk)[1])
  spaghetti_iters <- round(runif(nspaghetti, min=1, max=dim(bk)[1]))
  bksp <- cbind(metric="bk", name="SB/SBF=0", level=paste("spag",as.character(spaghetti_iters),sep=""),as.data.frame(bk[spaghetti_iters,]))
  catchsp <- cbind(metric="catch", name="Catch", level=paste("spag",as.character(spaghetti_iters),sep=""),as.data.frame(stock$catch[spaghetti_iters,]))
  ffmsysp <- cbind(metric="ffmsy", name="F / FMSY", level=paste("spag",as.character(spaghetti_iters),sep=""),as.data.frame(ffmsy[spaghetti_iters,]))
  rel_cpuesp <- cbind(metric="relcpue", name="Rel. CPUE", level=paste("spag",as.character(spaghetti_iters),sep=""),as.data.frame(rel_cpue[spaghetti_iters,]))
  spout <- rbind(bksp, catchsp, rel_cpuesp, ffmsysp)
  spout <- tidyr::gather(spout, key="year", value="value",-level, -metric, -name)
  spout <- cbind(spout, type="spaghetti")
  
  out <- rbind(qout,spout)
  # Producing warnings about factors?
  out$level <- as.character(out$level)
  out$type <- as.character(out$type)
  out$metric <- as.character(out$metric)
  out$name <- as.character(out$name)
  out$year <- as.numeric(out$year)
  return(out)
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
#' @export
get_hcr_op <- function(stock, stock_params, mp_params, yr){
  # Shape is not NA
  # Call HCR with the lagged input
  #hcr_op <- do.call(mp_params$hcr_shape, args=list(stock=stock, mp_params=mp_params, stock_params=stock_params, yr=yr))
  hcr_op <- do.call(mp_params$hcr_shape, args=list(input=stock$hcr_ip[,yr,drop=FALSE], mp_params=mp_params, stock_params=stock_params, yr=yr))
  return(hcr_op)
}

#-------------------------------------------------------------------
# HCR functions

# Analysis functions:
# Called by get_hcr_ip()
# asessment: a stock assessment that estimates biomass / k
# The analysis assessment function
assessment <- function(stock, mp_params, stock_params, yr){
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

# Other methods are available
transform_upside_down_pis <- function(x){
  #min_value <- 1e-6
  #adjuster <- -1.0001
  #x <- pmax(x, min_value)
  #x <- log(x) + log(min_value) * adjuster # Now x is between just greater than 0 and something positive
  #x <- (log(min_value) + log(min_value) * adjuster) / (x)
  #return(x)
  return(-x) # Just return the negative - simple!
}

# Get pis by year and iter
# Stick into a single data.frame
# @export
get_pis <- function(stock, stock_params, mp_params, app_params, pi_choices = NULL){
  rel_year <- app_params$initial_year + app_params$last_historical_timestep - 1
  bkm <- stock$biomass / stock_params$k
  # B/K
  bk <- cbind(pi="bk", iter=1:nrow(bkm), as.data.frame(bkm), name="SB/SBF=0")
  # Total catch
  catch <- cbind(pi="catch", iter=1:nrow(stock$catch), as.data.frame(stock$catch), name="Catch")
  # Variability in catch
  diffcatch <- abs(t(apply(stock$catch, 1, diff))) # Need to transpose due to odd apply() behaviour
  #diffcatch <- transform_upside_down_pis(diffcatch)
  # Stick some NAs for the first column and name column
  diffcatch <- cbind(NA, diffcatch)
  colnames(diffcatch)[1] <- colnames(stock$catch)[1]
  diffcatch <- cbind(pi="diffcatch", iter=1:nrow(diffcatch), as.data.frame(diffcatch), name="Catch variability")
  # Relative effort
  # Base effort is effort in the last historical timestep
  releffort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  releffort <- cbind(pi="releffort", iter=1:nrow(releffort), as.data.frame(releffort), name=paste("Effort (rel. ", rel_year," )", sep=""))
  # Variability of relative effort
  diffeffort <- abs(t(apply(stock$effort, 1, diff))) # Need to transpose due to odd apply() behaviour
  #diffeffort <- transform_upside_down_pis(diffeffort)
  diffeffort <- cbind(NA, diffeffort)
  colnames(diffeffort)[1] <- colnames(stock$effort)[1]
  diffeffort <- cbind(pi="diffeffort", iter=1:nrow(diffeffort), as.data.frame(diffeffort), name="Effort variability")
  # Relative CPUE
  cpue <- stock$catch / stock$effort
  relcpuem <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
  relcpue <- cbind(pi="relcpue", iter=1:nrow(relcpuem), as.data.frame(relcpuem), name=paste("CPUE (rel. ", rel_year, " )", sep=""))
  # Variability of relative CPUE
  diffcpue <- abs(t(apply(relcpuem, 1, diff))) # Need to transpose due to odd apply() behaviour
  #diffcpue <- transform_upside_down_pis(diffcpue)
  diffcpue <- cbind(NA, diffcpue)
  colnames(diffcpue)[1] <- colnames(stock$effort)[1]
  diffcpue <- cbind(pi="diffcpue", iter=1:nrow(diffcpue), as.data.frame(diffcpue), name="CPUE variability")

  # Probabilities in each year
  problrp <- apply(bkm > stock_params$lrp,2,mean)
  # A bit of mucking about to get a data.frame
  problrp <- cbind(pi="problrp", iter=1, as.data.frame(t(problrp)), name="Prob. SB > LRP")
  
  # pi_choices is a character string of these objects
  # Put altogether and reshape
  if (is.null(pi_choices)){
    out <- rbind(bk, problrp, catch, diffcatch, releffort, diffeffort, relcpue, diffcpue)
  }
  # Else only bind together the chosen pis
  else{
    out <- do.call(rbind, lapply(pi_choices,function(x) get(x)))
  }
  
  # Use tidyr or something else to put into a single df
  out <- tidyr::gather(out, key="year", value="value", -iter, -pi, -name)

  # F***ing factors!
  out$name <- as.character(out$name)
  return(out)
}

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

# Calc the PIs
#' @export
get_summary_pis <- function(stock, stock_params, mp_params, app_params, quantiles, ...){
  years <- dimnames(stock$biomass)$year
  # Average them over years? S, M, L?
  # Average over years - then calc quantiles
  time_periods <- get_time_periods(app_params, nyears=dim(stock$biomass)[2])
  short_term <- years[time_periods[["short_term"]]]
  medium_term <- years[time_periods[["medium_term"]]]
  long_term <- years[time_periods[["long_term"]]]

  pis <- get_pis(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, ...)
  pis <- pis[pis$year %in% c(short_term, medium_term, long_term),]

  # Add timeperiod column - as.data.frame to use stringsAsFactors
  pis <- as.data.frame(cbind(pis, term=as.character(NA), stringsAsFactors=FALSE))
  pis[pis$year %in% short_term,"term"] <- "short"
  pis[pis$year %in% long_term,"term"] <- "long"
  pis[pis$year %in% medium_term,"term"] <- "medium"
  # Drop the rows that do not have a term
  # This seems to take a long time

  # Take median value over the timeperiods

  #yearav <- pis %>% group_by(pi, iter, term, name) %>% summarise(value=median(value, na.rm=TRUE))
  temp <- dplyr::group_by(pis, pi, iter, term, name) 
  yearav <- dplyr::summarise(temp, value=median(value, na.rm=TRUE))

  # Take the quantiles - 
  qmed <- c(quantiles, 0.5)
  #piqs <- yearav %>% group_by(pi, term, name) %>% summarise(quantiles = list(paste("q",qmed*100,sep="")), value = list(quantile(value, qmed))) %>% unnest
  temp <- dplyr::group_by(yearav, pi, term, name)
  temp <- dplyr::summarise(temp, quantiles = list(paste("q",qmed*100,sep="")), value = list(quantile(value, qmed)))
  piqs <- tidyr::unnest(temp)



  return(list(piqs=piqs, terms=list(short=short_term, medium=medium_term, long=long_term)))
}

get_quantiles <- function(dat, quantiles){
  qs <- apply(dat, 2, function(x){quantile(x, probs=c(quantiles[1], 0.5, quantiles[2]), na.rm=TRUE)})
  return(qs)
}

#' @export
current_pi_table <- function(pis){
  pis$piqs
  piqs <- pis$piqs
  # Beat this into a table
  # Each row is a PI
  # 4 Columns: Name, short, medium, long
  # median (quantile)

  signif <- 2
  upis <- as.character(unique(piqs$pi))
  uterm <- as.character(unique(piqs$term))
  out <- data.frame()
  for (upi in upis){
    tempdat <- subset(piqs, term =="short" & pi==upi)
    svalues <- signif(sort(subset(piqs, term =="short" & pi==upi)$value),signif)
    mvalues <- signif(sort(subset(piqs, term =="medium" & pi==upi)$value),signif)
    lvalues <- signif(sort(subset(piqs, term =="long" & pi==upi)$value),signif)
    # If no uncertainty
    if ((svalues[2] == svalues[1]) & (svalues[4] == svalues[5]) &
        (mvalues[2] == mvalues[1]) & (mvalues[4] == mvalues[5]) &
        (lvalues[2] == lvalues[1]) & (lvalues[4] == lvalues[5])){
      sval <- as.character(svalues[3])
      mval <- as.character(mvalues[3])
      lval <- as.character(lvalues[3])
    }
    else {
      sval <- paste(svalues[3], " (", svalues[2], ",", svalues[4],")", sep="")
      mval <- paste(mvalues[3], " (", mvalues[2], ",", mvalues[4],")", sep="")
      lval <- paste(lvalues[3], " (", lvalues[2], ",", lvalues[4],")", sep="")
    }
    out <- rbind(out, data.frame(name = tempdat$name[1], short = sval, medium=mval, long=lval))
  }
  rownames(out) <- out$name
  # Drop rownames column
  out <- out[,colnames(out) != "name"]
  term_years <- pis$term
  colnames(out) <- c(paste("Short (", term_years$short[1], "-", term_years$short[length(term_years$short)],")", sep=""),
                     paste("Med. (", term_years$medium[1], "-", term_years$medium[length(term_years$medium)],")", sep=""),
                     paste("Long (", term_years$long[1], "-", term_years$long[length(term_years$long)],")", sep=""))

  return(out)
}

#' @export
big_pi_table <- function(pis, hcr_choices, pi_choices, term_choice="long"){
  if (length(pis) == 0){
    return()
  }
  # Put into one big DF
  piqs <- lapply(pis, function(x) return(x$piqs))
  piqs <- dplyr::bind_rows(piqs, .id="hcr")

  # Sanity check - if all the hcr choices are not in the PI table then something has gone wrong
  if (!all(hcr_choices %in% unique(piqs$hcr))){
    stop("In big_pi_table(). Not all hcr_choices are in the hcr list.\n")
  }
  signif <- 2
  # Big fuck-off table: PIs along top, HRCs in the rows
  # Just the median and two quantiles
  # which quantiles do we want? drop the "q" from the front and order
  qs <- sort(as.numeric(substring(unique(piqs$quantiles),2)))
  qs <- paste("q", qs[2:4],sep="")
  piqs <- subset(piqs, (hcr %in% hcr_choices) & (name %in% pi_choices) & (term == term_choice) & (quantiles %in% qs))
  piqs$value <- signif(piqs$value, signif)
  dat <- tidyr::spread(piqs, key="quantiles", value="value")
  # Make the text values
  dat$valtxt <- as.character(NA)
  # Which rows have same qs - no uncertainty
  certain_rows <- c((dat[,qs[2]] == dat[,qs[1]]) & (dat[,qs[2]] == dat[,qs[3]]))
  # uncertain rows
  uncertain_valtx <- paste(unlist(dat[,qs[2]])," (",
        unlist(dat[,qs[1]]),",",
        unlist(dat[,qs[3]]),")",sep="")[!certain_rows]
  certain_valtx <- paste(unlist(dat[,qs[2]]))[certain_rows]
  dat$valtxt[certain_rows] <- certain_valtx
  dat$valtxt[!certain_rows] <- uncertain_valtx

  dat <- dat[,c("hcr","name","valtxt")]
  out <- as.data.frame(tidyr::spread(dat, key="name", value="valtxt"))
  rownames(out) <- out$hcr
  out <- out[,colnames(out) != "hcr"]
  return(out)
}

# PIs for the IntroProjection app
# Each row in the stock is a separate projection
#' @export
get_projection_pis <- function(stock, stock_params, app_params, current_timestep){
  # Return a data.frame of:
  # Projection (1,2,3,..)
  # Last catch
  # Av. catch
  # Last effort
  # Av. effort
  # Last SB/SBF=0
  # Av. SB/SBF=0
  # Prop. years in green

  # Current values are that last value that isn't NA
  final_catch <- apply(stock$catch, 1, function(x)x[max(which(!is.na(x)))])
  bk <- stock$biomass / stock_params$k
  final_sbsbf0 <- apply(bk, 1, function(x)x[max(which(!is.na(x)))])
  rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  final_effort <- apply(rel_effort, 1, function(x)x[max(which(!is.na(x)))])

  # Average values over projection period only
  proj_period <- (app_params$last_historical_timestep+1):dim(stock$biomass)[2]
  mean_catch <- apply(stock$catch[,proj_period,drop=FALSE],1,mean,na.rm=TRUE)
  mean_sbsbf0 <- apply(bk[,proj_period,drop=FALSE],1,mean,na.rm=TRUE)
  mean_effort <- apply(rel_effort[,proj_period,drop=FALSE],1,mean,na.rm=TRUE)
  prop_sb_lrp <- apply(bk[,proj_period,drop=FALSE] > stock_params$lrp,1,mean,na.rm=TRUE)
  bmsy <- stock_params$k / 2
  prop_sb_bmsy <- apply(stock$biomass[,proj_period,drop=FALSE] > bmsy,1,mean,na.rm=TRUE)
  dat <- data.frame(Projection = 1:nrow(stock$biomass),
                    final_catch = final_catch,
                    average_catch = mean_catch,
                    final_effort = final_effort,
                    average_effort = mean_effort,
                    final_sbsbf0 = final_sbsbf0,
                    average_sbsbf0 = mean_sbsbf0,
                    prop_sb_lrp = prop_sb_lrp,
                    prop_sb_bmsy = prop_sb_bmsy)
  # Trim the digits a bit
  #dat <- signif(dat,3)
  dat <- round(dat,3)
  final_year <- app_params$initial_year + current_timestep - 1
  # Better column names
  colnames(dat) <- c("Projection",
                     "Last catch",
                     "Average catch",
                     "Last effort",
                     "Average effort",
                     "Last SB/SBF=0",
                     "Average SB/SBF=0",
                     "Prop. SB/SBF=0 > LRP",
                     "Prop. B > BMSY")
  return(dat)
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

