# Attempting to put together an R6 class for the stock object
# Some of this might need to be reactive - can't remember why stock is reactive in the originals

#source("hcr_funcs.R")


# Put this somewhere
## @references Ranta and Kaitala 2001 Proc. R. Soc.
## vt = b * vt-1 + s * sqrt(1 - b^2)
## s is normally distributed random variable with mean = 0
## b is the autocorrelation parameter > -1 and < 1
## e.g. -0.8 very blue, 0.8 red

# New package environment for keeping the current value of the autocorrelated noise
# This is a pretty ugly solution - else have create at start and pass around
# We create current_corrnoise object in this environment in the reset_stock() function
# pkg_env <- new.env()

next_corrnoise <- function(x, b, sd=0.1){
  # Each iter needs a separate noise (i.e. not correlated across iters)
  s <- rnorm(length(x),mean=0,sd=sd)
  nextx <- b * x + s * sqrt(1 - b^2)
  return(nextx)
}

#n <- rep(NA, 100)
#n[1] <- 1
#for (i in 2:length(n)){
#  n[i] <- next_corrnoise(n[i-1], 0.5, 0.2)
#}


# Continue with fill initial method - HCR ip and op
# project
# Figure out modules and put it all together


# Make all of this public

#' R6 Class representing a stock
#'
#' A stock has biomass, effort, catch, estimated_cpue and hcr_ip and hcr_op
Stock <- R6::R6Class("Stock",
  public = list(
    #' @field biomass Array of biomass
    biomass = NULL,
    #' @field catch Array of catches
    catch = NULL,
    #' @field effort Array of fishing effort
    effort = NULL,
    #' @field hcr_ip Array of HCR input signals
    hcr_ip = NULL,
    #' @field hcr_op Array of HCR output signals
    hcr_op = NULL,
    #' @field hcr_op Array of estimated CPUE
    #estimated_cpue = NULL,
    #' @field msy MSY (default = 100).
    msy = NULL,
    #' @field r Growth rate (default = 0.6). Set by the user in the app.
    r = NULL, # or slow 0.2 or fast 1.0 depending on input selection (default = 0.6).
    #' @field k Carrying capacity (default = NULL - set by msy and r when object is initialised).
    k = NULL,
    #' @field p Shape of the production curve (default = 1).
    p = NULL,
    #' @field q Catchability (default = 1).
    q = 1,
    #' @field lrp Limit reference point, expressed as depletion (default = 0.2).
    lrp = 0.2,
    #' @field lrp Limit reference point, expressed as depletion (default = 0.5).
    trp = 0.5,
    #' @field b0 Virgin biomass (default = NULL - set by msy and r when object is initialised).
    b0 = NULL,
    #' @field current_corrnoise Stores the current values of the correlated noise (by iteration).
    current_corrnoise = NULL,
    #' Biological variability
    biol_sigma = NULL,
    #' last historical timestep
    last_historical_timestep = NULL,


    # new() method will call this
    #' @description
    #' Create a new stock object, with fields of the right dimension and NA values.
    #' Equivalent to the old 'clear_stock' function.
    #' @param stock_params A list with essential elements: r (growth rate, numeric, default=6), stock_history (string: "fully", "over", "under", default="fully") initial_year (integer, default=2000), last_historical_timestep (integer, default=10), nyears (integer, default=30), biol_sigma (numeric, default = 0).
    #' @param mp_params A list of the MP parameters. Used to fill HCR ip and op.
    #' @param niters The number of iters in the stock (default = 1).
    #' @return A new Stock object.
    initialize = function(stock_params = list(r = 0.6, stock_history = "fully", nyears = 30, initial_year = 2000, last_historical_timestep=10, biol_sigma = 0), mp_params = NULL, niters = 1, ...){
      print("Initialising stock with NAs")
      # nyears must be greater than stock_params$last_historical_timestep
      nyears <- max(stock_params$last_historical_timestep+1, round(stock_params$nyears))
      initial_array <- array(NA, dim=c(niters, nyears), dimnames=list(iter=1:niters, year=stock_params$initial_year:(stock_params$initial_year+nyears-1)))
      self$biomass <- initial_array
      self$hcr_ip <- initial_array
      self$hcr_op <- initial_array
      self$catch <- initial_array
      self$effort <- initial_array
      #self$estimated_cpue <- initial_array
      self$p <- 1
      self$r <- stock_params$r
      self$msy <- 100
      self$k <- 4 * self$msy / self$r
      self$b0 <- self$k * 2/3
      self$current_corrnoise <-  rep(0, niters)
      self$biol_sigma = stock_params$biol_sigma
      self$last_historical_timestep <- stock_params$last_historical_timestep

      # Could fill up the history here too?
      # And then remove the reset function - does the same thing as initialise
      # fill_history also needs the mp_params for the hcrip and hcrop, pass into initialise
      # ... why are we filling up HCR IP and HCR OP here?
      self$fill_history(stock_params)

      # If mp_params passed in, also get HCR ip and op in the last historical time step + 1
      # This is so we plot the upcoming decision before it happens
      if (!is.null(mp_params)){
        hcr_ts <- self$last_historical_timestep + 1
        self$hcr_ip[,hcr_ts] <- get_hcr_ip(stock = self, mp_params = mp_params, yr = hcr_ts)
        self$hcr_op[,hcr_ts] <- get_hcr_op(stock = self, mp_params = mp_params, yr = hcr_ts)
      }


      invisible(self)
    },

    #' @description
    #' @param stock_params Named list with last_historical_timestep and stock_history elements.
    #' @param ... Other arguments
    #' Fills up the historical period of the stock
    fill_history = function(...){
      print("Filling the historical period")
      self$biomass[,1] <- self$b0
      self$fill_catch_history(...)

      print("Filling biomass history")
      # Fill up biomass in the historical period - including the biomass at the start of projection period
      for (ts in 1:self$last_historical_timestep){
        self$fill_next_biomass(ts)
      }
      # Other members
      self$effort <- self$catch / (self$biomass * self$q)
      invisible(self)
    },

    # Make this private? So only called by fill historical?
    #' @description
    #' Fill up the historical period of catches with random values to simulate a catch history
    #' @param stock_params A list with essential elements: r (growth rate, numeric), stock_history (string: "fully", "over", "under") initial_year (integer), last_historical_timestep (integer), nyears (integer).
    #' @param stock_history Character string of the exploitation history (default = "fully", alternatives are "under" or "over").
    fill_catch_history = function(stock_params){
      print("Filling catch history")
      if (!(stock_params$stock_history %in% c("fully", "under", "over"))){
        stop("stock_history parameter must be 'fully', 'under' or 'over'.")
      }
      # Base the catch history on the stock history
      catch_history <- switch(stock_params$stock_history,
                  "under" = rep(2*self$msy/3, self$last_historical_timestep),
                  "fully" = rep(self$msy, self$last_historical_timestep),
                  "over" = seq(from=3*self$msy/4, to=4*self$msy/3, length=self$last_historical_timestep)
      )
      niters <- dim(self$catch)[1]
      catch_history_iters <- array(NA,dim=c(niters, self$last_historical_timestep))
      catch_history_iters[] <- rep(catch_history, each=niters)
      # Sling a load of noise on it
      # Set seed so that the initial noise is always the same
      set.seed(666)
      catch_history_iters <- catch_history_iters * rlnorm(prod(dim(catch_history_iters)),meanlog=0,sdlog=0.1)
      # Set a proper random seed
      set.seed(as.numeric(Sys.time()))
      # Or don't include noise in initial period
      self$catch[,1:self$last_historical_timestep] <- catch_history_iters
      invisible(self)
    },

    #' @description
    #' Fills the biomass in the next timestep based on current biomass and catches
    #' The surplus production model has the general form:
    #' \code{Bt+1 = Bt + f(Bt) - Ct}
    #' Where the production function f() is a Pella & Tomlinson model with shape
    #' \code{f(Bt) = r/p Bt * (1 - (Bt/k)^p)}
    #' Here p is fixed at 1 to give a Schaefer model
    #' \code{cpue = Ct / Et = qBt}
    #' @param ts The current time step (time step, not year) - biomass is filled in time step ts+1
    #' @param biol_prod_sigma The biological productivity variability (default = 0).
    fill_next_biomass = function(ts){
      fB <- (self$r / self$p) * self$biomass[,ts] * (1 - (self$biomass[,ts] / self$k) ^ self$p)
      # Apply correlated noise to r
      # fB <- fB * process_variability
      # A value of b = 0.5 is red noise, make redder by increasing (< 1)
      # Currently not an input but could be
      b <- 0.5
      # How do we deal with biol_prod_sigma and other stochasticity parameters
      # Update current_corrnoise
      self$current_corrnoise <- next_corrnoise(self$current_corrnoise, b=b, sd=self$biol_sigma)
      fB <- fB * (self$current_corrnoise + 1)
      # Update biomass
      self$biomass[,ts+1] <- self$biomass[,ts] + fB - self$catch[,ts]
      # Biomass cannot be less than 1e-6
      self$biomass[,ts+1] <- pmax(self$biomass[,ts+1],1e-6)
      invisible(self)
    },

    #' @description
    #' Resets the stock by emptying out array slots, and refilling the historical period.
    #' The existing life history parameters and dimensions are unchanged.
    #' This is so the same stock can be used with a different MP.
    #' @param ... Arguments passed to 'Stock$initialize' and 'Stock$fill_historical'
    reset = function(...){
      # Empty the array members
      self$biomass[] <- NA
      self$hcr_ip[] <- NA
      self$hcr_op[] <- NA
      self$catch[] <- NA
      self$effort[] <- NA
      self$estimated_cpue[] <- NA
      # Then refill up the history
      self$fill_history(...)
      invisible(self)
    },

    as_data_frame = function(...){
      out <- data.frame(
        iter = rep(1:dim(self$biomass)[1], each=dim(self$biomass)[2]),
        biomass = c(t(self$biomass)),
        catch = c(t(self$catch)))
      return(out)
    },

    print = function(...){
      print(self$as_data_frame())
      invisible(self)
    }

    #' @description
    #' Projects the stock over the time steps given and updates the biomass, HCR ip / op and catches
    #' It uses a simple biomass dynamic model where the catches or fishing effort are set every time step by the harvest control rule.
    #'
    #' @param timesteps The timesteps to project over. A vector of length 2 (start and end).
    #' @param mp_params A vector of management procedure parameters.
    #' @return A stock object (a reactiveValues object with bits for the stock)
    project = function(timesteps, mp_params){
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
    #  for (yr in timesteps[1]:timesteps[2]){
    #    # Implementation function stuff
    #    # Test this to death with different yr ranges etc
    #    if (mp_params$output_type == "catch"){
    #      self$catch[,yr] <- self$hcr_op[,yr]
    #      self$effort[,yr] <- self$catch[,yr] / (self$q * self$biomass[,yr])
    #    }
#
    #    # base_effort used by relative effort HCRs
    #    base_effort <- self$effort[,self$last_historical_timestep]
    #    if (mp_params$output_type == "relative effort"){
    #      # HCR OP is relative to last historical effort
    #      self$effort[,yr] <- base_effort * self$hcr_op[,yr]
    #      self$catch[,yr] <- self$effort[,yr] * self$q * self$biomass[,yr]
    #    }
#
    #    # Only used with empirical HCR - not yet added
    #    #if (mp_params$output_type == "catch multiplier"){
    #    #  # Careful - yr may be 1, so yr-1 may be null
    #    #  new_catch <- self$catch[,yr-1] * self$hcr_op[,yr]
    #    #  new_catch[new_catch < 10] <- 10 # A minimum catch - set for safety
    #    #  self$catch[,yr]<- new_catch
    #    #  self$effort[,yr] <- self$catch[,yr] / (self$q * self$biomass[,yr])
    #    #}
#
#
    #    # Sometimes you get crazy high so we limit the maximum effort relative to the last historical year
    #    # After applying effort limit you need to recalculate catch to reflect what really happened
    #    rel_effort <- self$effort[,yr] / base_effort
    #    max_rel_effort <- pmin(rel_effort, 10) # Max relative effort capped at 10
    #    self$effort[,yr] <- max_rel_effort * base_effort
    #    self$catch[,yr] <- self$effort[,yr] * self$q * self$biomass[,yr] #
    #    # Update estimated cpue too
    #    #true_cpue <- stock$catch[,yr] / stock$effort[,yr]
    #    #stock$estimated_cpue[,yr] <- estimation_error(input =  true_cpue, sigma = stock_params$biol_est_sigma, bias = stock_params$biol_est_bias)
#
    #    # If room get the biomass in the next time step
    #    # Bt+1 = Bt + f(Bt) - Ct
    #    # f(Bt) = r/p Bt * (1 - (Bt/k)^p)
    #    if (yr < dim(self$biomass)[2]){
    #      self$fill_next_biomass(ts)
#
#
    #      #self$hcr_ip[,yr+1-mp_params$timelag] <- get_hcr_ip(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=yr+1-mp_params$timelag)
    #      #stock$hcr_op[,yr+1] <- get_hcr_op(stock=stock, stock_params=stock_params, mp_params=mp_params, yr=yr+1-mp_params$timelag)
    #    }
    #  }
    #  return(stock)


    }


  )
)



#-------------------------------------------------------------------




