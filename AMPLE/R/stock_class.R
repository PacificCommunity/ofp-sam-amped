# The Stock class based on R6
# stock_class.R

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: The Heavens by Sedibus & The Orb
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' Correlated random noise
#' 
#' Get the next value of correlated noise.
#' Not exported. For internal use only. Not a class method.
#' vt = b * vt-1 + s * sqrt(1 - b^2)
#' s is normally distributed random variable with mean = 0
#' b is the autocorrelation parameter > -1 and < 1
#' e.g. -0.8 very blue, 0.8 red
#' @param x Current value
#' @param b Correlation factor: -1 (red) to 1 (blue), 0 (white)
#' @param sd Standard deviation
#' @references Ranta and Kaitala 2001 Proc. R. Soc.
#' @importFrom stats "quantile" "reshape" "rlnorm" "rnorm"
#' @noRd
#' @keywords internal
next_corrnoise <- function(x, b, sd=0.1){
  # Each iter needs a separate noise (i.e. not correlated across iters)
  s <- rnorm(length(x),mean=0,sd=sd)
  nextx <- b * x + s * sqrt(1 - b^2)
  return(nextx)
}

# Note use of spurious importFrom to remove spurious note in R CMD check --as-cran

#' R6 Class representing a stock
#' 
#' @description
#' A stock object has life history parameters, fields and methods for a biomass dynamic model.
#'
#' @details
#' A stock has biomass, effort, catch and hcr_ip and hcr_op fields as well as the life history parameters.
#' The population dynamics are a simple biomass dynamic model.
#' The Stock class is used for the Shiny apps in the AMPLE package.
#' @importFrom R6 "R6Class"
Stock <- R6::R6Class("Stock",
  # R6 classes are not reactive, so even in you make a reactiveVal() of an instance of an R6 class
  # changing that object will not invalidate the shiny magic and nothing reacts.
  # Solution 1 is to use S3 class but they are horrible
  # Solution 2 is this workaround from Winston Change - remember to thank him:  
  # https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890/8 
  # I will try this here. It involves adding an 'invalidate' function which triggers if certain methods are
  # called. I want this class only to invalidate when the project method is called, because that means that
  # something happened and the plots should update.
  
  # Notes on using the reactive stock
  # stock is a reactiveExpr. Evaluating stock() calls the Stock$reactive() method.
  # This method accesses a reactiveVal (called reactiveDep) inside Stock and then returns self.
  # This effectively makes stock a reactive version of a Stock as each call to it accesses reactiveDep.
  # When reactiveDep changes, the stock object invalidates.
  # Changing reativeDep is added to the methods you want to invalidate the stock.
  # Here we add it to project() and reset(). When called, the last thing they do is change the
  # value of reactiveDep using the invalidate() method.
  # stock() therefore becomes invalid and triggers things in the Shiny app.
  
  
                     
  # Keeps track of whether object has been invalidated.
  private = list(
    reactiveDep = NULL,
    reactiveExpr = NULL,
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    count = 0
  ), 
                     
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
    # #' @field estimated_cpue Array of estimated CPUE
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
    #' @field trp Target reference point, expressed as depletion (default = 0.5).
    trp = 0.5,
    #' @field b0 Virgin biomass (default = NULL - set by msy and r when object is initialised).
    b0 = NULL,
    #' @field current_corrnoise Stores the current values of the correlated noise (by iteration).
    current_corrnoise = NULL,
    #' @field biol_sigma Standard deviation of biological variability (default = 0).
    biol_sigma = NULL,
    #' @field last_historical_timestep The last historical timestep of catch and effort data.
    last_historical_timestep = NULL,
    
    #' @description
    #' Create a new stock object, with fields of the right dimension and NA values (by calling the \code{reset()} method.
    #' See the \code{reset()} method for more details.
    #' @param stock_params A list of stock parameters with essential elements: r (growth rate, numeric), stock_history (string: "fully", "over", "under") initial_year (integer), last_historical_timestep (integer), nyears (integer), biol_sigma (numeric).
    #' @param mp_params A list of the MP parameters. Used to fill HCR ip and op.
    #' @param niters The number of iters in the stock (default = 1).
    #' @return A new Stock object.
    initialize = function(stock_params, mp_params, niters = 1){
      #print("Initialising stock with NAs")
      # Set up the reactive dependency - has to be done in the main constructor.
      private$reactiveDep <- function(x) NULL 
      # Make the fields and fill up the history
      self$reset(stock_params = stock_params, mp_params = mp_params, niters = niters)
    },
    
    #' @description
    #' Resets an existing stock object, by remaking all fields (possibly with different dimensions for the array fields) .
    #' Fills up the catch, effort and biomass fields in the historical period based on the stock history and
    #' life history parameters in the \code{stock_params} argument.
    #' This is a reactive method which invalidates a reactive instance of this class after it is called.
    #' @param stock_params A list with essential elements: r (growth rate, numeric, default=6), stock_history (string: "fully", "over", "under", default="fully") initial_year (integer, default=2000), last_historical_timestep (integer, default=10), nyears (integer, default=30), biol_sigma (numeric, default = 0).
    #' @param mp_params A list of the MP parameters. Used to fill HCR ip and op.
    #' @param niters The number of iters in the stock (default = 1).
    #' @return A new Stock object.
    reset = function(stock_params, mp_params, niters){
      #print("Resetting existing stock")
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
      # Fill up the historical period
      self$fill_history(stock_params = stock_params, mp_params = mp_params)
      
      # Invalidate the object so Shiny gets triggered
      private$invalidate() 
      invisible(self)
    },

    # Add option to initialiser to call this
    #' @description
    #' Method to create a reactive instance of a Stock.
    #' @return a reactiveExpr.
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    }, 

    #' @description
    #' Fills the historical period of the stock
    #' @param stock_params Named list with last_historical_timestep and stock_history elements.
    #' @param mp_params A list of the MP parameters. Used to fill HCR ip and op.
    fill_history = function(stock_params, mp_params){
      #print("Filling the historical period")
      self$biomass[,1] <- self$b0
      self$fill_catch_history(stock_params)
      #print("Filling biomass history")
      # Fill up biomass in the historical period - including the biomass at the start of projection period
      # Also get the historical HCR ip and op over the time series
      # This is so we plot the upcoming decision before it happens, and also show observed vs true stock status
      for (ts in 2:(self$last_historical_timestep + 1)){
        self$fill_biomass(ts)
        self$hcr_ip[,ts] <- get_hcr_ip(stock = self, mp_params = mp_params, yr = ts)
        self$hcr_op[,ts] <- get_hcr_op(stock = self, mp_params = mp_params, yr = ts)
      }
      # HCR ip and op also need the first time step
      self$hcr_ip[,1] <- get_hcr_ip(stock = self, mp_params = mp_params, yr = 1)
      self$hcr_op[,1] <- get_hcr_op(stock = self, mp_params = mp_params, yr = 1)
      
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
      #print("Filling catch history")
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
      # And that the catch history for each iteration is the same
      set.seed(666)
      catch_history_iters <- catch_history_iters * rep(rlnorm(dim(catch_history_iters)[2],meanlog=0,sdlog=0.1), each=niters)
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
    #' @param ts The biomass time step to be filled (required catch etc in ts - 1).
    #' @param iters The iterations to calculate the biomass for (optional - default is all of them).
    fill_biomass = function(ts, iters = 1:dim(self$biomass)[1]){
      # Check that ts > 1
      if(ts < 2){
        stop("Cannot get biomass in ts = 1 (as you need an initial biomass)")
      }
      # Get fB in previous timestep
      fB <- (self$r / self$p) * self$biomass[iters,ts-1] * (1 - (self$biomass[iters,ts-1] / self$k) ^ self$p)
      # Apply correlated noise to r if not in the historical period
      # fB <- fB * process_variability
      # A value of b = 0.5 is red noise, make redder by increasing (< 1)
      # Currently not an input but could be
      if (ts > self$last_historical_timestep){
        b <- 0.5
        # Update current_corrnoise
        self$current_corrnoise[iters] <- next_corrnoise(self$current_corrnoise[iters], b=b, sd=self$biol_sigma)
        fB <- fB * (self$current_corrnoise[iters] + 1)
      }
      # Update biomass
      self$biomass[iters,ts] <- self$biomass[iters,ts-1] + fB - self$catch[iters,ts-1]
      # Biomass cannot be less than 1e-6
      self$biomass[iters,ts] <- pmax(self$biomass[iters,ts],1e-6)
      invisible(self)
    },

    #' @description
    #' Produces a data.frame of some of the array-based fields, like biomass.
    #' Just used for testing purposes.
    as_data_frame = function(){
      out <- data.frame(
        iter = rep(1:dim(self$biomass)[1], each=dim(self$biomass)[2]),
        year = rep(1:dim(self$biomass)[2], times=dim(self$biomass)[1]),
        biomass = c(t(self$biomass)),
        catch = c(t(self$catch)),
        effort = c(t(self$effort)),
        hcr_ip = c(t(self$hcr_ip)),
        hcr_op = c(t(self$hcr_op))
        )
      return(out)
    },

    #' @description
    #' Projects the stock over the time steps given and updates the biomass, HCR ip / op and catches
    #' It uses a simple biomass dynamic model where the catches or fishing effort are set every time step by the harvest control rule.
    #'
    #' @param timesteps The timesteps to project over. A vector of length 2 (start and end).
    #' @param mp_params A vector of management procedure parameters.
    #' @param iters A vector of iterations to be projected. Default is all the iterations in the stock
    #' @return A stock object (a reactiveValues object with bits for the stock)
    project = function(timesteps, mp_params, iters=1:dim(self$biomass)[1]){
      # Check timesteps - should be a range of two values
      if (length(timesteps) == 1){
        timesteps <- rep(timesteps,2)
      }
      if (length(timesteps) != 2){
        stop("In project(). timesteps argument should be of length 2.")
      }
      if((timesteps[1] - mp_params$timelag) < 1){
        stop("In project(). Trying to access element in yr less than 1.")
      }
      # Check iters
      if(!all(iters %in% 1:dim(self$biomass)[1])){
        stop("In project(). Iterations outside of range.")
      }

      # Loop over the timesteps and update catch and biomass
      # yr is the year we get catch for
      # yr + 1 is the  year we get true biomass and HCR op for
      for (yr in timesteps[1]:timesteps[2]){
        # Setting up future catch and effort depends on the output of the HCR
        # Could be catch, relative effort or some empirical based one
        # The HCR OP will have already been set up (either in initialisation or end of this loop)
        # This gets converted into the catch and effort that will be realised
        # Note the relationship between: effort, catch, q, and biomass
        
        # Catch based HCRs - like the typical catch threshold HCR
        if (mp_params$output_type == "catch"){
          self$catch[iters,yr] <- self$hcr_op[iters,yr]
          self$effort[iters,yr] <- self$catch[iters,yr] / (self$q * self$biomass[iters,yr])
        }

        # base_effort used by relative effort HCRs
        base_effort <- self$effort[iters,self$last_historical_timestep]
        if (mp_params$output_type == "relative effort"){
          # HCR OP is relative to last historical effort
          self$effort[iters,yr] <- base_effort * self$hcr_op[iters,yr]
          self$catch[iters,yr] <- self$effort[iters,yr] * self$q * self$biomass[iters,yr]
        }

        # Only used with empirical HCR - not yet added
        #if (mp_params$output_type == "catch multiplier"){
        #  # Careful - yr may be 1, so yr-1 may be null
        #  new_catch <- self$catch[,yr-1] * self$hcr_op[,yr]
        #  new_catch[new_catch < 10] <- 10 # A minimum catch - set for safety
        #  self$catch[,yr]<- new_catch
        #  self$effort[,yr] <- self$catch[,yr] / (self$q * self$biomass[,yr])
        #}

        # Sometimes you get crazy high efforts (when biomass is low)
        # So we limit the maximum effort relative to the last historical year
        # After applying effort limit you need to recalculate catch to reflect what really happened
        rel_effort <- self$effort[iters,yr] / base_effort
        max_rel_effort <- pmin(rel_effort, 10) # Max relative effort capped at 10 - could be lower
        self$effort[iters,yr] <- max_rel_effort * base_effort
        # Update catch based on the updated effort
        self$catch[iters,yr] <- self$effort[iters,yr] * self$q * self$biomass[iters,yr] #
        
        # What was estimated CPUE used for? Empirical HCR?
        # Update estimated cpue too
        #true_cpue <- stock$catch[,yr] / stock$effort[,yr]
        #stock$estimated_cpue[,yr] <- estimation_error(input =  true_cpue, sigma = stock_params$biol_est_sigma, bias = stock_params$biol_est_bias)
#
        # If room get the biomass and evaluate the HCR in the next time step, yr+1
        # Bt+1 = Bt + f(Bt) - Ct
        # f(Bt) = r/p Bt * (1 - (Bt/k)^p)
        if (yr < dim(self$biomass)[2]){
          # Update biomass
          self$fill_biomass(ts = yr+1, iters=iters)
          # Evaluate HCR in the next (yr argument to get_hcr_ip and get_hcr_op is current or next?)
          self$hcr_ip[iters,yr+1] <- get_hcr_ip(stock = self, mp_params = mp_params, yr = yr+1, iters=iters)
          self$hcr_op[iters,yr+1] <- get_hcr_op(stock = self, mp_params = mp_params, yr = yr+1, iters=iters)
        }
      } # End of main project timestep for loop
      
      # Invalidate the object so Shiny gets triggered
      private$invalidate() 
      
      invisible(self)
    }, # End of project()

    #' @description
    #' The catch per unit effort (CPUE, or catch rate) relative to the CPUE in the last historical period.
    #' @return An array of same dims as the catch and effort fields.
    relative_cpue = function(){
      cpue <- self$catch / self$effort
      base_cpue<- cpue[,self$last_historical_timestep]
      rel_cpue <- sweep(cpue, 1, base_cpue, "/")
      return(rel_cpue)
    },

    #' @description
    #' The effort relative to the effort in the last historical period.
    #' @return An array of same dims as the effort field.
    relative_effort = function(){
      base_effort <- self$effort[,self$last_historical_timestep]
      rel_effort <- sweep(self$effort, 1, base_effort, "/")
      return(rel_effort)
    },

    #' @description
    #' Summarises the final year of each iteration. Only used for the Measuring Performance app.
    #' @param iters The iterations to calculate the table values for (default is iteration 1).
    #' @param quantiles Numeric vector of the quantile range. Default values are 0.05 and 0.95.
    replicate_table = function(iters = 1, quantiles = c(0.05, 0.95)){
      # How to deal with iters being empty
      final_ts <- dim(self$biomass)[2]
      sbsbf0 <- self$biomass[iters,final_ts] / self$k
      catch <- self$catch[iters,final_ts]
      rel_cpue <- self$relative_cpue()[iters, final_ts]
      # Get median and percentiles
      signif <- 2 # Could make option but no point
      sbsbf0_qs <- signif(quantile(sbsbf0, probs=c(quantiles[1], 0.5, quantiles[2])),signif)
      sbsbf0_summary <- paste(sbsbf0_qs[2], " (", sbsbf0_qs[1], ",", sbsbf0_qs[3], ")", sep="")
      catch_qs <- signif(quantile(catch, probs=c(quantiles[1], 0.5, quantiles[2])), signif)
      catch_summary <- paste(catch_qs[2], " (", catch_qs[1], ",", catch_qs[3], ")", sep="")
      rel_cpue_qs <- signif(quantile(rel_cpue, probs=c(quantiles[1], 0.5, quantiles[2])), signif)
      rel_cpue_summary <- paste(rel_cpue_qs[2], " (", rel_cpue_qs[1], ",", rel_cpue_qs[3], ")", sep="")
      # Put all together - 
      # Reorder and sort so that average and range are top, and iterations are reversed
      out <- data.frame(Replicate= rev(c(1:length(sbsbf0),"Average and range")),
                        sbsbf0=rev(c(signif(sbsbf0, signif), sbsbf0_summary)),
                        Catch=rev(c(signif(catch, signif), catch_summary)),
                        rel_cpue=rev(c(signif(rel_cpue, signif), rel_cpue_summary)))
      colnames(out)[2] <- "Final biomass"
      colnames(out)[3] <- "Final catch"
      colnames(out)[4] <- "Final relative CPUE"
      return(out)
    }, 

    #' @description 
    #' Calculates the short, medium and long term periods to calculate the performance indicators over,
    #' based on the last historic year of data and the number of years in the projection.
    time_periods = function(){
      nprojyears <- dim(self$catch)[2] - self$last_historical_timestep
      period_length <- round(nprojyears / 3)
      short_term <- (self$last_historical_timestep + 1):(self$last_historical_timestep + period_length)
      medium_term <- short_term + period_length
      long_term <- (max(medium_term) + 1):dim(self$catch)[2]
      all_years <- dimnames(self$catch)$year
      short_term <- all_years[short_term]
      medium_term <- all_years[medium_term]
      long_term <- all_years[long_term]
      short_name <- paste("Short (", short_term[1], "-", short_term[length(short_term)],")", sep="")
      medium_name <- paste("Medium (", medium_term[1], "-", medium_term[length(medium_term)],")", sep="")
      long_name <- paste("Long (", long_term[1], "-", long_term[length(long_term)],")", sep="")
      out <- list(short = short_term, medium = medium_term, long = long_term)
      names(out) <- c(short_name, medium_name, long_name)
      return(out)
    },

    #' @description 
    #' Gets the performance indicators across all indicators, for three time periods.
    #' Used in the Measuring Performance and Comparing Performance apps.
    #' @param iters The iterations to calculate the table values for (default is all of them).
    #' @param quantiles Numeric vector of the quantile range. Default values are 0.05 and 0.95.
    #' @return A data.frame
    performance_indicators = function(iters = 1:dim(self$biomass)[1], quantiles=c(0.05, 0.95)){
      niters <- length(iters)
      sbsbf0 <- self$biomass[iters,,drop=FALSE] / self$k
      catch <- self$catch[iters,,drop=FALSE]
      rel_cpue <- self$relative_cpue()[iters,,drop=FALSE]
      rel_effort <- self$relative_effort()[iters,,drop=FALSE]
      # Add median to quantile range
      if (!(0.5 %in% quantiles)){
        quantiles <- c(quantiles, 0.5)
      }
      # Reorder so pi_table knows() what order the quantiles are coming in
      quantiles <- sort(quantiles)
      
      # Probability above LRP
      # Apply drops the dimensions which is annoying
      prob_lrp <- apply(sbsbf0 > self$lrp, 2, sum, na.rm=TRUE) / niters
      prob_lrp <- test <- array(prob_lrp, dim=c(1, length(prob_lrp)), dimnames=list(iter = 1, year=names(prob_lrp)))
      
      # Catch stability
      catch_diff <- abs(catch[,2:ncol(catch), drop=FALSE] - catch[,1:(ncol(catch)-1), drop=FALSE])
      max_catch_diff <- self$k / 10 # For rescale - Has to be same for all stocks - arbitrary
      catch_stab <- (max_catch_diff - catch_diff) / max_catch_diff # rescale
      catch_stab[catch_stab < 0] <- 0
      
      # Proximity to TRP
      max_distance_from_trp <- max(self$trp, 1 - self$trp)
      prox_trp <- pmax(1.0 - (abs(sbsbf0 - self$trp) / max_distance_from_trp), 0.0)
      
      # Get the averages over the time periods
      # This all gets a bit gnarly
      time_periods <- self$time_periods()
      #data <- list(sbsbf0 = sbsbf0, prob_lrp = prob_lrp, catch = catch, rel_cpue = rel_cpue, rel_effort = rel_effort, catch_stab = catch_stab, prox_trp = prox_trp)
      # Give them proper names
      data <- list("Biomass" = sbsbf0, "Prob. > LRP" = prob_lrp, "Catch" = catch, "Relative CPUE" = rel_cpue, "Relative effort" = rel_effort, "Catch stability"= catch_stab, "Proximity to TRP" = prox_trp)
      
      t2 <- lapply(data, function(x) {
        lapply(time_periods, function(y) {
          iter_means <- apply(x[,y,drop=FALSE], 1, mean, na.rm=TRUE)
          quants <- quantile(iter_means, probs = quantiles, na.rm=TRUE)
        })
      })
      
      # Force the order of the time period and metrics
      time_period_names <- factor(names(time_periods), levels=names(time_periods))
      data_names <- factor(names(data), levels=names(data))
      
      out <- data.frame(pi = rep(data_names, each = length(time_periods) * length(quantiles)),
               time_period = rep(rep(time_period_names, length(data)), each=length(quantiles)),
               quantiles = rep(quantiles,length(time_periods) * length(data)),
               value = unlist(t2), row.names = NULL)
      return(out)
    },

    #' @description
    #' Makes a table of the performance indicators.
    #' @param quantiles Numeric vector, length 2, of the low and high quantiles.
    #' @param iters The iterations to calculate the table values for (default is all of them).
    pi_table = function(iters = 1:dim(self$biomass)[1], quantiles = c(0.05, 0.95)){
      if(length(quantiles) != 2){
        stop("In Stock$pi_table(). quantiles must be length 2")
      }
      pis <- self$performance_indicators(iters=iters, quantiles=quantiles) 
      # Order so that 
      out <- tapply(signif(pis$value,2), INDEX = list(pis$pi, pis$time_period), FUN = function(x) paste0(x[2], " (",x[1], ",", x[3], ")"))
      out <- as.data.frame(out)
      return(out)
    }

  ) # End of public fields
)






