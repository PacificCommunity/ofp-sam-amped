# Functions for handling the HCR

# yr is the timestep that use to generate the HCR IP in the current year
# e.g. look at biomass / k in year yr

# DO WE NEED STOCK PARAMS HERE?

# What do the mp_analysis functions look like?
# "empirical_cpue_slope_ip" or "assessment"
# Only "assessment" implemented here. Empirical still to come.
# What parameters do these need?

# MP analysis functions
# For empirical HCR
# Analysis functions:
# Called by get_hcr_ip()


#' Get the input to the HCR
#'
#' Run the MP analyses function to generate the input to the HCR i.e. observed stock status.
#' For example, estimated biomass from an assessment.
#' @param stock The stock object
#' @param mp_params The HCR / management procedure parameters used to evaluate the HCR (as a list).
#' @param yr The time step of the true stock status used to generate the HCR IP .
#' @param ... Other arguments, including iters
#' @import shiny
#' @export
get_hcr_ip <- function(stock, mp_params, yr, ...){
  # Check for NA in mp_analysis, if so return NA
  if (is.na(mp_params$mp_analysis)){
    hcr_ip <- NA
  }
  # If not NA call the analysis function
  else {
    # Call HCR with correct inputs:
    # What are stock_params here?
    hcr_ip <- do.call(mp_params$mp_analysis, args=list(stock=stock, mp_params=mp_params, yr=yr, ...))
  }
  return(hcr_ip)
}

# This needs testing!


# Analysis function - called by get_hcr_ip() to fill the hcr_ip slot
# This is then used by the get_hcr_op() function to fill the hcr_op slot
# That is then used by project() to update catch and effort in the next time step

# asessment: a stock assessment that estimates biomass / k
# The analysis assessment function
# Called by get_hcr_ip()

#' assessment
#'
#' Function used by \code{get_hcr_ip()} to generate input data for an assessment based HCR.
#' The input to the HCR is depletion (i.e. Biomass / K).
#' @param stock The stock object
#' @param mp_params A named list of MP parameters (with est_sigma and est_bias elements)
#' @param yr The timestep that the biomass is taken from.
#' @param iters Numeric vector of iters. Default is all of them.
#' @export
assessment <- function(stock, mp_params, yr, iters = 1:dim(stock$biomass)[1]){
  # Return observed depletion
  true_ip <- stock$biomass[iters,yr] / stock$k
  est_ip <- estimation_error(input =  true_ip, sigma = mp_params$est_sigma, bias = mp_params$est_bias)
  # Max depletion is 1.0
  est_ip <- pmin(est_ip, 1.0)
  return(est_ip)
}


#' estimation_error
#'
#' Estimation error applied to the 'true' stock status to generate an 'observed' stock status used in the HCR.
#' The error is a combination of bias and lognormally distributed noise.
#' @param input A vector of the 'true' stock status
#' @param sigma Observation error standard deviation
#' @param bias Observation error bias
#' @export
estimation_error <- function(input, sigma, bias){
  # Transform sigma - it means we can set the parameter as a similar scale to the biol var sigma
  sigma <- sigma / 5
  est_variability <- rlnorm(length(input),meanlog=0,sdlog=sigma)
  output <- input * est_variability * (1 + bias)
  return(output)
}





#' Evaluates the harvest control rule.
#'
#' Evaluates the harvest control rule in a single year (timestep).
#' @param stock The stock object
#' @param mp_params The HCR / management procedure parameters used to evaluate the HCR (as a list).
#' @param yr The timestep.
#' @param iters A numeric vector of iters.
#' @return A vector of outputs from the HCR.
#' @export
get_hcr_op <- function(stock, mp_params, yr, iters=1:dim(stock$biomass)[1]){
  # Shape is not NA
  # Call HCR with the lagged input
  #hcr_op <- do.call(mp_params$hcr_shape, args=list(stock=stock, mp_params=mp_params, stock_params=stock_params, yr=yr))
  hcr_op <- do.call(mp_params$hcr_shape, args=list(input=stock$hcr_ip[iters,yr,drop=FALSE], mp_params=mp_params, yr=yr))
  return(hcr_op)
}


# HCR shape functions
# Called by get_hcr_op()

#' Evaluates a threshold harvest control rule
#' 
#' Evaluates a threshold (i.e. hockey stick) harvest control rule.
#' Used by the \code{hcr_op} function.
#' @param input A vector of the 'true' stock status
#' @param mp_params The HCR / management procedure parameters used to evaluate the HCR (as a list).
#' @param ... Unused
#' @return A vector of the same dimension as the input.
#' @export
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


#' Evaluates a constant harvest control rule
#' 
#' Evaluates a constant harvest control rule, i.e. one that ignores the stock status and just returns the constant level (catch or effort).
#' Used by the \code{hcr_op} function.
#' @param mp_params The HCR / management procedure parameters used to evaluate the HCR (as a list).
#' @param ... Unused
#' @export
constant <- function(mp_params, ...){
  return(mp_params$params["constant_level"])
}



