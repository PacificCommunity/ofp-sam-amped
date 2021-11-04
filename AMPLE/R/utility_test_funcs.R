# Helpful functions for testing
# Not exported

#' Make dummy stock params
#' 
#' Make dummy stock params to help with testing
#' @noRd
#' @keywords internal
make_dummy_stock_params <- function(r = 0.5, stock_history="fully", nyears = 30, initial_year = 2010, last_historical_timestep = 10, biol_sigma=0){
  out <- list(
    r = r,
    stock_history = stock_history,
    nyears = nyears,
    initial_year = initial_year,
    last_historical_timestep = last_historical_timestep,
    biol_sigma = biol_sigma
  )
  return(out)
}

#' Make dummy mp params
#' 
#' Make dummy mp params to help with testing
#' @noRd
#' @keywords internal
make_dummy_mp_params <- function(hcr_shape = "threshold", mp_analysis = "assessment", mp_type="model", output_type="catch", name = "Dummy", params = c(lim = 0.2, elbow = 0.5, min = 10, max = 140), est_bias = 0.0, est_sigma = 0.0, timelag = 0){
  out <- list(hcr_shape = hcr_shape,
              mp_analysis = mp_analysis,
              mp_type = mp_type,
              output_type = output_type,
              name = name,
              params = params,  
              est_bias = est_bias,
              est_sigma = est_sigma,
              timelag = timelag)
  return(out)
}
