# Plotting functions

# Plot stuff


# The connecting arrow
#' Plot biomass to HCR arrow
#' 
#' Plots the connecting arrow between the biomass plot and the HCR plot in the 'Introduction to HCRs' app.
#' @param stock An R6 class Stock object.
#' @param timestep The time step of the HCR input.
plot_hcr_intro_arrow <- function(stock, timestep){
  # Arrow from B/K to HCR
  btimestep <- min(timestep, dim(stock$hcr_ip)[2])
  last_hcr_ip  <- stock$hcr_ip[1, btimestep]
  npoints <- 100
  theta <- seq(from=0, to=pi/2, length=npoints)
  x <- sin(theta) * last_hcr_ip
  y <- -1 * cos(theta) * (1-last_hcr_ip)
  # Set up plot
  plot(x=x, y=y, xlim=c(0,1), ylim=c(-1,0), type="n", xaxt="n", yaxt="n",xlab="", ylab="", axes=FALSE,xaxs="i", yaxs="i")
  lines(x=x, y=y, col="blue", lwd=3)
  # Add an arrow
  arrows(x0=x[length(x)-1], y0=y[length(y)-1], x1=x[length(x)], y1=y[length(y)],col="blue", lwd=3)
}

#' plot_biomass
#'
#' plot_biomass() plots time series of 'true' and observed depletion (SB/SBF=0).
#'
#' @param stock An R6 class Stock object.
#' @param mp_params The management procedure parameters (a list including mp_type).
#' @param max_spaghetti_iters The number of iterations to show as spaghetti before ribbons are shown.
#' @param quantiles Quantiles of the ribbons.
#' @param nspaghetti The number of spaghetti iterations to plot on top of the ribbons.
#' @param add_grid Add a grid.
#' @param xlab The x-label.
#' @param ghost_col Colours of the ghost iterations.
#' @param last_col Colours of the last iteration.
#' @param ylim Y limits
#' @param ... Other arguments to pass to the plot() function.
#' @return A plot
plot_biomass <- function(stock, mp_params, show_last=TRUE, max_spaghetti_iters=50, quantiles=NA, nspaghetti=5, ghost_col="grey", last_col="blue", ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # True bk
  bk_true <- stock$biomass / stock$k
  # Set up empty plot 
  ylim <- c(0,1) #max(1, max(bk_true, na.rm=TRUE), na.rm=TRUE)
  plot(x=years, y= bk_true[1,], type="n", ylab="SB/SBF=0", xlab="Year", ylim=ylim, xaxs="i", yaxs="i", ...)
  # Add LRP and TRP
  lines(x=c(years[1], years[length(years)]),y=rep(stock$lrp, 2), lty=3, lwd=2, col="black")
  lines(x=c(years[1], years[length(years)]),y=rep(stock$trp, 2), lty=3, lwd=2, col="black")
  # Add a grid
  grid()
  
  # Need to figure out the iteration stuff for the 'Introduction to indicators' app
  # Get last iteration - what if lots of iters set up, but not all filled?
  last_iter <- dim(bk_true)[1]
  ## If we have more than X iters, draw envelope of iters
  #if(last_iter > max_spaghetti_iters){
  #  # Draw ribbon
  #  # Send the 2nd and 4th quantile
  #  draw_ribbon(x=years, y=bk_true, quantiles=quantiles)
  #  # Add spaghetti
  #  for (iter in 1:nspaghetti){
  #    lines(x=years, y=bk_true[iter,], lty=spaghetti_lty, lwd=spaghetti_lwd, col=spaghetti_col)
  #  }
  #}  else {
  # If we are not drawing the envelope, show all iters as ghosts
  #  if (last_iter > 1){
  #    for (i in 1:(last_iter - 1)){
  #      lines(x=years, y=bk_true[i,], col=ghost_col, lty=ghost_lty, lwd=ghost_lwd)
  #    }
  #  }
  #}
  
  # Show the last iter as an illustration
  true_col <- "black"
  obs_col <- "blue"
  if(show_last){
    # This will only work for model based for now
    # If we have a model based MP, show the HCR IP as it is the estimated biomass too
    #if (mp_params$mp_type == "model"){
    
    # With no estimation error, the true biomass is the same as the observed biomass
    lines(x=years, y=stock$hcr_ip[last_iter,], col=obs_col, lty=1, lwd=2)
    # Add a legend if true and observed are different
    if ((mp_params$est_sigma != 0) | (mp_params$est_bias != 0)){
      # Plot the true B/K  - plotted first so that the Intro to HCR shows it
      lines(x=years, y=bk_true[last_iter,], col="black", lwd=2, lty=1)
      legend(x="bottomleft", legend=c("True","Estimated"), lwd=2, col=c(true_col, obs_col))
    }
  }
}


