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
  plot(x=years, y= bk_true[1,], type="n", ylab="SB/SBF=0", xlab="Year", ylim=ylim, xaxs="i", yaxs="i", main="SB / SBF=0", ...)
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

# Called by a couple of plotting functions
# Better than maintaining same code in multiple places
# So the HCR and catch plot in the Intro to HCR app have the same scale
get_catch_ymax <- function(catch, mp_params){
  # Sort out dimensions and labels
  # Bit annoying that we have a switch
  ymax <- switch(mp_params$hcr_shape,
                 constant = mp_params$params["constant_level"],
                 threshold = mp_params$params["max"])
  ymax <- max(c(ymax, c(catch)), na.rm=TRUE) * 1.1
  return(ymax)
}



plot_catch <- function(stock, mp_params, timestep, plot_ghost_hcr_ops = TRUE, ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # Set Ylim - use same as HCR plot
  ymax <- get_catch_ymax(stock$catch, mp_params)
  yrange <- c(0, ymax)
  # Empty axis
  plot(x=years, y=years, type="n", ylim=c(0, ymax), ylab="Catch", xlab="Year", main="Catch", xaxs="i", yaxs="i", ...)
  grid()
  
  # Get last iteration 
  last_iter <- dim(stock$catch)[1]
  
  # If we have more than X iters, draw envelope of iters
  #if(last_iter > max_spaghetti_iters){
  #  # Draw ribbon
  #  draw_ribbon(x=years, y=stock$catch, quantiles=quantiles)
  #  # Add spaghetti
  #  for (iter in 1:nspaghetti){
  #    lines(x=years, y=stock$catch[iter,], lty=spaghetti_lty, lwd=spaghetti_lwd, col=spaghetti_col)
  #  }
  #} else {
  
  # Plot the historical HCR OPs (current timestep is considered as historical as that has already been fished)
  if (plot_ghost_hcr_ops == TRUE){
    if (timestep > stock$last_historical_timestep){
      for (yr in (stock$last_historical_timestep+1):timestep){
        lines(x=c(years[1], years[length(years)]), y=rep(stock$catch[last_iter, yr],2), lty=2, col="grey", lwd=2)
      }
    }
  }
  
  # If HCR OP is total catch, plot the proposed catch from the HCR - i.e. the catch in the next time step
  # This is only used in the 'Introduction to HCRs' app.
  # Could add something similar for relative effort based HCRs but I don't they should be included in the
  # 'Introduction to HCRs' app,
  # Would need an additional step of converting that relative effort to actual catch (done in the project() method)
  if(mp_params$output_type == "catch"){
    if (timestep < dim(stock$hcr_op)[2]){ 
      next_catch <- stock$hcr_op[last_iter, timestep+1]
      # If HCR OP is catch multiplier - not implemented yet - used for empirical
      # if (mp_params$output_type == "catch multiplier"){
      #   next_catch <- stock$catch[last_iter,timestep] * stock$hcr_op[,timestep+1]
      #   next_catch[next_catch < 10] <- 10 # A minimum catch
      # }
      lines(x=c(years[1], years[length(years)]), y=rep(next_catch,2), lty=2, col="blue", lwd=2) 
    }
  }
  #  }
  #  # Plot all iters as ghosts
  #  if (last_iter > 1){
  #    for (i in 1:last_iter){
  #      lines(x=years, y=stock$catch[i,], col=ghost_col, lwd=ghost_lwd, lty=ghost_lty)
  #    }
  #  }
  #}
  
  # Current iteration
  lines(x=years, y=stock$catch[last_iter,], col="blue", lwd=2, lty=1)
}

#' Plot the HCR
#' 
#' Plot the HCR, including current stock status and current HCR OP.
plot_model_based_hcr <- function(stock, mp_params, timestep=NULL, show_ref_pts=FALSE, ...){
  
  if (mp_params$output_type=="catch"){
    ymax <- get_catch_ymax(stock$catch, mp_params)
  }
  if (mp_params$output_type=="relative effort"){
    rel_effort <- sweep(stock$effort, 1, stock$effort[,stock$last_historical_timestep], "/")
    ymax <- max(c(rel_effort, mp_params$params["max"], mp_params$params["constant_level"]), na.rm=TRUE) * 1.1
  }
  ylab <- paste("Next ", mp_params$output_type, sep="")
  yrange <- c(0, ymax)
  
  # Need to set these depending on the HCR somehow
  # From MP analysis type? Leave as B/K for now. Could set additional argument in mp_params like xlab?
  xrange <- c(0, 1)
  xlab <- "Estimated SB/SBF=0"
  # Plot empty axes 
  plot(x=xrange,y=yrange,type="n",xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", main="The HCR", ...) 
  grid()
  
  # If model based MP (or NA) add the B/K based reference points
  if (show_ref_pts){
    if (mp_params$mp_type %in% c(as.character(NA), "model")){
      lines(x=rep(stock$lrp,2), y=c(0, ymax),lty=3, lwd=2, col="black")
      lines(x=rep(stock$trp,2), y=c(0, ymax),lty=3, lwd=2, col="black")
    }
  }
  
  # Add all HCR inputs and outputs so far
  # Only show the HCR years (from last historical timestep)
  # The years that the HCR has been active
  hcr_ip_yrs <- ((stock$last_historical_timestep+1):dim(stock$hcr_ip)[2]) - mp_params$timelag
  hcr_op_yrs <- (stock$last_historical_timestep+1):dim(stock$hcr_ip)[2]
  # Show all iters and timesteps as ghosts
  points(x=c(stock$hcr_ip[,hcr_ip_yrs]), y=c(stock$hcr_op[,hcr_op_yrs]), col="grey", pch=16, cex=2)
  
  # Plot HCR outline - depends on the HCR shape
  # Constant catch
  hcr_lwd <- 3
  if (mp_params$hcr_shape == "constant"){
    lines(x=xrange, y=c(mp_params$params["constant_level"], mp_params$params["constant_level"]), lwd=hcr_lwd, lty=1, col="red")
  }
  # Threshold
  else if (mp_params$hcr_shape == "threshold"){
    lines(x = c(0, mp_params$params["lim"], mp_params$params["elbow"], 1),
          y = c(rep(mp_params$params["min"], 2), rep(mp_params$params["max"], 2)), lwd=hcr_lwd, lty=1, col="red")
  }
  else {
    stop("In plot_model_based_hcr(). Trying to plot the HCR shape but hcr_shape is not recognised.")
  }
  
  # Show the IP and OP for the current iter and timestep
  if (!is.null(timestep)){
    # Stop timestep going beyond bounds
    if (timestep > dim(stock$hcr_ip)[2]){
      timestep <- dim(stock$hcr_ip)[2]
    }
    last_iter <- dim(stock$hcr_ip)[1]
    # timestep is the catch to be
    last_ip <- c(stock$hcr_ip[last_iter,timestep-mp_params$timelag]) 
    last_op <- c(stock$hcr_op[last_iter,timestep])  
    lines(x = c(last_ip, last_ip), y=c(0, last_op), lty=2, lwd=2, col="blue")
    lines(x = c(0, last_ip), y=c(last_op, last_op), lty=2, lwd=2, col="blue")
    # Plot the last points
    #points(x=lastx, y=lasty, col=last_col, pch=last_pch, cex=last_cex)
  }
  
}




