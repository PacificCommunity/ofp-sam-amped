# Plotting functions for the Shiny apps.
# Note that none of these are exported and no man pages are written.
# They are documented for internal purposes only.
# plot_funcs.R

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: Waterfall Cities by Ozric Tentacles
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' Plot biomass to HCR arrow
#' 
#' Plots the connecting arrow between the biomass plot and the HCR plot in the 'Introduction to HCRs' app.
#' @param stock An R6 class Stock object.
#' @param timestep The time step of the HCR input.
#' @importFrom graphics "arrows" "grid" "legend" "lines" "par" "points" "polygon"
#' @noRd
#' @keywords internal 
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

#' Plot time periods
#' 
#' Adds the time periods to the current stock as vertical lines.
#' @param stock An R6 class Stock object.
#' @noRd
#' @keywords internal 
plot_time_periods <- function(stock){
    time_periods <- stock$time_periods()
    tp1 <- as.numeric(time_periods[[1]][1])# - 1 # So it looks better with catch
    tp2 <- as.numeric(time_periods[[2]][1])# - 1 
    tp3 <- as.numeric(time_periods[[3]][1])# - 1
    lines(x=c(tp1, tp1), y=c(-1e9, 1e9), lty=2, lwd=1, col="black")
    lines(x=c(tp2, tp2), y=c(-1e9, 1e9), lty=2, lwd=1, col="black")
    lines(x=c(tp3, tp3), y=c(-1e9, 1e9), lty=2, lwd=1, col="black")
    return(NULL)
}

#' plot_biomass
#'
#' plot_biomass() plots time series of 'true' and observed depletion (SB/SBF=0).
#' Used in all of the Shiny apps in this package.
#'
#' @param stock An R6 class Stock object.
#' @param mp_params The management procedure parameters (a list including mp_type).
#' @param ylab The x-label.
#' @param iters The iters to plot. Default is all of them.
#' @param max_spaghetti_iters The number of iterations to show as spaghetti before ribbons are shown. Default is 50.
#' @param quantiles Quantiles of the ribbons.
#' @param show_time_periods Boolean. Show the time period lines on the plot.
#' @param cex_leg Expansion of legend text (not handled by ... to plot)
#' @param ... Other arguments to pass to the plot() function.
#' @return A plot
#' @noRd
#' @keywords internal 
plot_biomass <- function(stock, mp_params, ylab = "SB/SBF=0", iters = 1:dim(stock$biomass)[1], max_spaghetti_iters=50, quantiles=c(0.05, 0.95), cex_leg = 1, show_time_periods = FALSE, ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # True bk
  bk_true <- stock$biomass / stock$k
  # Set up empty plot 
  ylim <- c(0,1) #max(1, max(bk_true, na.rm=TRUE), na.rm=TRUE)
  plot(x=years, y= bk_true[1,], type="n", ylab=ylab, xlab="Year", ylim=ylim, xaxs="i", yaxs="i", ...)
  # Add LRP and TRP
  lines(x=c(years[1], years[length(years)]),y=rep(stock$lrp, 2), lty=3, lwd=2, col="black")
  lines(x=c(years[1], years[length(years)]),y=rep(stock$trp, 2), lty=3, lwd=2, col="black")
  # Show time periods
  if(show_time_periods){
    plot_time_periods(stock)
  }
  # Add a grid
  grid()
  # Draw the iterations in grey
  true_col <- "black"
  est_col <- "blue"
  # Show the last iter - including true and observed if there is estimation variability
  last_iter <- max(iters)
  # Draw a ribbon if more than X iters
  if (length(iters) >= max_spaghetti_iters){
    draw_ribbon(x=years, y=bk_true[iters,], quantiles=quantiles)
    legend(x="bottomleft", legend=c("Average true biomass","Last replicate"), lty=c(2,1), lwd=2, col=true_col, cex=cex_leg)
  } else {
    for(i in iters){
      lines(x=years, y=bk_true[i,], col=scales::alpha("black", 0.25), lwd=2, lty=1)
    }
    # This will only work for model based for now
    # If we have a model based MP, show the HCR IP as it is the estimated biomass too
    #if (mp_params$mp_type == "model"){
    # With no estimation error, the true biomass is the same as the observed biomass
    if ((mp_params$est_sigma != 0) | (mp_params$est_bias != 0)){
      # Plot the estimated B/K  - plotted first so that the Intro to HCR shows it
      lines(x=years, y=stock$hcr_ip[last_iter,], col=est_col, lty=1, lwd=2)
      # Only show legend if not already showing a legend
      legend(x="bottomleft", legend=c("True","Estimated"), lwd=2, col=c(true_col, est_col), cex=cex_leg)
    }
  }
  # Plot the last true iteration in black
  lines(x=years, y=bk_true[last_iter,], col=true_col, lwd=2, lty=1)
  # Hack to show the estimated biomass on the Introduction to HCR app
  if(dim(stock$biomass)[1]==1){
    lines(x=years, y=stock$hcr_ip[last_iter,], col=est_col, lty=1, lwd=2)
  }
  
}

# Called by a couple of plotting functions
# Better than maintaining same code in multiple places
# So the HCR and catch plot in the Intro to HCR app have the same scale

#' Get ymax for catch plots
#' 
#' Useful function to calculate the ymax for catch plots
#' @param catch Catch data.
#' @param mp_params The MP parameters.
#' @noRd
#' @keywords internal 
get_catch_ymax <- function(catch, mp_params){
  # Sort out dimensions and labels
  # Bit annoying that we have a switch
  ymax <- switch(mp_params$hcr_shape,
                 constant = mp_params$params["constant_level"],
                 threshold = mp_params$params["max"])
  ymax <- max(c(ymax, c(catch)), na.rm=TRUE) * 1.1
  return(ymax)
}




# Plot a single iteration (the first one) with options for historical HCR OPs (used in Intro to HCR)
  # The is only used in the 'Introduction to HCRs' app.

#' Catch plot for Intro to HCR app
#' 
#' Plots a time series of the catches with the addition of the HCR output.
#' Only deals with a single iteration and is only used in the Introduction to HCRs app.
#' @param stock An R6 Stock class object.
#' @param mp_params The MP parameters.
#' @param timestep Current time timestep.
#' @noRd
#' @keywords internal 
plot_catch_hcr <- function(stock, mp_params, timestep, ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # Set ylim - use same as HCR plot
  ymax <- get_catch_ymax(stock$catch, mp_params)
  yrange <- c(0, ymax)
  # Empty axis
  plot(x=years, y=years, type="n", ylim=c(0, ymax), ylab="Catch", xlab="Year", xaxs="i", yaxs="i", ...)
  grid()
  
  # Plot the historical catches as horizontal dashed lines
  # (current timestep is considered as historical as that has already been fished)
  if (timestep > stock$last_historical_timestep){
    for (yr in (stock$last_historical_timestep+1):timestep){
      lines(x=c(years[1], years[length(years)]), y=rep(stock$catch[1, yr],2), lty=2, col="grey", lwd=2)
    }
  }
  
  # If HCR OP is total catch, plot the proposed catch from the HCR - i.e. the catch in the next time step
  # Could add something similar for relative effort based HCRs but I don't they should be included in the
  # 'Introduction to HCRs' app,
  # Would need an additional step of converting that relative effort to actual catch (done in the project() method)
  # This could be combined with the above bit of plotting ghosts - but is we need to convert the HCR OP to catch
  if(mp_params$output_type == "catch"){
    if (timestep < dim(stock$hcr_op)[2]){ 
      next_catch <- stock$hcr_op[1, timestep+1]
      # If HCR OP is catch multiplier - not implemented yet - used for empirical
      # if (mp_params$output_type == "catch multiplier"){
      #   next_catch <- stock$catch[1,timestep] * stock$hcr_op[,timestep+1]
      #   next_catch[next_catch < 10] <- 10 # A minimum catch
      # }
      lines(x=c(years[1], years[length(years)]), y=rep(next_catch,2), lty=2, col="blue", lwd=2) 
    }
  }
  
  # Plot the catch in the first (only) iteration
  lines(x=years, y=stock$catch[1,], col="blue", lwd=2, lty=1)
}

#' Plot the catch with iterations
#' 
#' Plot the catch time series when there are potentially multiple iterations.
#' Used in the Measuring Performance and Comparing Performance apps.
#' @param stock An R6 class Stock object.
#' @param mp_params The management procedure parameters (a list including mp_type).
#' @param iters The iters to plot. Default is all of them.
#' @param max_spaghetti_iters The number of iterations to show as spaghetti before ribbons are shown. Default is 50.
#' @param quantiles Quantiles of the ribbons.
#' @param show_time_periods Boolean. Show the time period lines on the plot.
#' @param cex_leg Expansion of legend text (not handled by ... to plot)
#' @param ... Other arguments to pass to the plot() function.
#' @noRd
#' @keywords internal 
plot_catch_iters <- function(stock, mp_params, iters = 1:dim(stock$biomass)[1], max_spaghetti_iters=50, quantiles=c(0.05, 0.95), cex_leg = 1, show_time_periods=FALSE, ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # Set ylim - use same as HCR plot
  ymax <- get_catch_ymax(stock$catch, mp_params)
  yrange <- c(0, ymax)
  # Empty axis
  plot(x=years, y=years, type="n", ylim=c(0, ymax), ylab="Catch", xlab="Year", xaxs="i", yaxs="i", ...)
  if(show_time_periods){
    plot_time_periods(stock)
  }
  grid()
  # Draw a ribbon if more than X iters
  if (length(iters) >= max_spaghetti_iters){
    draw_ribbon(x=years, y=stock$catch[iters,], quantiles=quantiles)
    legend(x="bottomleft", legend=c("Average catch","Last replicate"), lty=c(2,1), lwd=2, col="black", cex=cex_leg)
  } else {
  # Otherwise plot all iters in grey
    for(i in iters){
      lines(x=years, y=stock$catch[i,], lty=1, lwd=2, col=scales::alpha("black", 0.25))
    }
  }
  # Plot the most recent catch in black
  lines(x=years, y=stock$catch[max(iters),], lty=1, lwd=2, col="black")
} 

#' Plot relative CPUE
#' 
#' Plot time series of the CPUE relative to the CPUE in the last historical time step.
#' Used in the Measuring Performance and Comparing Performance apps.
#' @param stock An R6 class Stock object.
#' @param mp_params The management procedure parameters (a list including mp_type).
#' @param iters The iters to plot. Default is all of them.
#' @param max_spaghetti_iters The number of iterations to show as spaghetti before ribbons are shown. Default is 50.
#' @param quantiles Quantiles of the ribbons.
#' @param show_time_periods Boolean. Show the time period lines on the plot.
#' @param cex_leg Expansion of legend text (not handled by ... to plot)
#' @param ... Other arguments to pass to the plot() function.
#' @noRd
#' @keywords internal 
plot_cpue <- function(stock, mp_params, iters = 1:dim(stock$biomass)[1], max_spaghetti_iters=50, quantiles=c(0.05, 0.95), cex_leg=1, show_time_periods = FALSE,  ...){
  cpue <- stock$relative_cpue()
  years <- as.numeric(dimnames(cpue)$year)
  # Set ylim - use same as HCR plot
  ymax <- max(1, cpue, na.rm=TRUE)
  yrange <- c(0, ymax)
  # Empty axis
  plot(x=years, y=years, type="n", ylim=c(0, ymax), ylab="Relative CPUE (catch rate)", xlab="Year", xaxs="i", yaxs="i", ...)
  if(show_time_periods){
    plot_time_periods(stock)
  }
  grid()
  # Draw a ribbon if more than X iters
  if (length(iters) >= max_spaghetti_iters){
    draw_ribbon(x=years, y=cpue[iters,], quantiles=quantiles)
    legend(x="bottomleft", legend=c("Average CPUE","Last replicate"), lty=c(2,1), lwd=2, col="black", cex=cex_leg)
  } else {
  # Otherwise plot all iters in grey
    for(i in iters){
      lines(x=years, y=cpue[i,], lty=1, lwd=2, col=scales::alpha("black", 0.25))
    }
  }
  # Plot the most recent catch in black
  lines(x=years, y=cpue[max(iters),], lty=1, lwd=2, col="black")
} 



#' Draw uncertainty ribbon.
#' 
#' Add ribbon based on quantiles and add median to the current plot.
#' @param x Year range
#' @param y The data
#' @param quantiles A vector of quantiles (length 2)
#' @noRd
#' @keywords internal 
draw_ribbon <- function(x, y, quantiles){
  # Get quantiles
  qs <- apply(y, 2, function(x){quantile(x, probs=c(quantiles[1], 0.5, quantiles[2]), na.rm=TRUE)})
  # Draw envelope using polygon - horrible
  polyx <- c(x, rev(x))
  polyy <- c(qs[1,],rev(qs[3,]))
  # Drop NAs
  polyx <- polyx[!is.na(polyy)]
  polyy <- polyy[!is.na(polyy)]
  polygon(x=polyx, y=polyy, col="grey", border=NA)
  # Add qlines
  #lines(x=x, y=qs[1,], lty=ribbon_lty, col=ribbon_border_col, lwd=ribbon_lwd)
  #lines(x=x, y=qs[3,], lty=ribbon_lty, col=ribbon_border_col, lwd=ribbon_lwd)
  # Add median
  lines(x=x, y=qs[2,], lty=2, col="black", lwd=2)
}




  
#' Plot a model based HCR
#' 
#' Plot a model based HCR, including current stock status and current HCR OP.
#' Can also show the current and historical input and output values depending on the \code(timestep) and \code{iter} arguments.
#' @param stock An R6 class Stock object.
#' @param mp_params The management procedure parameters (a list including mp_type).
#' @param timestep The current timestep (optional)
#' @param iter The current iter (optional).
#' @param show_ref_pts Boolean. Plot the LRP and TRP lines. Default is false.
#' @noRd
#' @keywords internal 
plot_model_based_hcr <- function(stock, mp_params, timestep=NULL, iter=NULL, show_ref_pts=FALSE, ...){
  
  # Stop timestep going beyond bounds
  if (!is.null(timestep)){
    if (timestep > dim(stock$hcr_ip)[2]){
      timestep <- dim(stock$hcr_ip)[2]
    }
  }
  
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
  xlab <- "Estimated biomass"
  # Plot empty axes 
  plot(x=xrange,y=yrange,type="n",xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", ...) 
  grid()
  
  # Add the B/K based reference points
  if (show_ref_pts){
    lines(x=rep(stock$lrp,2), y=c(0, ymax),lty=3, lwd=2, col="black")
    lines(x=rep(stock$trp,2), y=c(0, ymax),lty=3, lwd=2, col="black")
  }
  
  # Add all HCR inputs and outputs so far
  # The years that the HCR has been active
  hcr_ip_yrs <- ((stock$last_historical_timestep+1):dim(stock$hcr_ip)[2]) - mp_params$timelag
  hcr_op_yrs <- (stock$last_historical_timestep+1):dim(stock$hcr_ip)[2]
  # Plot current timestep of first iter in a different shade
  # Used in Introduction to HCR app
  if (!is.null(timestep)){
    # Only show the HCR years (from last historical timestep)
    # Show ghosts
    points(x=c(stock$hcr_ip[1,hcr_ip_yrs]), y=c(stock$hcr_op[1,hcr_op_yrs]), col="grey", pch=16, cex=2)
    # Show current
    points(x=c(stock$hcr_ip[1,timestep - mp_params$timelag]),
           y=c(stock$hcr_op[1,timestep]), col="blue", pch=16, cex=2)
  } else if (!is.null(iter)) {
    # Used in Measuring and Comparing Performance apps
    # Only show points if a projection has been run?
    iters_run <- !is.na(stock$catch[,dim(stock$catch)[2]])
    points(x=c(stock$hcr_ip[iters_run,hcr_ip_yrs]), y=c(stock$hcr_op[iters_run,hcr_op_yrs]), col=scales::alpha("black", 0.1), pch=16, cex=2)
    if(length(iter) == 1){
      points(x=c(stock$hcr_ip[iter,hcr_ip_yrs]), y=c(stock$hcr_op[iter,hcr_op_yrs]), col="blue", pch=16, cex=2)
    }
  }
  
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
  
  # If timestep is present, show the IP and OP as lines for the 1st iter and timestep
  # Used in Introduction to HCRs app
  # Plotted last so they lie on top of everything else
  if (!is.null(timestep)){
    # timestep is the catch to be
    last_ip <- c(stock$hcr_ip[1,timestep-mp_params$timelag]) 
    last_op <- c(stock$hcr_op[1,timestep])  
    lines(x = c(last_ip, last_ip), y=c(0, last_op), lty=2, lwd=2, col="blue")
    lines(x = c(0, last_ip), y=c(last_op, last_op), lty=2, lwd=2, col="blue")
  } 
}




