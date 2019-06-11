# Common plotting routines for AMPED and PIMPLE
# 
# Copyright 2019 Finlay Scott. Distributed under the GPL 3 or later
# Maintainer: Finlay Scott, SPC
#

# Plotting functions
# Globals - eesh!
# Plot params - could make arguments
#, app_params=app_params WTF? There are so many of them - reduce this madness
  hcr_lwd <-  2
  hcr_lty <-  1
  hcr_col <- "red"

  true_col <- "black"

  last_col <- "blue"
  last_cex <- 2
  last_pch <- 16
  last_lty <- 1
  last_lwd <- 2

  guide_lty <- 2
  guide_lwd <- 2
  guide_col <- "blue"

  ref_lty <- 3
  ref_lwd <- 2
  ref_col <- "black"

  ghost_col <- "grey"
  ghost_pch <- 16
  ghost_cex <- 2
  ghost_lty <- 1
  ghost_lwd <- 2

  spaghetti_col <- "black"
  spaghetti_lty <- 1
  spaghetti_lwd <- 1

  ribbon_col <- "grey"
  ribbon_lty <- 1
  ribbon_border_col="black"
  ribbon_lwd = 2
  
  median_col <- "blue"
  median_lty <- 2
  median_lwd <- 2

# Called by a couple of plotting functions
# Better than maintaining same code in multiple places
get_catch_ymax <- function(catch, mp_params){
  # Sort out dimensions and labels
  # Bit annoying that we have a switch
  ymax <- switch(mp_params$hcr_shape,
         constant = mp_params$params["constant_level"],
         threshold = mp_params$params["max"]
  )
  ymax <- max(c(ymax, c(catch)), na.rm=TRUE) * 1.1
  return(ymax)
}

sideways_histogram <- function(dat, range, lhist=20, num.dnorm=5*lhist, dcol="blue"){
    yhist <- hist(dat, plot=FALSE, breaks=seq(from=range[1], to=range[2], length.out=lhist)) 
    # Use dnorm
    yx <- seq(range[1], range[2], length.out=num.dnorm)
    yy <- dnorm(yx, mean=mean(dat), sd=sd(dat))
    yy[is.infinite(yy)] <- 1.0
    parmar <- par()$mar
    par(mar=c(parmar[1], 0, parmar[3], 0))
    barplot(yhist$density, axes=FALSE, xlim=c(0, max(yhist$density, yy)), space=0, horiz=TRUE, xaxs="i", yaxs="i") # barplot
    lines(yy, seq(from=0, to=lhist-1, length.out=num.dnorm), col=dcol) # line
    # Or use density
    #dens <- density(dat)
    #barplot(yhist$density, axes=FALSE, xlim=c(0, max(yhist$density, dens$y)), space=0, horiz=TRUE) # barplot
    #lines(dens$y, seq(from=0, to=lhist-1, length.out=length(dens$y)), col=dcol) # line
}

# Draw the HCR (output vs input)
# Input can be SB/SBF=0, or some output from empirical MP
# Try to keep it flexible
# type - are we stepping or projecting? Better way of doing this
# Use ... instead of passing in more args like timestep?
plot_hcr <- function(stock, stock_params, mp_params, app_params, timestep=NULL, show_last=TRUE, ...){

  if (mp_params$output_type=="catch"){
    ymax <- get_catch_ymax(stock$catch, mp_params)
  }
  if (mp_params$output_type=="relative effort"){
    rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
    ymax <- max(c(rel_effort, mp_params$params["max"], mp_params$params["constant_level"]), na.rm=TRUE) * 1.1
  }

  ylab <- paste("Next ", mp_params$output_type, sep="")
  yrange <- c(0, ymax)
  # Need to set these depending on the HCR somehow
  # From MP analysis type? Leave as B/K for now. Could set additional argument in mp_params like xlab?
  xrange <- c(0, 1)
  #xlab <- "Estimated biomass / K"
  xlab <- "Estimated SB/SBF=0"
  # Plot empty axes 
  plot(x=xrange,y=yrange,type="n",xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", main="The HCR") 

  # If model based MP (or NA) add the B/K based reference points
  if (mp_params$mp_type %in% c(as.character(NA), "model")){
    lines(x=c(stock_params[["lrp"]],stock_params[["lrp"]]), y=c(0, ymax),lty=ref_lty, lwd=ref_lwd, col=ref_col)
    lines(x=c(stock_params[["trp"]],stock_params[["trp"]]), y=c(0, ymax),lty=ref_lty, lwd=ref_lwd, col=ref_col)
  }
  # If empirical MP how are the reference points defined? Still B/K or some CPUE based measure?

  # Plot HCR outline - this is horrible because it depends on the HCR shape
  # Constant catch
  if (mp_params$hcr_shape == "constant"){
    lines(x=c(0, 1), y=c(mp_params$params["constant_level"], mp_params$params["constant_level"]), lwd=hcr_lwd, lty=hcr_lty, col=hcr_col)
  }
  # Threshold
  else if (mp_params$hcr_shape == "threshold"){
    lines(x = c(0, mp_params$params["lim"], mp_params$params["elbow"], 1),
      y = c(mp_params$params["min"], mp_params$params["min"], mp_params$params["max"], mp_params$params["max"]), lwd=hcr_lwd, lty=hcr_lty, col=hcr_col)
  }
  else {
    stop("In plot_hcr(). Trying to plot the HCR shape but hcr_shape is not recognised.")
  }

  # Add all HCR inputs and outputs so far
  # Only show the HCR years (from last historical timestep)
  # The years that the HCR has been active
  hcr_ip_yrs <- ((app_params$last_historical_timestep+1):dim(stock$hcr_ip)[2]) - mp_params$timelag
  hcr_op_yrs <- (app_params$last_historical_timestep+1):dim(stock$hcr_ip)[2]
  # Show all iters and timesteps as ghosts
  points(x=c(stock$hcr_ip[,hcr_ip_yrs]), y=c(stock$hcr_op[,hcr_op_yrs]), col=ghost_col, pch=ghost_pch, cex=ghost_cex)

  # Show iters 1:last_iter as ghosts
  # Show last as last
  # If there is a timestep argument, last is just that timestep  
  # Else, last is all of the last iter 
  if (show_last){
    last_iter <- dim(stock$hcr_ip)[1]
    # No timestep so show all timesteps in the iter
    if (is.null(timestep)){
      lastx <- c(stock$hcr_ip[last_iter,hcr_ip_yrs]) 
      lasty <- c(stock$hcr_op[last_iter,hcr_op_yrs]) 
    }
    # If we have a timestep, just the last timestep of the last iter
    else {
      # Check that timestep has not gone beyond bounds
      if (timestep > dim(stock$hcr_ip)[2]){
        timestep <- dim(stock$hcr_ip)[2]
      }
      # timestep is the catch to be
      lastx <- c(stock$hcr_ip[last_iter,timestep-mp_params$timelag]) 
      lasty <- c(stock$hcr_op[last_iter,timestep])  
      # Show the last point as lines
      lines(x = c(lastx, lastx), y=c(0, lasty), lty=guide_lty, lwd=guide_lwd, col=guide_col)
      lines(x = c(0, lastx), y=c(lasty, lasty), lty=guide_lty, lwd=guide_lwd, col=guide_col)
    }
    # Plot the last points
    points(x=lastx, y=lasty, col=last_col, pch=last_pch, cex=last_cex)
  }
}

# Draw polygon and median
draw_ribbon <- function(x, y, quantiles){
  # Get quantiles
  qs <- apply(y, 2, function(x){quantile(x, probs=c(quantiles[1], 0.5, quantiles[2]), na.rm=TRUE)})
  # Draw envelope using polygon - horrible
  polyx <- c(x, rev(x))
  polyy <- c(qs[1,],rev(qs[3,]))
  # Drop NAs
  polyx <- polyx[!is.na(polyy)]
  polyy <- polyy[!is.na(polyy)]
  polygon(x=polyx, y=polyy, col=ribbon_col, border=NA)
  # Add qlines
  lines(x=x, y=qs[1,], lty=ribbon_lty, col=ribbon_border_col, lwd=ribbon_lwd)
  lines(x=x, y=qs[3,], lty=ribbon_lty, col=ribbon_border_col, lwd=ribbon_lwd)
  # Add median
  lines(x=x, y=qs[2,], lty=median_lty, col=median_col, lwd=median_lwd)
}

# Biomass / K
# Plot 'true' and only plot observed if model based MP
plot_biomass <- function(stock, stock_params, mp_params, timestep=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, xlab="Year", ghost_col="grey", last_col="blue", ylim=c(0,1), ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # True bk
  bk_true <- stock$biomass / stock_params[["k"]]
  # Set up empty plot 
  plot(x=years, y= bk_true[1,], type="n", ylab="SB/SBF=0", xlab=xlab, ylim=ylim, xaxs="i", yaxs="i", ...)
  # Add LRP and TRP
  lines(x=c(years[1], years[length(years)]),y=c(stock_params[["lrp"]],stock_params[["lrp"]]), lty=ref_lty, lwd=ref_lwd, col=ref_col)
  lines(x=c(years[1], years[length(years)]),y=c(stock_params[["trp"]],stock_params[["trp"]]), lty=ref_lty, lwd=ref_lwd, col=ref_col)
  if (add_grid){
    grid()
  }
  # Get last iteration 
  last_iter <- dim(bk_true)[1]

  # If we have more than X iters, draw envelope of iters
  if(last_iter > max_spaghetti_iters){
    # Draw ribbon
    # Send the 2nd and 4th quantile
    draw_ribbon(x=years, y=bk_true, quantiles=quantiles)
    # Add spaghetti
    for (iter in 1:nspaghetti){
      lines(x=years, y=bk_true[iter,], lty=spaghetti_lty, lwd=spaghetti_lwd, col=spaghetti_col)
    }
  }
  # If we are not drawing the envelope, show all iters as ghosts
  else {
    if (last_iter > 1){
      for (i in 1:(last_iter - 1)){
        lines(x=years, y=bk_true[i,], col=ghost_col, lty=ghost_lty, lwd=ghost_lwd)
      }
    }
  }

  # Show the last iter as an illustration
  if(show_last){
    # Plot the true B/K  - plotted first so that the Intro to HCR shows it
    lines(x=years, y=bk_true[last_iter,], col=true_col, lwd=last_lwd, lty=last_lty)
    # If we have a model based MP, show the HCR IP as it is the estimated biomass too
    if (mp_params$mp_type == "model"){
      lines(x=years, y=stock$hcr_ip[last_iter,], col=last_col, lty=last_lty, lwd=last_lwd)
      # And if we have obs error
      if ((stock_params$biol_est_sigma != 0) | (stock_params$biol_est_bias != 0)){
        legend(x="bottomleft", legend=c("True","Estimated"), lwd=2,col=c(true_col, last_col))
      }
    }
  }
}

# If we have timestep we also need app_params
# quantiles of length 2
# If time, try to use the generic plot below
plot_catch <- function(stock, stock_params, mp_params, app_params=NULL, timestep=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, xlab="Year", ghost_col="grey", true_col="black", ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # Set Ylim - use same as HCR plot
  ymax <- get_catch_ymax(stock$catch, mp_params)
  yrange <- c(0, ymax)
  # Empty axis
  plot(x=years, y=years, type="n", ylim=c(0, ymax), ylab="Catch", xlab=xlab, xaxs="i", yaxs="i",...)
  if (add_grid){
    grid()
  }
  # Get last iteration 
  last_iter <- dim(stock$catch)[1]

  # If we have more than X iters, draw envelope of iters
  if(last_iter > max_spaghetti_iters){
    # Draw ribbon
    draw_ribbon(x=years, y=stock$catch, quantiles=quantiles)
    # Add spaghetti
    for (iter in 1:nspaghetti){
      lines(x=years, y=stock$catch[iter,], lty=spaghetti_lty, lwd=spaghetti_lwd, col=spaghetti_col)
    }
  }
  # Else plot individual catch iters, depending on timestep argument
  else{
    # If we have a timestep then plot horizontal lines showing the catches in the last timestep
    if (!is.null(timestep)){
      if (timestep > app_params$last_historical_timestep){
        for (yr in (app_params$last_historical_timestep+1):timestep){
          lines(x=c(years[1], years[length(years)]), y=rep(stock$catch[last_iter, yr],2), lty=guide_lty, col=ghost_col, lwd=guide_lwd)
        }
      }
      # Also plot the proposed catch from the HCR
      # Careful with the timelag here? Should be OK
      if (timestep < dim(stock$hcr_op)[2]){
        lines(x=c(years[1], years[length(years)]), y=rep(stock$hcr_op[last_iter, timestep+1],2),lty=guide_lty, col=guide_col, lwd=guide_lwd) 
      }
    }
    # Plot all iters as ghosts
    if (last_iter > 1){
      for (i in 1:last_iter){
        lines(x=years, y=stock$catch[i,], col=ghost_col, lwd=ghost_lwd, lty=ghost_lty)
      }
    }
  }
  # Current iteration
  if(show_last){
    lines(x=years, y=stock$catch[last_iter,], col=true_col, lwd=last_lwd, lty=last_lty)
  }
}


# Generic timeseries plot
plot_indiv_timeseries_base <- function(data, stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, yrange, ylab, add_grid=TRUE, xlab="Year", ghost_col="grey", true_col="black", ...){

  years <- as.numeric(dimnames(stock$biomass)$year)
  # Plot empty axis
  plot(x=years, y=years, type="n", ylim=c(yrange[1], yrange[2]), ylab=ylab, xlab=xlab, xaxs="i", yaxs="i",...)
  if (add_grid){
    grid()
  }
  # Get last iteration 
  last_iter <- dim(data)[1]
  # If we have more than X iters, draw envelope of iters
  if(last_iter > max_spaghetti_iters){
    # Draw ribbon
    draw_ribbon(x=years, y=data, quantiles=quantiles)
    # Add spaghetti
    for (iter in 1:nspaghetti){
      lines(x=years, y=data[iter,], lty=spaghetti_lty, lwd=spaghetti_lwd, col=spaghetti_col)
    }
  }
  # Else plot individual iters
  else{
    # Plot all iters as ghosts
    if (last_iter > 1){
      for (i in 1:last_iter){
        lines(x=years, y=data[i,], col=ghost_col, lwd=ghost_lwd, lty=ghost_lty)
      }
    }
  }
  # Current iteration
  if(show_last){
    lines(x=years, y=data[last_iter,], col=true_col, lwd=last_lwd, lty=last_lty)
  }

}

 


# Relative CPUE
plot_relcpue <- function(stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, ...){

  years <- as.numeric(dimnames(stock$biomass)$year)
  cpue <- stock$catch / stock$effort
  rel_cpue <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
  ymax <- max(c(rel_cpue * 1.1, 1.0), na.rm=TRUE)
  yrange <- c(0, ymax)

  # Plot it
  plot_indiv_timeseries_base(data=rel_cpue, stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, nspaghetti=nspaghetti, yrange=yrange, ylab="Relative CPUE", add_grid=add_grid, ...)

  # Add 1 line
  lines(x=years,y=rep(1,length(years)), lty=2)

}


plot_releffort <- function(stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  # Set Ylim - use same as HCR plot
  ymax <- max(c(rel_effort * 1.1, 1.0), na.rm=TRUE)
  ymax <- min(10, ymax, na.rm=TRUE)
  yrange <- c(0, ymax)

  # Plot it
  plot_indiv_timeseries_base(data=rel_effort, stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, nspaghetti=nspaghetti, yrange=yrange, ylab="Relative effort", add_grid=add_grid, ...)

  # Add 1 line
  lines(x=years,y=rep(1,length(years)), lty=2)

}


plot_F <- function(stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  harvest_rate <- stock$catch / stock$biomass
  # Set Ylim - use same as HCR plot
  ymax <- max(harvest_rate, na.rm=TRUE) * 1.1
  yrange <- c(0, ymax)

  # Plot it
  plot_indiv_timeseries_base(data=harvest_rate, stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, nspaghetti=nspaghetti, yrange=yrange, ylab="F", add_grid=add_grid, ...)

}

# Move to main app window?
# Projection plots for the IntroProjections app
plot_projection <- function(stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, ...){
  current_col <- "blue"
  prev_col <- "black"

  par(mfrow=c(3,1))
  # And then cock about with margins
  par(mar=c(0, 4.1, 5, 2.1))
  plot_biomass(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=quantiles, max_spaghetti_iters=max_spaghetti_iters, xaxt='n', xlab="", ghost_col=prev_col, last_col=current_col, ylim=c(0,1.1), ...)

  par(mar=c(2.5, 4.1, 2.5, 2.1))
  plot_catch(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=quantiles, max_spaghetti_iters=max_spaghetti_iters, xaxt='n', xlab="", ghost_col=prev_col, true_col=current_col, ...)

  par(mar=c(5, 4.1, 0, 2.1))
  plot_releffort(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, quantiles=quantiles, max_spaghetti_iters=max_spaghetti_iters, ghost_col=prev_col, true_col=current_col, ...)
}

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
  lines(x=x,y=y,col=guide_col, lwd=guide_lwd)
  # Add an arrow
  arrows(x0=x[length(x)-1], y0=y[length(y)-1], x1=x[length(x)], y1=y[length(y)],col=guide_col,lwd=guide_lwd)
}

# This works but...
# It's hard to get a single plot (2x2) to be the right dims.
# It looks better plotting each plot separately
plot_hcr_intro <- function(stock, stock_params, mp_params, app_params, timestep){
  # Main plot for the Intro to HCR app
  # 2 x 2 panel
  # Catch  |  HCR
  # ----------------
  # B/K    |  connecting arrow

  par(mfrow=c(2,2))
  #par(mfrow=c(2,2), pty="s") # make square - looks bad
  # Catch
  # timestep is the timestep of the catch
  plot_catch(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, timestep=timestep)

  # HCR
  # Add one more onto the timestep because the timestep coming in is for the catch and biomass, hcr_ip and hcr_op are 1 step ahead
  plot_hcr(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, timestep=timestep+1)

  # B/K
  plot_biomass(stock=stock, stock_params=stock_params, mp_params=mp_params, timestep=timestep+1)

  # Arrow from B/K to HCR
  btimestep <- min(timestep+1, dim(stock$hcr_ip)[2])
  last_hcr_ip  <- stock$hcr_ip[1, btimestep]
  npoints <- 100
  theta <- seq(from=0, to=pi/2, length=npoints)
  x <- sin(theta) * last_hcr_ip
  y <- -1 * cos(theta) * (1-last_hcr_ip)
  # Set up plot
  plot(x=x, y=y, xlim=c(0,1), ylim=c(-1,0), type="n", xaxt="n", yaxt="n",xlab="", ylab="", axes=FALSE,xaxs="i", yaxs="i")
  lines(x=x,y=y,col=guide_col, lwd=guide_lwd)
  # Add an arrow
  # Just last two points looks weird
  arrows(x0=x[length(x)-1], y0=y[length(y)-1], x1=x[length(x)], y1=y[length(y)],col=guide_col,lwd=guide_lwd)
}

# Could combine with function above into a single 
# quantiles of length 2: lower and upper
plot_metric_with_histo <- function(stock, stock_params, mp_params, metric, app_params=NULL, show_last=TRUE, quantiles=c(0.2,0.8)){
  # Plot the metric with an extra sideways histogram
  layout(matrix(c(1,2), ncol=2), widths=c(6/7, 1/7))
  ospc <- 0.5 # outer space
  pext <- 4 # par extension down and to the left
  bspc <- 1 # space between scatter plot and bar plots
  par. <- par(mar=c(pext, pext, bspc, bspc), oma=rep(ospc, 4)) # plot parameters
  if (metric == "biomass"){
    # The timeseries of biomass in the big window
    plot_biomass(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=quantiles)
    # The histogram should be the true B/K
    final_yr <- dim(stock$biomass)[2]
    dat <- stock$biomass[,final_yr] / stock_params$k
    range <- c(0,1)
  }
  else if (metric == "catch"){
    # The timeseries of biomass in the big window
    plot_catch(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=quantiles)
    final_yr <- dim(stock$catch)[2]
    dat <- stock$catch[,final_yr]
    range <- c(0, get_catch_ymax(stock$catch, mp_params))
  }
  else if (metric == "relcpue"){
    plot_relcpue(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, quantiles=quantiles)
    cpue <- stock$catch / stock$effort
    rel_cpue <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
    final_yr <- dim(rel_cpue)[2]
    dat <- rel_cpue[,final_yr]
    range <- c(0, max(c(rel_cpue * 1.1, 1.0), na.rm=TRUE))
  }
  else {
    stop("In plot_metric_with_histo(). Unknown metric\n")
  }
  # Adapted from: https://stackoverflow.com/questions/11022675/rotate-histogram-in-r-or-overlay-a-density-in-a-barplot
  if((nrow(stock$hcr_ip) > 1) & (!all(is.na(dat)))){
    # Get all iters and right timesteps (including timelag)
    sideways_histogram(dat=dat, range=range)
    # restore parameters
  }
  #par(par.)
}

#---------------------------------------------------------------
# Majuoro plotting functions
# Plot a single stock using the stock object
# Plotting all the stocks using the time series results
plot_majuro_all_stocks <- function(timeseries, hcr_choices, stock_params){
  # Put into one big DF
  timeseries <- dplyr::bind_rows(timeseries, .id="hcr")
  # Get the colour palette and add colour to the dataframe 
  hcrcols <- get_hcr_colours(hcr_names=unique(timeseries$hcr), chosen_hcr_names=hcr_choices)
  hcrcolsdf <- data.frame(hcr=names(hcrcols), colour=unname(hcrcols), stringsAsFactors=FALSE)
  # Subset metrics and types
  timeseries <- subset(timeseries, (hcr %in% hcr_choices) & (type=="quantile") & (metric %in% c("bk", "ffmsy")))
  # Add in the colours
  timeseries <- merge(timeseries, hcrcolsdf)
  # Add Legend name through the the HCR number
  hcrno <- unlist(lapply(strsplit(timeseries$hcr, "\\."),"[",1))
  timeseries$hcrlegend <- paste("HCR ", hcrno, sep="")
  plot_majuro(timeseries, stock_params)
}

plot_majuro_single_stock <- function(stock, stock_params, quantiles){
  # Get the medians and percentiles
  fmsy <- stock_params$r / 2
  f <- stock$catch / stock$biomass 
  ffmsy <- f / fmsy
  ffmsyq <- get_quantiles(ffmsy, quantiles=quantiles)
  ffmsyq <- cbind(metric="ffmsy", name="F / FMSY", level=c("lower","median","upper"),as.data.frame(ffmsyq))
  bk <- stock$biomass / stock_params$k
  bkq <- get_quantiles(bk, quantiles=quantiles)
  bkq <- cbind(metric="bk", name="SB / SBF=0", level=c("lower","median","upper"),as.data.frame(bkq))
  dat <- rbind(bkq, ffmsyq)
  dat <- gather(dat, key="year", value="value",-level, -metric, -name)
  dat$hcr <- "Current"
  dat$colour <- "black"
  dat$hcrlegend <- NA
  plot_majuro(dat, stock_params)
}

# For plotting with the IntroProjection app
# stock has multiple projections as rows
plot_majuro_projections <- function(stock, stock_params){
  fmsy <- stock_params$r / 2
  f <- stock$catch / stock$biomass 
  ffmsy <- f / fmsy
  ffmsy <- as.data.frame(ffmsy)
  ffmsy <- cbind(ffmsy, hcr=1:nrow(ffmsy))
  ffmsy <- gather(ffmsy, key="year", value="value", -hcr)
  ffmsy <- cbind(ffmsy, metric="ffmsy", name="F / FMSY", level="median", colour="black",hcrlegend=NA, stringsAsFactors=FALSE)
  bk <- stock$biomass / stock_params$k
  bk <- as.data.frame(bk)
  bk <- cbind(bk, hcr=1:nrow(bk))
  bk <- gather(bk, key="year", value="value", -hcr)
  bk <- cbind(bk, metric="bk", name="SB / SBF=0", level="median", colour="black",hcrlegend=NA, stringsAsFactors=FALSE)
  # Need to shunt the years by 1 as B in year Y is the result of F in year Y-1 
  # So add 1 to the F years
  bk$year <- as.numeric(bk$year)
  ffmsy$year <- as.numeric(ffmsy$year)
  ffmsy$year <- ffmsy$year + 1
  # Only include common years between the two sets
  common_years <- intersect(bk$year, ffmsy$year)
  dat <- rbind(subset(bk, year %in% common_years), subset(ffmsy, year %in% common_years))
  # Set last hcr to blue
  last_hcr <- nrow(stock$catch)
  dat[dat$hcr==last_hcr,"colour"] <- "blue"
  plot_majuro(dat, stock_params)
}

plot_kobe_projections<- function(stock, stock_params){
  fmsy <- stock_params$r / 2
  f <- stock$catch / stock$biomass 
  ffmsy <- f / fmsy
  ffmsy <- as.data.frame(ffmsy)
  ffmsy <- cbind(ffmsy, hcr=1:nrow(ffmsy))
  ffmsy <- gather(ffmsy, key="year", value="value", -hcr)
  ffmsy <- cbind(ffmsy, metric="ffmsy", name="F / FMSY", level="median", colour="black",hcrlegend=NA, stringsAsFactors=FALSE)
  # BMSY = K / 2
  bmsy <- stock_params$k / 2
  bbmsy <- stock$biomass / bmsy
  bbmsy <- as.data.frame(bbmsy)
  bbmsy <- cbind(bbmsy, hcr=1:nrow(bbmsy))
  bbmsy <- gather(bbmsy, key="year", value="value", -hcr)
  bbmsy <- cbind(bbmsy, metric="bbmsy", name="B / BMSY", level="median", colour="black",hcrlegend=NA, stringsAsFactors=FALSE)
  # Need to shunt the years by 1 as B in year Y is the result of F in year Y-1 
  # So add 1 to the F years
  bbmsy$year <- as.numeric(bbmsy$year)
  ffmsy$year <- as.numeric(ffmsy$year)
  ffmsy$year <- ffmsy$year + 1
  # Only include common years between the two sets
  common_years <- intersect(bbmsy$year, ffmsy$year)
  dat <- rbind(subset(bbmsy, year %in% common_years), subset(ffmsy, year %in% common_years))
  # Set last hcr to blue
  last_hcr <- nrow(stock$catch)
  dat[dat$hcr==last_hcr,"colour"] <- "blue"
  plot_kobe(dat, stock_params)
}


# Dat is a data.frame with columns:
# hcr (name)
# metric (bk or ffmsy)
# name ("SB/SBF=0" or "F / FMSY")
# level (lower, median, upper)
# year
# value
# type - not used apparently
# colour
# hcrlegend
plot_majuro <- function(dat, stock_params){
  ymax <- max(c(2.0, 1.1*subset(dat, metric=="ffmsy" & level=="upper")$value, 1.1*subset(dat, metric=="ffmsy" & level=="median")$value), na.rm=TRUE)
  ymax <- min(ymax, 5.0) # In case of stock collapse
  # Set up the axes
  plot(x=c(0,1), y=c(0,ymax), type="n", xlim=c(0,1), ylim=c(0,ymax), xlab = "SB / SBF=0", ylab = "F / FMSY", xaxs="i", yaxs="i")
  # Set the colour panels
  # The big red one
  rect(0.0,0.0,stock_params$lrp,ymax, border="black", col="red", lty=1, lwd=1)
  # The orange one
  rect(stock_params$lrp,1.0,1.0,ymax, border="black", col="orange", lty=1, lwd=1)
  # The other one - leave white for now
  rect(stock_params$lrp,0.0,1.0,1.0, border="black", col="white", lty=1, lwd=1)
  # Loop over HCRs
  hcrs <- unique(dat$hcr)
  for (hcrcount in hcrs){
    hcrdat <- subset(dat, hcr==hcrcount)
    colour <- hcrdat$colour[1]
    # Add the medians as points and a line to tell the story
    medbk <- subset(hcrdat, metric == "bk" & level=="median")$value
    medffmsy <- subset(hcrdat, metric == "ffmsy" & level=="median")$value
    points(x=medbk, y=medffmsy, pch=16, col=colour)
    # Do a thick black line then overlay the real line
    lines(x=medbk, y=medffmsy, lty=1, lwd=4, col="black")
    lines(x=medbk, y=medffmsy, lty=1, lwd=3, col=colour)
    # Add quantile lines
    lowerbk <- subset(hcrdat, metric == "bk" & level=="lower")$value
    lowerffmsy <- subset(hcrdat, metric == "ffmsy" & level=="lower")$value
    upperbk <- subset(hcrdat, metric == "bk" & level=="upper")$value
    upperffmsy <- subset(hcrdat, metric == "ffmsy" & level=="upper")$value
    # We always have one more B value so use FFMsy for final year
    finalyr <- max(which(!is.na(medffmsy))) 
    for (yr in 1:finalyr){
      lines(x=c(medbk[yr],medbk[yr]), y=c(lowerffmsy[yr],upperffmsy[yr]), lty=1, lwd=4, col="black")
      lines(x=c(medbk[yr],medbk[yr]), y=c(lowerffmsy[yr],upperffmsy[yr]), lty=1, lwd=3, col=colour)
      lines(x=c(lowerbk[yr], upperbk[yr]),y=c(medffmsy[yr], medffmsy[yr]), lty=1, lwd=4, col="black")
      lines(x=c(lowerbk[yr], upperbk[yr]),y=c(medffmsy[yr], medffmsy[yr]), lty=1, lwd=3, col=colour)
    }
    # Blob at the end
    finalbk <- medbk[finalyr]
    finalffmsy <- medffmsy[finalyr]
    points(x=finalbk, y=finalffmsy, pch=21, col=colour, bg="white")
  }
  # Add legend
  # Adapt HCR name text
  # This is wrong - numbers not necessarily 1:X 
  #if (!("Current" %in% hcrs )){ # Match single stock name in above function - bit dodgy
  if(!any(is.na(dat$hcrlegend))){
    hcrnames <- unique(dat$hcrlegend)
    legend(x="bottomright", legend=hcrnames, lty=1, lwd=4, col=unique(dat$colour), cex=1.0)
  }
}

# Dat is a data.frame with columns:
# hcr (name)
# metric (bk or ffmsy)
# name ("SB/SBF=0" or "F / FMSY")
# level (lower, median, upper)
# year
# value
# type - not used apparently
# colour
# hcrlegend
# As majuro - would be good to combine the two - only difference is B metric and colours
plot_kobe <- function(dat, stock_params){
  ymax <- max(c(2.0, 1.1*subset(dat, metric=="ffmsy" & level=="upper")$value, 1.1*subset(dat, metric=="ffmsy" & level=="median")$value), na.rm=TRUE)
  ymax <- min(ymax, 5.0) # In case of stock collapse
  xmax <- max(c(2.0, 1.1*subset(dat, metric=="bbmsy" & level=="upper")$value, 1.1*subset(dat, metric=="bbmsy" & level=="median")$value), na.rm=TRUE)
  # Set up the axes
  plot(x=c(0,xmax), y=c(0,ymax), type="n", xlim=c(0,xmax), ylim=c(0,ymax), xlab = "B / BMSY", ylab = "F / FMSY", xaxs="i", yaxs="i")
  # Set the colour panels
  # The red one - top right
  rect(0.0,1.0,1.0,ymax, border="black", col="red", lty=1, lwd=1)
  # Bottom left - yellow
  rect(0.0, 0.0, 1.0, 1.0, border="black", col="yellow", lty=1, lwd=1)
  # Top right - yellow
  rect(1.0, 1.0, xmax, ymax, border="black", col="yellow", lty=1, lwd=1)
  # Bottom right - green
  rect(1.0, 0.0, xmax, 1.0, border="black", col="green", lty=1, lwd=1)
  # Loop over HCRs
  hcrs <- unique(dat$hcr)
  for (hcrcount in hcrs){
    hcrdat <- subset(dat, hcr==hcrcount)
    colour <- hcrdat$colour[1]
    # Add the medians as points and a line to tell the story
    medbbmsy <- subset(hcrdat, metric == "bbmsy" & level=="median")$value
    medffmsy <- subset(hcrdat, metric == "ffmsy" & level=="median")$value
    points(x=medbbmsy, y=medffmsy, pch=16, col=colour)
    # Do a thick black line then overlay the real line
    lines(x=medbbmsy, y=medffmsy, lty=1, lwd=4, col="black")
    lines(x=medbbmsy, y=medffmsy, lty=1, lwd=3, col=colour)
    # Add quantile lines
    lowerbbmsy <- subset(hcrdat, metric == "bbmsy" & level=="lower")$value
    lowerffmsy <- subset(hcrdat, metric == "ffmsy" & level=="lower")$value
    upperbbmsy <- subset(hcrdat, metric == "bbmsy" & level=="upper")$value
    upperffmsy <- subset(hcrdat, metric == "ffmsy" & level=="upper")$value
    # We always have one more B value so use FFMsy for final year
    finalyr <- max(which(!is.na(medffmsy))) 
    for (yr in 1:finalyr){
      lines(x=c(medbbmsy[yr],medbbmsy[yr]), y=c(lowerffmsy[yr],upperffmsy[yr]), lty=1, lwd=4, col="black")
      lines(x=c(medbbmsy[yr],medbbmsy[yr]), y=c(lowerffmsy[yr],upperffmsy[yr]), lty=1, lwd=3, col=colour)
      lines(x=c(lowerbbmsy[yr], upperbbmsy[yr]),y=c(medffmsy[yr], medffmsy[yr]), lty=1, lwd=4, col="black")
      lines(x=c(lowerbbmsy[yr], upperbbmsy[yr]),y=c(medffmsy[yr], medffmsy[yr]), lty=1, lwd=3, col=colour)
    }
    # Blob at the end
    finalbbmsy <- medbbmsy[finalyr]
    finalffmsy <- medffmsy[finalyr]
    points(x=finalbbmsy, y=finalffmsy, pch=21, col=colour, bg="white")
  }
  # Add legend
  # Adapt HCR name text
  # This is wrong - numbers not necessarily 1:X 
  #if (!("Current" %in% hcrs )){ # Match single stock name in above function - bit dodgy
  if(!any(is.na(dat$hcrlegend))){
    hcrnames <- unique(dat$hcrlegend)
    legend(x="bottomright", legend=hcrnames, lty=1, lwd=4, col=unique(dat$colour), cex=1.0)
  }
}



# Plot the majuro plot
# quantiles of length 2 - lower and upper
old_plot_majuro <- function(stock, stock_params, quantiles){
  # F / FMSY against
  # B / K
  # Plot medians and percentiles
  # Just final point?
  bk <- stock$biomass / stock_params$k
  fmsy <- stock_params$r / 2
  f <- stock$catch / stock$biomass 
  ffmsy <- f / fmsy
  # Get the medians and percentiles
  ffmsyqs <- get_quantiles(ffmsy, quantiles=quantiles)
  bkqs <- get_quantiles(bk, quantiles=quantiles)

  # Set up the axes
  ymax <- max(c(2.0, ffmsyqs * 1.1), na.rm=TRUE)
  plot(x=c(0,1), y=c(0,2), xlim=c(0,1), ylim=c(0,ymax), xlab = "SB / SBF=0", ylab = "F / FMSY")
  # And the colour panels
  # The big red one
  rect(0.0,0.0,stock_params$lrp,ymax, border="black", col="red", lty=1, lwd=1)
  # The orange one
  rect(stock_params$lrp,1.0,1.0,ymax, border="black", col="orange", lty=1, lwd=1)
  # The other one - leave white for now
  rect(stock_params$lrp,0.0,1.0,1.0, border="black", col="white", lty=1, lwd=1)

  # Add the medians as points and a line to tell the story
  points(x=bkqs[2,], y=ffmsyqs[2,], pch=16, col="black")
  lines(x=bkqs[2,], y=ffmsyqs[2,], lty=1, lwd=1)
  # And the percentiles as crosses
  for (yr in 1:dim(bkqs)[2]){
    if (!is.na(bkqs[1,yr]) & !is.na(bkqs[3,yr]) & !is.na(ffmsyqs[1,yr]) & !is.na(ffmsyqs[3,yr])){
      lines(x=c(bkqs[1,yr], bkqs[3,yr]), y=c(ffmsyqs[2,yr], ffmsyqs[2,yr]), lty=1, lwd=1)
      lines(x=c(bkqs[2,yr], bkqs[2,yr]), y=c(ffmsyqs[1,yr], ffmsyqs[3,yr]), lty=1, lwd=1)
    }
  }
  # Show the final year as a big yellow blob
  final_yr <- dim(bkqs)[2]
  points(x=bkqs[2,final_yr], y=ffmsyqs[2,final_yr], pch=16, cex=1.5, col="yellow")
}

# Generate vector of colours the same length as the total no of hcrs
# Then subset out the hcrs in hcr_choices
# see for more names: display.brewer.all(colorblindFriendly=TRUE)
# Check the max number of colours in the palette brewer.pal.info
# No HCRs is the total number of HCRs - not the number selected
get_hcr_colours <- function(hcr_names, chosen_hcr_names){
  #allcols <- colorRampPalette(brewer.pal(11,"PiYG"))(length(hcr_names))
  #allcols <- colorRampPalette(brewer.pal(12,"Paired"))(length(hcr_names))
  allcols <- colorRampPalette(RColorBrewer::brewer.pal(8,"Dark2"))(length(hcr_names))
  names(allcols) <- hcr_names
  hcrcols <- allcols[chosen_hcr_names]
  return(hcrcols)
}
#------------------------------------------------------------
# Yield curve

plot_yieldcurve_projections <- function(stock, stock_params, app_params){
  # x-axis = Effort
  # y-axis = Catch
  # In final year
  # Only plot this if running a long term projection?
  final_ts <- dim(stock$catch)[2]

  rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  #final_rel_effort <- rel_effort[,final_ts]
  final_rel_effort <- apply(rel_effort, 1, function(x)x[max(which(!is.na(x)))])

  #final_catch <- stock$catch[,final_ts]
  final_catch <- apply(stock$catch, 1, function(x)x[max(which(!is.na(x)))])


  # Bit of a mess to start with when we only have NAs
  #if (all(is.na(final_rel_effort))){
  #  xrange <- c(0, 1)
  #}
  #else {
    #xrange <- c(0,min(max(final_rel_effort, na.rm=TRUE)*1.1,5, na.rm=TRUE))
    xrange <- c(0,min(max(rel_effort, na.rm=TRUE)*1.1,5, na.rm=TRUE))
  #}
  #if (all(is.na(final_catch))){
  #  yrange <- c(0, 100)
  #}
  #else {
    yrange <- c(0, max(stock$catch, na.rm=TRUE) * 1.1)
  #}


  plot(x=xrange, y=yrange, type="n", xlab="Final relative fishing effort", ylab="Final catch", xlim=xrange, ylim=yrange)
  points(x=final_rel_effort, y=final_catch, pch=16, cex=3)
  # Draw last one in blue
  nproj <- nrow(stock$catch)
  points(x=final_rel_effort[nproj], y=final_catch[nproj], col="blue", pch=16, cex=3)

  # Add lines of full trajectories?
  # Looks too messy
  for (proj in 1:nrow(stock$catch)){
    lines(x=rel_effort[proj,], y=stock$catch[proj,], lty=3, col="black")
    points(x=rel_effort[proj,], y=stock$catch[proj,], col="black", cex=0.5)
  }
  # Draw last one in blue
  nproj <- nrow(stock$catch)
  lines(x=rel_effort[nproj,], y=stock$catch[nproj,], lty=3, col="blue")
  points(x=rel_effort[nproj,], y=stock$catch[nproj,], col="blue", cex=0.5)

}




#------------------------------------------------------------
# PI plots

# Importing a load of stuff to make the code a bit cleaner - adds to NAMESPACE
#' @importFrom ggplot2 "aes"
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "facet_wrap"
#' @importFrom ggplot2 "element_blank"
#' @importFrom ggplot2 "scale_fill_manual"
#' @importFrom ggplot2 "theme"
#' @importFrom ggplot2 "xlab"
#' @importFrom ggplot2 "ylab"
plot_pi_choice <- function(pis, hcr_choices, pi_choices, plot_choice){
  if (length(pis) == 0){
    return()
  }
  # Put into one big DF
  piqs <- lapply(pis, function(x) return(x$piqs))
  piqs <- dplyr::bind_rows(piqs, .id="hcr")

  # Sanity check - if all the hcr choices are not in the PI table then something has gone wrong
  if (!all(hcr_choices %in% unique(piqs$hcr))){
    stop("In plot_pi_choice(). Not all hcr_choices are in the hcr list.\n")
  }

  # Capitalise first letter of term column (add years?)
  piqs$term <- paste(toupper(substring(piqs$term, 1,1)), substring(piqs$term, 2), sep="") 
  # Reorder term column: Short, Medium, Long
  piqs$term <- as.factor(piqs$term)
  piqs$term <- factor(piqs$term,levels(piqs$term)[c(3,2,1)])

  # Add Legend name through the the HCR number
  hcrno <- unlist(lapply(strsplit(piqs$hcr, "\\."),"[",1))
  piqs$hcrlegend <- paste("HCR ", hcrno, sep="")
  # Get the colour palette - based on the hcr legend rather than the full name
  hcr_choices2 <- unlist(lapply(strsplit(hcr_choices, "\\."),"[",1))
  hcr_choices2 <- paste("HCR ", hcr_choices2, sep="")
  hcrcols <- get_hcr_colours(hcr_names=unique(piqs$hcrlegend), chosen_hcr_names=hcr_choices2)
  #hcrcols <- get_hcr_colours(hcr_names=unique(piqs$hcr), chosen_hcr_names=hcr_choices)

  # Drop out unwanted HCRs and PIs
  piqs <- subset(piqs, (hcr %in% hcr_choices) & (name %in% pi_choices))

  # Transform medians for upside down ones - not sure about this
  piqs[piqs$value < 1e-9, "value"] <- 1e-9
  piqs_normal <- subset(piqs, pi %in% c("bk", "problrp", "catch", "relcpue"))
  piqs_usdown <- subset(piqs, pi %in% c("diffcatch","diffeffort","diffcpue","releffort"))
  # Doesn't make sense to transform quantiles
  #piqs_usdown <- piqs_usdown %>% group_by(pi,term,name,quantiles) %>% mutate(scvalue = (value / max(value, na.rm=TRUE)))
  temp <-  dplyr::group_by(piqs_usdown, pi,term,name,quantiles)
  piqs_usdown <- dplyr::mutate(temp, scvalue = (value / max(value, na.rm=TRUE)))

  #piqs_usdown <- piqs_usdown %>% group_by(pi,term,name,quantiles) %>% mutate(value = (min(scvalue, na.rm=TRUE)/scvalue))
  temp <-  dplyr::group_by(piqs_usdown, pi,term,name,quantiles)
  piqs_usdown <- dplyr::mutate(temp, value = (min(scvalue, na.rm=TRUE)/scvalue))

  piqs_transform <- rbind(piqs_normal, piqs_usdown)
  # Rename
  piqs_transform[piqs_transform$pi == "diffcatch", "name"] <- "Relative catch stability"
  piqs_transform[piqs_transform$pi == "diffeffort", "name"] <- "Relative effort stability"
  piqs_transform[piqs_transform$pi == "diffcpue", "name"] <- "Relative CPUE stability"
  piqs_transform[piqs_transform$pi == "releffort", "name"] <- "Rescaled effort"

  # Plot the medians as a bar chart
  if (plot_choice == "PImeds"){
    # The plots - facet on name
    # Plot type one - bar chart on q50
    #dat <- subset(piqs, (quantiles=="q50") & (hcr %in% hcr_choices) & (name %in% pi_choices))
    #dat <- subset(piqs_transform, (quantiles=="q50"))
    dat <- subset(piqs, (quantiles=="q50"))
    p <- ggplot(dat, aes(x=term, y=value, fill=hcrlegend))
    p <- p + ggplot2::geom_bar(stat="identity", position="dodge", colour="black", width=0.7)
    p <- p + xlab("Time period")
    p <- p + scale_fill_manual(values=hcrcols)
    p <- p + facet_wrap(~name, scales="free")
    p <- p + theme(legend.position="bottom", legend.title=element_blank())
    #p <- p + ylim(0, NA) # If -ve variability
    return(p) 
  }
  else if (plot_choice == "PIbox"){
    #dat <- subset(piqs)
    dat <- piqs
    # Spread out the quantiles columns
    dat <- dat %>% spread(key="quantiles", value="value")
    p <- ggplot(dat, aes(x=term))
    p <- p + ggplot2::geom_boxplot(aes(ymin=q5, ymax=q95, lower=q20,middle=q50,upper=q80, fill=hcrlegend), stat="identity")
    p <- p + xlab("Time period")
    p <- p + scale_fill_manual(values=hcrcols)
    p <- p + theme(legend.position="bottom", legend.title=element_blank())
    p <- p + facet_wrap(~name, scales="free")
    #p <- p + ylim(0, NA) # Can be -ve variability
    return(p)
  }
  # What is this shit?
  else if (plot_choice == "PIradar"){
    # Want a DF with column of HCR, PI, value
    # Use rescaled medians - upside down
    dat <- subset(piqs_transform, (quantiles=="q50"))
    #dat <- subset(piqs, (quantiles=="q50"))
    # Rescale the median to the maximum
    # Be careful with 0 values, scaling by max can stuff it up if all PIs have 0
    dat[dat$value == 0,"value"] <- 1e-9
    #dat <- dat %>% group_by(term, pi, name) %>% mutate(scvalue = value / max(abs(value), na.rm=TRUE))
    dat <- dplyr::group_by(dat, term, pi, name)
    dat <- dplyr::mutate(dat, scvalue = value / max(abs(value), na.rm=TRUE))


    #browser()
    # Move variability PIs to + ve scale
    #dat[dat$pi %in% c("diffcatch", "diffeffort", "diffcpue"), "scvalue"] <- dat[dat$pi %in% c("diffcatch", "diffeffort", "diffcpue"), "scvalue"] + 1
    # Drop variability PIs as negative scale is a problem
    #dat <- dat[!(dat$pi %in% c("diffcatch", "diffeffort", "diffcpue")),]
    ## Also drop effort (also points the wrong way)
    #dat <- dat[!(dat$pi %in% c("releffort")),]

    # Order is important, rbinding above made a mess
    dat <- dat[order(dat$name),]
    # ggproto magic taken from:
    # From: http://web-r.org/board_ISVF22/8270 
    # See: http://web-r.org/board_ISVF22/8271 
    # inherits from CoordPolar and sets other arguments?
    # coord_polar on its own is a bit weird - I'm guessing that the is_linear argument is the new thing
    p <- ggplot(data=dat, aes(x=name, y=scvalue,fill=hcrlegend, group=hcrlegend, colour=hcrlegend))
    #p <- p + geom_point(size=3)
    p <- p + ggplot2::geom_polygon(alpha=0.6)
    p <- p + xlab("") + ylab("") + theme(legend.position="bottom", legend.title=element_blank())
    p <- p + facet_wrap(~term)
    p <- p + scale_fill_manual(values=hcrcols)
    p <- p + ggplot2::scale_colour_manual(values=hcrcols)
    p <- p + ggplot2::ggproto("CoordRadar", ggplot2::CoordPolar, theta = "x", r = "y", start = 0, direction = 1, is_linear = function(coord) TRUE)
    p <- p + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) # Remove y axis 
    p <- p + ggplot2::ylim(0,1)
    #p + coord_polar() # For a weird fat look!
    return(p)
  }
  else {
    return()
  }
}

# Importing a load of stuff to make the code a bit cleaner - adds to NAMESPACE
#' @importFrom ggplot2 "aes"
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "facet_wrap"
#' @importFrom ggplot2 "element_blank"
#' @importFrom ggplot2 "scale_fill_manual"
#' @importFrom ggplot2 "theme"
#' @importFrom ggplot2 "xlab"
#' @importFrom ggplot2 "ylab"
#' @importFrom ggplot2 "geom_line"
plot_timeseries <- function(timeseries, hcr_choices, stock_params, show_spaghetti=FALSE){
  if (length(timeseries) == 0){
    return()
  }
  # Put into one big DF
  timeseries <- dplyr::bind_rows(timeseries, .id="hcr")

  # Add Legend name through the the HCR number
  hcrno <- unlist(lapply(strsplit(timeseries$hcr, "\\."),"[",1))
  timeseries$hcrlegend <- paste("HCR ", hcrno, sep="")
  # Get the colour palette - based on the hcr legend rather than the full name
  hcr_choices2 <- unlist(lapply(strsplit(hcr_choices, "\\."),"[",1))
  hcr_choices2 <- paste("HCR ", hcr_choices2, sep="")
  hcrcols <- get_hcr_colours(hcr_names=unique(timeseries$hcrlegend), chosen_hcr_names=hcr_choices2)
  #hcrcols <- get_hcr_colours(hcr_names=unique(piqs$hcr), chosen_hcr_names=hcr_choices)
  # Get the colour palette
  #hcrcols <- get_hcr_colours(hcr_names=unique(timeseries$hcr), chosen_hcr_names=hcr_choices)

  qdat <- subset(timeseries, (type=="quantile") & (hcr %in% hcr_choices))
  qdat <- qdat %>% spread(key="level", value="value")
  # For adding the LRP line to the biomass plot
  bkdat <- subset(qdat, metric=="bk")
  bkdat$lrp <- stock_params$lrp
  bkdat$trp <- stock_params$trp
  # For adding the F/FMSY = 1 line
  ffmsydat <- subset(qdat, metric=="ffmsy")
  ffmsydat$ffmsyref <- 1.0


  p <- ggplot(qdat, aes(x=year))
  # Add ribbons
  p <- p + ggplot2::geom_ribbon(aes(ymin=lower, ymax=upper, fill=hcrlegend), alpha=0.6)
  # Add upper and lower lines
  p <- p + geom_line(aes(y=lower, colour=hcrlegend))
  p <- p + geom_line(aes(y=upper, colour=hcrlegend))
  # Add median line
  p <- p + geom_line(aes(y=median, colour=hcrlegend), linetype=2, size=1)

  # Add LRP and TRP
  p <- p + geom_line(data=bkdat, aes(y=lrp), colour="black", linetype=2, size=0.5)
  p <- p + geom_line(data=bkdat, aes(y=trp), colour="black", linetype=2, size=0.5)

  p <- p + facet_wrap(~name, scales="free")
  p <- p + xlab("Year") + theme(legend.position="bottom", legend.title=element_blank())
  p <- p + scale_fill_manual(values=hcrcols)
  p <- p + ggplot2::scale_colour_manual(values=hcrcols)
  p <- p + ggplot2::ylim(0, NA)

  # Add F/FMSY=1
  p <- p + geom_line(data=ffmsydat, aes(y=ffmsyref), colour="black", linetype=2, size=0.5)

  # Add spaghetti
  if (show_spaghetti==TRUE){
    spdat <- subset(timeseries, (type=="spaghetti") & (hcr %in% hcr_choices))
    for(ihcr in hcr_choices){
      p <- p + geom_line(data=subset(spdat, hcr == ihcr), aes(y=value, group=level, colour=hcrlegend), size=0.5)
    }
  }
  return(p)
}





