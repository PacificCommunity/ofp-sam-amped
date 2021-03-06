# Common plotting routines for AMPED and PIMPLE
# 
# Copyright 2019 Finlay Scott. Distributed under the GPL 3 or later
# Maintainer: Finlay Scott, SPC

# A metric fuckton of imports
#' @importFrom graphics "plot" "polygon" "lines"
#' @importFrom ggplot2 "geom_rect" "scale_x_continuous" "scale_y_continuous" "geom_point" "geom_errorbar" "geom_errorbarh" "geom_ribbon" "geom_line" "geom_vline" "scale_x_continuous" "aes" "ggplot" "geom_bar" "geom_boxplot" "facet_wrap" "element_blank" "element_text" "scale_fill_manual" "theme" "xlab" "ylab" "coord_polar" "ggproto" "scale_colour_manual" "geom_polygon" "ylim" "xlim" "geom_text"
# Plotting functions
# Globals - eesh!

# see cocktailApp() about use of globalVariables()
# Maybe put them all as part of the pkg_env?
# Or some kind of theme?
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


#-------------------------------------------------------------------------------------
# Generate vector of colours the same length as the total no of hcrs
# Then subset out the hcrs in hcr_choices
# see for more names: display.brewer.all(colorblindFriendly=TRUE)
# Check the max number of colours in the palette brewer.pal.info
# No HCRs is the total number of HCRs - not the number selected
#' Default palette for HCRs
#' 
#' Get the default palette for the HCR colours
#' 
#' @param hcr_names The names of all of the HCRs
#' @param chosen_hcr_names The names of only the chosen HCRs
#' @export
get_hcr_colours <- function(hcr_names, chosen_hcr_names){
  allcols <- colorRampPalette(RColorBrewer::brewer.pal(12,"Paired"))(length(hcr_names))
  names(allcols) <- hcr_names
  hcrcols <- allcols[chosen_hcr_names]
  return(hcrcols)
}
#-------------------------------------------------------------------------------------
# Front page plots for the AMPED apps - not used by PIMPLE

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

# Advice from CRAN submission about how to deal with resetting par()
# opar <- par(mfrow = c(2,2))
# on.exit(par(opar))
sideways_histogram <- function(dat, range, lhist=20, num.dnorm=5*lhist, dcol="blue"){
    #def.par <- par(no.readonly = TRUE) # as seen in layout doc
    yhist <- hist(dat, plot=FALSE, breaks=seq(from=range[1], to=range[2], length.out=lhist)) 
    # Use dnorm
    yx <- seq(range[1], range[2], length.out=num.dnorm)
    yy <- dnorm(yx, mean=mean(dat), sd=sd(dat))
    yy[is.infinite(yy)] <- 1.0
    parmar <- par()$mar
    opar <- par(mar=c(parmar[1], 0, parmar[3], 0))
    on.exit(par(opar))
    barplot(yhist$density, axes=FALSE, xlim=c(0, max(yhist$density, yy)), space=0, horiz=TRUE, xaxs="i", yaxs="i") # barplot
    lines(yy, seq(from=0, to=lhist-1, length.out=num.dnorm), col=dcol) # line
    # restore parameters
    #par(def.par)  #- reset to default - as seen in layout man pages
}

# Draw the HCR (output vs input)
# Input can be SB/SBF=0, or some output from empirical MP
# Try to keep it flexible
# type - are we stepping or projecting? Better way of doing this

#' Plots for the front page of the Introduction to HCRs and other AMPED apps.
#'
#' plot_hcr() plots the shape of the HCR.
#'
#' @param stock A list with elements biomass, hcr_ip, hcr_op, effort and catch.
#' @param stock_params A vector of life history and stochasticy parameters.
#' @param mp_params A vector of management procedure parameters.
#' @param app_params A vector of application parameters.
#' @param timestep The current timestep (optional).
#' @param show_last Show the previous iters as ghosts (optional).
#' @param percentile_range A vector of length with minimum and maximum percentile range to plot.
#' 
#' @return A plot
#' @rdname front_page_plots
#' @name Front page plots
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
#'   biol_est_sigma = 0.2,
#'   biol_prod_sigma = 0.2, 
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
#'   app_params = app_params, initial_biomass = stock_params$b0, nyears = 20, niters = 100)
#' # Finally project over the timesteps
#' stock <- project(stock, timesteps = c(11,20), stock_params = stock_params,
#'   mp_params = mp_params, app_params = app_params)
#' 
#' # The plots
#' # Plot the HCR
#' plot_hcr(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params)
#' # Plot biomass timeseries
#' plot_biomass(stock=stock, stock_params=stock_params, mp_params=mp_params, quantiles=c(0.2,0.8))
#' # Plot catch timeseries
#' plot_catch(stock=stock, stock_params=stock_params, mp_params=mp_params, quantiles=c(0.2,0.8))
#' # Plot the projection (biomass, catch and rel cpue)
#' # The arrow connecting the HCR to the biomass
#' plot_hcr_intro_arrow(stock=stock, timestep=15)
#' # Time series with a histogram on the end
#' plot_metric_with_histo(stock=stock, stock_params=stock_params, mp_params=mp_params,
#'   metric="catch", app_params=app_params)
#' # Kobe or Majuro plot
#' # Just a few iters for efficiency
#' stock2 <- lapply(stock, '[',1:5,)
#' # Yield curve
#' plot_yieldcurve_projections(stock=stock2, stock_params=stock_params, app_params=app_params)
#' @export
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
  plot(x=xrange,y=yrange,type="n",xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", main="The HCR", ...) 

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

#' plot_biomass
#'
#' plot_biomass() plots time series of 'true' and observed depletion (SB/SBF=0).
#'
#' @param max_spaghetti_iters The number of iterations to show as spaghetti before ribbons are shown.
#' @param quantiles Quantiles of the ribbons.
#' @param nspaghetti The number of spaghetti iterations to plot on top of the ribbons.
#' @param add_grid Add a grid.
#' @param xlab The x-label.
#' @param ghost_col Colours of the ghost iterations.
#' @param last_col Colours of the last iteration.
#' @param ylim Y limits
#' @param ... Other arguments to pass to the plot() function.
#' 
#' @return A plot
#' @rdname front_page_plots
#' @name Front page plots
#' @export
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
#' plot_catch
#'
#' plot_catch() plots time series of catches.
#'
#' @param true_col Colour of the current iteration.
#' 
#' @return A plot
#' @rdname front_page_plots
#' @name Front page plots
#' @export
# Used for IntroHCR and IntroIndicators and maybe others
plot_catch <- function(stock, stock_params, mp_params, app_params=NULL, timestep=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, xlab="Year", ghost_col="grey", true_col="black", ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  # Set Ylim - use same as HCR plot
  ymax <- get_catch_ymax(stock$catch, mp_params)
  yrange <- c(0, ymax)
  # Empty axis
  plot(x=years, y=years, type="n", ylim=c(0, ymax), ylab="Catch", xlab=xlab, xaxs="i", yaxs="i", ...)
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
        # If HCR OP is total catch
        next_catch <- stock$hcr_op[last_iter, timestep+1]
        # If HCR OP is catch multiplier
        if (mp_params$output_type == "catch multiplier"){
          next_catch <- stock$catch[last_iter,timestep] * stock$hcr_op[,timestep+1]
          next_catch[next_catch < 10] <- 10 # A minimum catch
        }
        lines(x=c(years[1], years[length(years)]), y=rep(next_catch,2),lty=guide_lty, col=guide_col, lwd=guide_lwd) 
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
plot_relcpue <- function(stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, ymax=NA, ...){
  
  years <- as.numeric(dimnames(stock$biomass)$year)
  cpue <- stock$catch / stock$effort
  rel_cpue <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
  if(is.na(ymax)){
    ymax <- max(c(rel_cpue * 1.1, 1.0), na.rm=TRUE)
  }
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



# Could combine with function above into a single 
# quantiles of length 2: lower and upper
#' plot_metric_with_histo
#'
#' plot_metric_with_histo() is the generic plotting routine for plotting time series of metrics with histograms of the final time step. 
#'
#' @param metric The name of the metric to plot (catch, biomass or relcpue).
#' 
#' @return A plot
#' @rdname front_page_plots
#' @name Front page plots
#' @export
plot_metric_with_histo <- function(stock, stock_params, mp_params, metric, app_params=NULL, show_last=TRUE, percentile_range = c(20,80), ...){
  def.par <- par(no.readonly = TRUE) # as seen in layout doc
  on.exit(par(def.par))
  # Plot the metric with an extra sideways histogram
  layout(matrix(c(1,2), ncol=2), widths=c(6/7, 1/7))
  ospc <- 0.5 # outer space
  pext <- 4 # par extension down and to the left
  bspc <- 1 # space between scatter plot and bar plots
  par(mar=c(pext, pext, bspc, bspc), oma=rep(ospc, 4)) # plot parameters
  if (metric == "biomass"){
    # The timeseries of biomass in the big window
    plot_biomass(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=percentile_range / 100, ...)
    # The histogram should be the true B/K
    final_yr <- dim(stock$biomass)[2]
    dat <- stock$biomass[,final_yr] / stock_params$k
    range <- c(0,1)
  }
  else if (metric == "catch"){
    # The timeseries of biomass in the big window
    plot_catch(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=percentile_range / 100, ...)
    final_yr <- dim(stock$catch)[2]
    dat <- stock$catch[,final_yr]
    range <- c(0, get_catch_ymax(stock$catch, mp_params))
  }
  else if (metric == "relcpue"){
    cpue <- stock$catch / stock$effort
    rel_cpue <- sweep(cpue, 1, cpue[,app_params$last_historical_timestep], "/")
    # Set a maximum value for rel_cpue - just for plotting purposes
    max_rel_cpue <- 2
    rel_cpue[rel_cpue > max_rel_cpue] <- max_rel_cpue
    ymax <- max(c(rel_cpue * 1.1, 1.0), na.rm=TRUE)
    plot_relcpue(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, quantiles=percentile_range / 100, ymax=ymax, ...)
    # data for the histogram
    final_yr <- dim(rel_cpue)[2]
    dat <- rel_cpue[,final_yr]
    range <- c(0, ymax) # Matches the ymax in the plot_relcpue function
  }
  else {
    stop("In plot_metric_with_histo(). Unknown metric\n")
  }
  # Adapted from: https://stackoverflow.com/questions/11022675/rotate-histogram-in-r-or-overlay-a-density-in-a-barplot
  if((nrow(stock$hcr_ip) > 1) & (!all(is.na(dat)))){
    # Get all iters and right timesteps (including timelag)
    sideways_histogram(dat=dat, range=range)
  }
  # restore parameters
  #par(def.par)  #- reset to default - as seen in layout man pages
}


#----------------------------------------------------------------
# Comparison plotting routines
# For Measuring Performance and PIMPLE
# Originally held in the PIMPLE app (except the Majuro)

# Can we get Kobe in here too - just change the background colour and the X data
# SB/SBF=0, B/BMSY

# Quantile plot - for time series
# All HCRs on same plot
# Why sometimes worms and sometime spaghetti?

#' Plots for comparing HCR performance
#'
#' quantile_plot() plots times series of indicators for each HCR.
#'
#' @param wormdat Data set of the spaghetti (worms).
#' @param alpha20_80 Alpha of the ribbons.
#' @param linetype_worm Line type of the spaghetti.
#' @param colour_worm Colour of the spaghetti.
#' @param size_worm Thickness of the spaghetti.
#' @param add_start_line Add a line to the start of the projection (TRUE / FALSE).
#' @param time_period_lines Add a lines to show the time periods (TRUE / FALSE).
#' @param short_term Year range for the short-term.
#' @param medium_term Year range for the medium-term.
#' @param long_term Year range for the long-term.
#' @param last_plot_year Last year to plot.
#' @param show_spaghetti Show the spaghetti (worms) (TRUE / FALSE).
#' 
#' @return A ggplot2 plot object.
#' @rdname comparison_plots
#' @name Comparison plots
#' @examples
#'
#'
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
#'   biol_est_sigma = 0.2,
#'   biol_prod_sigma = 0.2, 
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
#'   app_params = app_params, initial_biomass = stock_params$b0, nyears = 40, niters = 10)
#' # Finally project over the timesteps
#' stock <- project(stock, timesteps = c(11,40), stock_params = stock_params,
#'   mp_params = mp_params, app_params = app_params)
#' # Get the summaries
#' pisums <- get_summaries(stock=stock, stock_params=stock_params, app_params=app_params,
#'   quantiles=c(0.01,0.05,0.20,0.5,0.80,0.95,0.99))
#' # Add an HCR name (done inside Shiny app)
#' pisums$worms$hcrref <- "HCR 1"
#' pisums$yearqs$hcrref <- "HCR 1"
#' pisums$periodqs$hcrref <- "HCR 1"
#'
#' # Time series quantile plot
#' quantile_plot(dat=pisums$yearqs, hcr_choices="HCR 1", wormdat=pisums$worms)
#' # Bar and box plots
#' myboxplot(dat=pisums$periodqs, hcr_choices="HCR 1", plot_type="median_bar")
#' myboxplot(dat=pisums$periodqs, hcr_choices="HCR 1", plot_type="box")
#' # Radar plot
#' myradar(dat=pisums$periodqs, hcr_choices="HCR 1", scaling="scale", polysize=2)
#' # Table of PIs. Only pass in 1 time period
#' pitable(dat=subset(pisums$periodqs, period=="Long"))
#' @export
quantile_plot <- function(dat, hcr_choices, wormdat=NULL,
                          alpha20_80 = 0.6, linetype_worm=1,
                          percentile_range = c(20,80),
                          colour_worm="black",
                          size_worm=0.3, add_start_line=TRUE, time_period_lines=TRUE, short_term = 2016:2024, medium_term = 2025:2033, long_term = 2034:2042, last_plot_year=2042, show_spaghetti=FALSE){
  # Sort out colours based on chosen HCRs
  hcrcols <- get_hcr_colours(hcr_names=unique(dat$hcrref), chosen_hcr_names=hcr_choices)
  # Select the chosen HCRs only - could do this in the call to plot in app?
  dat <- subset(dat, hcrref %in% hcr_choices)
  # Rename for percentiles reference
  dat <- dplyr::rename(dat, "min" = paste("X",percentile_range[1],".",sep=""), "max" = paste("X",percentile_range[2],".",sep="")) 
  # Chop out NA rows
  dat <- dat[!is.na(dat$X50.),]
  # Start the plot
  p <- ggplot(dat, aes(x=year))
  # Ribbons and lines
  p <- p + geom_ribbon(aes(ymin=min, ymax=max, fill=hcrref), alpha=alpha20_80)
  p <- p + geom_line(aes(y=max, group=hcrref), colour="black", size=ggplot2::rel(0.5))
  p <- p + geom_line(aes(y=min, group=hcrref), colour="black", size=ggplot2::rel(0.5))
  # Add median line
  #p <- p + geom_line(aes(y=X50., group=hcrref), colour="black", linetype=2, size=1)
  p <- p + geom_line(aes(y=X50., group=hcrref), colour="black", linetype=2, size=ggplot2::rel(0.5))

  # Plotting options
  p <- p + xlab("Year")
  # Add worms
  if (!is.null(wormdat) & show_spaghetti==TRUE){
    wormdat <- subset(wormdat, hcrref %in% hcr_choices)
    wormdat <- wormdat[!is.na(wormdat$data),]
    p <- p + geom_line(data=wormdat, aes(x=year, y=data, group=wormid), colour="black", linetype=linetype_worm, size=size_worm*1.1)
    p <- p + geom_line(data=wormdat, aes(x=year, y=data, group=wormid, colour=hcrref), linetype=linetype_worm, size=size_worm)
  }
  ## Colours
  p <- p + scale_fill_manual(values=hcrcols)
  p <- p + scale_colour_manual(values=hcrcols)
  # Add vertical line at start of MSE
  if (add_start_line){
    p <- p + geom_vline(aes(xintercept=min(short_term)-1), linetype=2)
  }
  # Time period lines?
  if(time_period_lines){
    p <- p + geom_vline(aes(xintercept=max(short_term)), linetype=2)
    p <- p + geom_vline(aes(xintercept=max(medium_term)), linetype=2)
    p <- p + geom_vline(aes(xintercept=max(long_term)), linetype=2)
  }

  # Faceting by PI - for comparing lots of different metrics - add later
  p <- p + facet_wrap(~piname, scales="free", ncol=1)

  # Size of labels etc
  p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
  p <- p + theme(legend.position="bottom", legend.title=element_blank())

  return(p)
}

# Bar plots and box plots

#' myboxplot
#'
#' myboxplot() plots box plots or bar charts of the median values of the indicators for each HCR.
#'
#' @param plot_type Either median_bar or box.
#' 
#' @return A ggplot2 plot object.
#' @rdname comparison_plots
#' @name Comparison plots
#' @export
myboxplot <- function(dat, hcr_choices, plot_type="median_bar"){
  hcrcols <- get_hcr_colours(hcr_names=unique(dat$hcrref), chosen_hcr_names=hcr_choices)
  # Fucking subset and fucking non standard evaluation - why is this so shit??????
  dat <- subset(dat, hcrref %in% hcr_choices)
  if (plot_type=="median_bar"){
    p <- ggplot(dat, aes(x=period, y=X50., fill=hcrref))
    p <- p + geom_bar(stat="identity", position="dodge", colour="black", width=0.7)
    p <- p + scale_fill_manual(values=hcrcols)
    p <- p + theme(legend.position="bottom", legend.title=element_blank())
    p <- p + xlab("Time period")
  }

  if (plot_type=="box"){
    p <- ggplot(dat, aes(x=period))
    p <- p + geom_boxplot(aes(ymin=X5., ymax=X95., lower=X20., middle=X50., upper=X80., fill=hcrref), stat="identity")
    p <- p + xlab("Time period")
    p <- p + scale_fill_manual(values=hcrcols)
    p <- p + theme(legend.position="bottom", legend.title=element_blank())
  }
  p <- p + facet_wrap(~piname, scales="free")

  # Size of labels etc
  p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
  return(p)
}

# Radar plot

#' myradar
#'
#' myradar() plots a radar chart in each time period, scaled either by the maximum or by ranking.
#'
#' @param scaling Either scale or rank.
#' @param polysize The thickness of the radar.
#' 
#' @return A ggplot2 plot object.
#' @rdname comparison_plots
#' @name Comparison plots
#' @export
# Updated radar plot - also add to AMPLE when fixed
myradar <- function(dat, hcr_choices, scaling="scale", polysize=2, textsize=5){
    hcrcols <- get_hcr_colours(hcr_names=unique(dat$hcrref), chosen_hcr_names=hcr_choices)
    dat <- subset(dat, hcrref %in% hcr_choices)
    # Scale by maximum - so max = 1
    if (scaling=="scale"){
      #dat <- dat %>% group_by(period, pi) %>% mutate(value = X50. / max(X50.))
      dat <- dplyr::group_by(dat, period, pi)
      dat <- dplyr::mutate(dat, value = X50. / max(X50.))
    }
    # Rank PI
    if (scaling=="rank"){
      #dat <- dat %>% group_by(period, pi) %>% mutate(value = order(X50.) / length(hcr_choices))
      dat <- dplyr::group_by(dat, period, pi)
      dat <- dplyr::mutate(dat, value = order(X50.) / length(hcr_choices))
    }
    # Need to wrap text of piname
    max_len <- 15 # max length of label in characters
    dat$piname_wrap <- sapply(dat$piname, function(y) paste(strwrap(y, max_len), collapse = "\n"), USE.NAMES = FALSE)
    dat <- dat[order(dat$piname_wrap),]
    p <- ggplot(data=dat, aes(x=piname_wrap, y=value,group=hcrref))
    p <- p + geom_polygon(aes(fill=hcrref), colour="black", alpha=0.6, size=polysize)
    p <- p + xlab("") + ylab("") + theme(legend.position="bottom", legend.title=element_blank())
    p <- p + scale_fill_manual(values=hcrcols)
    p <- p + scale_colour_manual(values=hcrcols)
    p <- p + coord_polar(theta = "x", start = 0, direction = 1, clip = "on")  # clip = "on"
    p <- p + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) # Remove y axis 
    p <- p + theme(axis.text.x=element_blank()) # Remove x axis 
    p <- p + ylim(0,1.5)
    p <- p + facet_wrap(~period)
    p <- p + geom_text(aes(y = 1.4,label = piname_wrap), size=textsize) # Hand write the labels
    p <- p + theme(strip.text=element_text(size=16), legend.text=element_text(size=16))
    return(p)
}




#-------------------------------------------------------------------------------------
# PIMPLE HCR plots

# Prep all the data in the PI calculation script

#' hcr_plot
#'
#' hcrplot() plots the shape of each HCR.
#'
#' @param hcr_shape The shape parameters of each HCR,
#' @param hcr_points Optionally show the bits of the HCR that triggered (currently not used).
#' @param lrp The limit reference point.
#' @param trp The target reference point.
#' @param blacklinesize Size of the underlying black lines.
#' @param linesize Size of the coloured lines.
#' @param pointsize Size of the points (currently not used).
#' @param stroke Mmmmm, stroking.
#' 
#' @return A ggplot2 plot object.
#' @rdname comparison_plots
#' @name Comparison plots
#' @export
hcr_plot <- function(hcr_choices, hcr_shape, hcr_points, lrp, trp, add_points=FALSE, add_path=FALSE, blacklinesize=4, linesize=3, pointsize=4.2, stroke=3){
    hcrcols <- get_hcr_colours(hcr_names=unique(hcr_shape$hcrref), chosen_hcr_names=hcr_choices)
    # Select the chosen HCRs only - could do this in the call to plot in app?
    shapedat <- subset(hcr_shape, hcrref %in% hcr_choices)
    pointsdat <- subset(hcr_points, hcrref %in% hcr_choices)
    p <- ggplot(shapedat, aes(x=x, y=y))
    p <- p + geom_line(aes(group=hcrref), colour="black", size=blacklinesize) # outline
    p <- p + geom_line(aes(colour=hcrref), size=linesize)
    p <- p + xlab("Estimated SB/SBF=0") + ylab("Effort multiplier")
    p <- p + theme(legend.position="bottom", legend.title=element_blank())
    p <- p + scale_fill_manual(values=hcrcols)
    p <- p + scale_colour_manual(values=hcrcols)
    p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    p <- p + scale_x_continuous(expand = c(0, 0))
    # Add points to it
    if (add_points){
      #p <- p + geom_point(dat=pointsdat, aes(x=sbsbf0, y=scaler, fill=hcrref), colour="black", shape=21, size=pointsize, alpha=0.3, stroke=stroke)
      p <- p + geom_jitter(dat=pointsdat, aes(x=sbsbf0, y=scaler, fill=hcrref), width=0.00, height=0.01, colour="black", shape=21, size=pointsize, stroke=stroke)
    }
    if (add_path){
      # Connect the iters by lines so you can see what happens?
      p <- p + geom_path(dat=pointsdat, aes(x=sbsbf0, y=scaler, group=interaction(iter, hcrref)), colour="black")
    }
    # Add LRP and TRP
    p <- p + geom_vline(aes(xintercept=lrp), linetype=2)
    p <- p + geom_vline(aes(xintercept=trp), linetype=2)
    p <- p + ylim(0, NA)
    return(p)
}

# Histograms for HCRs

#' hcr_histo_plot
#'
#' hcr_histo_plot() plots histograms of the HCR outputs in each time period.
#'
#' @param histodat Data for the histograms.
#' 
#' @return A ggplot2 plot object.
#' @rdname comparison_plots
#' @name Comparison plots
#' @export
hcr_histo_plot <- function(hcr_choices, histodat){
  hcrcols <- get_hcr_colours(hcr_names=unique(histodat$hcrref), chosen_hcr_names=hcr_choices)
  hdat <- subset(histodat, hcrref %in% hcr_choices)
  p <- ggplot(hdat, aes(x=bin, y=prop))
  p <- p + coord_flip()
  #p <- ggplot(hdat, aes(y=bin, x=prop))
  p <- p + geom_bar(aes(fill=hcrref), stat='identity', position='identity',colour="black", alpha=0.7)
  p <- p + facet_wrap(~period)
  p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
  p <- p + xlab("Effort scaler") + ylab("Proportion")
  p <- p + theme(legend.position="bottom", legend.title=element_blank())
  p <- p + ylim(0, NA)
  p <- p + xlim(0, NA)
  p <- p + scale_fill_manual(values=hcrcols)
  return(p)
}





