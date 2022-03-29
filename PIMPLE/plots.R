#' Default palette for HCRs
#' 
#' Get the default palette for the HCR colours
#' 
#' @param hcr_names The names of all of the HCRs
#' @param chosen_hcr_names The names of only the chosen HCRs
#' @noRd
#' @keywords internal
get_hcr_colours <- function(hcr_names, chosen_hcr_names){
  allcols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12,"Paired"))(length(hcr_names))
  names(allcols) <- hcr_names
  hcrcols <- allcols[as.character(chosen_hcr_names)] # subsetting on factors seems to have stuffed this up
  return(hcrcols)
}



#' barboxplot
#'
#' barboxplot() plots box plots or bar charts of the median values of the indicators for each HCR.
#'
#' @param dat The data frame of performance indicators.
#' @param hcr_choices The names of the chosen HCRs to plot.
#' @param plot_type Either median_bar or box.
#' @param no_cols Number of column in each row. Default is 2.
#' 
#' @return A ggplot2 plot object.
#' @importFrom ggplot2 "ggplot" "aes_string" "geom_bar" "theme" "element_blank" "xlab" "facet_wrap" "theme_bw" "geom_boxplot" "scale_fill_manual" "ylab" "element_text"
#' @noRd
#' @keywords internal
barboxplot <- function(dat, hcr_choices, plot_type="median_bar", quantiles=c(0.10, 0.25, 0.75, 0.90), no_cols=2){
  all_hcr_names <- unique(dat$hcrref)
  dat <- dat[dat$hcrref %in% hcr_choices,] # Trying to remove weird warning about hcr_no being a global variable
  hcr_cols <- get_hcr_colours(hcr_names=all_hcr_names, chosen_hcr_names=unique(dat$hcrref))
  if (plot_type=="median_bar"){
    p <- ggplot(dat, aes_string(x="period", y="X50.", fill="hcrref"))
    p <- p + geom_bar(stat="identity", position="dodge", colour="black", width=0.7)
    p <- p + ylab("Average value")
  }
  if (plot_type=="box"){
    quantiles <- sort(quantiles)
    #quantiles_text <- paste("value_", quantiles, sep="") 
    quantiles_text <- paste("X", quantiles * 100, ".", sep="") 
    ymin <- quantiles_text[1]
    ymax <- quantiles_text[4]
    lower <- quantiles_text[2]
    upper <- quantiles_text[3]
    p <- ggplot(dat, aes_string(x="period"))
    p <- p + geom_boxplot(aes_string(ymin=ymin, ymax=ymax, lower=lower, upper=upper, middle="X50.", fill="hcrref"), stat="identity")
  }
  p <- p + xlab("Time period")
  p <- p + scale_fill_manual(values=hcr_cols)
  p <- p + facet_wrap(~pi, scales="free", ncol=no_cols)
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", legend.title=element_blank())
  
  # Size of labels etc
  text_size <- 14
  p <- p + theme(axis.text=element_text(size=text_size), axis.title=element_text(size=text_size), strip.text=element_text(size=text_size), legend.text=element_text(size=text_size))
  return(p)
}

#' time_series_plot() plots times series of indicators for each HCR.
#'
#' time_series_plot() plots times series of indicators for each HCR.
#'
#' @param dat The data.
#' @param hcr_choices Vector of selected HCR names.
#' @param wormdat Data set of the spaghetti (worms).
#' @param alpha20_80 Alpha of the ribbons.
#' @param linetype_worm Line type of the spaghetti.
#' @param percentile_range The minimum and maximum range of the ribbon.
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
#' @export
time_series_plot <- function(dat, hcr_choices, wormdat=NULL,
                          alpha20_80 = 1.0, linetype_worm=1,
                          percentile_range = c(10,90),
                          colour_worm="black",
                          size_worm=0.3, add_start_line=TRUE, time_period_lines=TRUE,
                          short_term = 2016:2024, medium_term = 2025:2033, long_term = 2034:2042,
                          last_plot_year=2042, show_spaghetti=FALSE){
  # Sort out colours based on chosen HCRs
  all_hcr_names <- unique(dat$hcrref)
  dat <- dat[dat$hcrref %in% hcr_choices,] # Trying to remove weird warning about hcr_no being a global variable
  hcr_cols <- get_hcr_colours(hcr_names=all_hcr_names, chosen_hcr_names=unique(dat$hcrref))
  # Rename for percentiles reference
  dat <- dplyr::rename(dat, "min" = paste("X",percentile_range[1],".",sep=""), "max" = paste("X",percentile_range[2],".",sep="")) 
  # Chop out NA rows
  dat <- dat[!is.na(dat$X50.),]
  # Start the plot
  p <- ggplot(dat, aes(x=year))
  # The ribbon
  p <- p + geom_ribbon(aes(ymin=min, ymax=max, fill=hcrref), alpha=alpha20_80)
  # The outside lines - needed?
  p <- p + geom_line(aes(y=max, group=hcrref), colour="black", size=ggplot2::rel(0.5))
  p <- p + geom_line(aes(y=min, group=hcrref), colour="black", size=ggplot2::rel(0.5))
  # Add median line
  p <- p + geom_line(aes(y=X50., group=hcrref), colour="black", linetype=2, size=ggplot2::rel(0.5))
  p <- p + xlab("Year")
  # Add worms
  # These look bad - hard to see the colours
  if (!is.null(wormdat) & show_spaghetti==TRUE){
    wormdat <- subset(wormdat, hcrref %in% hcr_choices)
    wormdat <- wormdat[!is.na(wormdat$data),]
    # Put a black background on the line to help
    p <- p + geom_line(data=wormdat, aes(x=year, y=data, group=wormid), colour="black", linetype=linetype_worm, size=size_worm*1.2)
    #p <- p + geom_line(data=wormdat, aes(x=year, y=data, group=wormid, colour=hcrref), linetype=linetype_worm, size=size_worm)
  }
  # Colours
  p <- p + scale_fill_manual(values=hcr_cols)
  p <- p + scale_colour_manual(values=hcr_cols)
  # Add vertical line at start of MSE
  if (add_start_line){
    p <- p + geom_vline(aes(xintercept=min(short_term)-3), linetype=2)
  }
  # Time period lines?
  if(time_period_lines){
    p <- p + geom_vline(aes(xintercept=min(short_term)), linetype=2)
    p <- p + geom_vline(aes(xintercept=min(medium_term)), linetype=2)
    p <- p + geom_vline(aes(xintercept=min(long_term)), linetype=2)
  }
  
  # Faceting by PI - for comparing lots of different metrics - add later
  #p <- p + facet_wrap(~piname, scales="free", ncol=1)
  p <- p + facet_grid(piname ~ hcrref, scales="free")#, ncol=1)
  p <- p + theme_bw()
  
  p <- p + theme(legend.position="bottom", legend.title=element_blank())
  
  return(p)
}


