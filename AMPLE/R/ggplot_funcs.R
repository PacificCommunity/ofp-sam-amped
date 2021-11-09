# Plotting functions that use the ggplot2 package.
# Used in the Comparing Performance app.
# Note that none of these are exported and no man pages are written.
# They are documented for internal purposes only.
# ggplot_funcs.R

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: Hail Be You Sovereigns, Lief and Dear: Dark Britannica III by Various
# Distributed under the terms of the GNU General Public License GPL (>= 3)

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
  hcrcols <- allcols[chosen_hcr_names]
  return(hcrcols)
}



#' barboxplot
#'
#' barboxplot() plots box plots or bar charts of the median values of the indicators for each HCR.
#'
#' @param dat The data frame of performance indicators.
#' @param hcr_nos The numbers of the HCRs to plot.
#' @param plot_type Either median_bar or box.
#' @param no_cols Number of column in each row. Default is 2.
#' 
#' @return A ggplot2 plot object.
#' @importFrom ggplot2 "ggplot" "aes_string" "geom_bar" "theme" "element_blank" "xlab" "facet_wrap" "theme_bw" "geom_boxplot" "scale_fill_manual" "ylab" "element_text"
#' @noRd
#' @keywords internal
barboxplot <- function(dat, hcr_nos, plot_type="median_bar", quantiles=c(0.05, 0.10, 0.90, 0.95), no_cols=2){
  all_hcr_names <- unique(dat$hcr_ref)
  #dat <- subset(dat, hcr_no %in% hcr_nos)
  dat <- dat[dat$hcr_no %in% hcr_nos,] # Trying to remove weird warning about hcr_no being a global variable
  hcr_cols <- get_hcr_colours(hcr_names=all_hcr_names, chosen_hcr_names=unique(dat$hcr_ref))
  
  
  if (plot_type=="median_bar"){
    p <- ggplot(dat, aes_string(x="time_period", y="value_0.5", fill="hcr_ref"))
    p <- p + geom_bar(stat="identity", position="dodge", colour="black", width=0.7)
    p <- p + ylab("Average value")
  }
  if (plot_type=="box"){
    quantiles <- sort(quantiles)
    quantiles_text <- paste("value_", quantiles, sep="") 
    ymin <- quantiles_text[1]
    ymax <- quantiles_text[4]
    lower <- quantiles_text[2]
    upper <- quantiles_text[3]
    p <- ggplot(dat, aes_string(x="time_period"))
    p <- p + geom_boxplot(aes_string(ymin=ymin, ymax=ymax, lower=lower, upper=upper, middle="value_0.5", fill="hcr_ref"), stat="identity")
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


