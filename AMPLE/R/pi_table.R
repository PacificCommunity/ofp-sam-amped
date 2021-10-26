# Performance indicator table for multiple HCRs
# pi_table.R

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Soundtrack: Spice Doubt by Ozric Tentacles
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' Performance indicator table for multiple HCRs
#' 
#' Returns a neatly formatted table of the performance indicators for multiple HCRs.
#' The \code{pis} argument should already be subsetted for desired PIs, HCRs, and time periods.
#' Used in the Comparing Performance app.
#' @param pis A data.frame of quantiles of performance indicators for each HCR.
#' @param quantiles The quantile values to form the uncertainty part of the table (need to already exist in the \code{pis} data.frame).
#' @param signif The number of significant digits to use in the table.
#' @noRd
#' @keywords internal
pi_table_all_hcrs <- function(pis, quantiles = c(0.05, 0.95), signif=2){
  quantile_cols <- paste0("value_", sort(quantiles))
  pis$value <- paste(signif(pis$value_0.5, signif), " (", signif(pis[,quantile_cols[1]], signif), ",", signif(pis[,quantile_cols[2]], 2), ")", sep="")
  # Fix Prob. PIs - no uncertainty
  prob_pis <- grepl("Prob", pis$pi)
  pis$value[prob_pis] <- as.character(signif(pis$value_0.5),2)[prob_pis]
  dat <- pis[,c("hcr_ref", "pi", "value")]
  dat <- reshape(data = dat, direction = "wide", timevar = "pi", idvar = c("hcr_ref"), v.names = "value", sep="_")
  # Col names
  new_colnames <- substring(colnames(dat)[-1], first = nchar("value_") + 1)
  colnames(dat)[-1] <- new_colnames
  colnames(dat)[1] <- "HCR"
  return(dat) 
}
