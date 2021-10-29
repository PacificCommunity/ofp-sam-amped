# Man page for AMPLE
# ample.R

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' AMPLE: A package of Shiny apps that introduce Harvest Control Rules (HCR) for fisheries management.
#'
#' AMPLE provides three Shiny apps that introduce Harvest Control Rules (HCR) for fisheries management.
#'    'Introduction to HCRs' provides a simple overview to how HCRs work. Users are able to select their own HCR and
#'    step through its performance, year by year. Biological variability and estimation uncertainty are introduced.
#'    'Introduction to indicators' builds on the previous app and introduces the idea of using performance indicators
#'    to evaluate HCR performance.
#'    'Comparing performance' allows multiple HCRs to be created and tested, and their performance compared so that the
#'    preferred HCR can be selected.
#' 
#' @section AMPLE functions:
#' Too launch the apps use the functions: \code{intro_hcr()}, \code{measuring_performance()} and \code{comparing_performance()}.
#' 
#' @section Acknowledgement:
#' With thanks to Andre Punt, and Winston Chang (for help with the R6 / Shiny reactivity).
#'
#' @docType package
#' @name ample
NULL
#> NULL