# All the sidebars have the same width and SPC logo
# ui_funcs.R 

# Author: Finlay Scott (SPC) <finlays@spc.int>
# Sountrack: Disintegration Dubs by G36 vs JK Flesh
# Distributed under the terms of the GNU General Public License GPL (>= 3)

#' Sidebar setup for the apps
#' 
#' Internal function, not exported, to help maintain consistent sidebars in a tabbed app.
#' @param ... Other stuff to add to the \code{sidebarPanel} function.
#' @noRd
#' @keywords internal
sidebar_setup <- function(...){
  sb <- sidebarPanel(
            width=3,
            br(),
            #img(src = "spc.png", height = 100),
            br(),
            br(),
            br(),
            ...
          ) # End of sidebarPanel
  return(sb)
}
