# sidebar set func

# So all the sidebars have the same width and SPC logo
#' Intro HCR sidebar setup
#' 
#' Internal function, not exported, to help maintain consistent sidebars in a tabbed app.
#' @param ... Other stuff to add to the \code{sidebarPanel} function.
intro_hcr_sidebar_setup <- function(...){
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
