# sidebar set func

# So all the sidebars have the same width and SPC logo
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
