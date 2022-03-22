# For deployment to shinyapps.io
# Do not include in package or commit to git or anything

# Include pkgload in DESCRIPTION imports
# Put this app.R in script package root, next to DESCRIPTION
pkgload::load_all(".")
intro_hcr()
#measuring_performance()
#comparing_performance()

# Click publish and select all
# (could probably drop man, vignettes, test)


# Include this shite to make the publish button appear in RStudio - otherwise do rsconnect by hand
#library(shiny)
#ui <- fluidPage(
#  "Hello, world!"
#  #br(),
#  #packageDescription("AMPLE")
#)
#server <- function(input, output, session) {
#}
#shinyApp(ui, server)
