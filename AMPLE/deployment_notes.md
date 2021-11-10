
## Deployment

To deploy the apps to shinyapps.io you need to make an 'app.R' file in the *root* directory of the package, i.e. in the same directory as the DESCRIPTION file.

The app.R file should have these contents:

```{r eval=FALSE}
# Include pkgload in DESCRIPTION import as we need to use it here.
pkgload::load_all(".")
intro_hcr() # Or whichever app function you want

# Include this stuff to make the publish button appear in RStudio - otherwise do rsconnect() by hand
#library(shiny)
#ui <- fluidPage(
#  "Hello, world!"
#  #br(),
#  #packageDescription("AMPLE")
#)
#server <- function(input, output, session) {
#}
#shinyApp(ui, server)
```

In RStudio, set the working directory to the package *root* directory.
Click publish and select all the files we need  (dropping man, vignettes, test folders).

Do not add this app.R file to the package when submitting to CRAN.
It should be listed in the .Rbuildignore.
