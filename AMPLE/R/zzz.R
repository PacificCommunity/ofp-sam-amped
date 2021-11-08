# zzz.R

# Stuff so that the logo in the inst/www folder gets found
# Adapted from help found here: 
# https://community.rstudio.com/t/trouble-including-image-jpeg-png-svg-etc-in-shiny-app-embedded-in-r-package/56156/2
# Thanks!
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "www/img",
      package = "AMPLE"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}
