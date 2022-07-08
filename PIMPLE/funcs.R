# General functions

#' SPC about information
#' 
#' SPC about information included in all the Shiny apps.
#' @noRd
#' @keywords internal
spc_about <- function(){
  out <- tags$html(
    tags$p(style="opacity: 0.5", class="caption", align="center", HTML("&copy"), "Pacific Community, 2021"),
    tags$h1("About us:"),
    tags$p(align="justify", "The Pacific Community (SPC) is the principal scientific and technical organisation in the Pacific region, proudly supporting development since 1947. It is an international development organisation owned and governed by its 26 country and territory members. The members are: American Samoa, Australia, Cook Islands, Federated States of Micronesia, Fiji, France, French Polynesia, Guam, Kiribati, Marshall Islands, Nauru, New Caledonia, New Zealand, Niue, Northern Mariana Islands, Palau, Papua New Guinea, Pitcairn Islands, Samoa, Solomon Islands, Tokelau, Tonga, Tuvalu, United States of America, Vanuatu, and Wallis and Futuna."),
    tags$p(align="justify", "In pursuit of sustainable development to benefit Pacific people, this unique organisation works across more than 25 sectors. SPC is renowned for its knowledge and innovation in such areas as fisheries science, public health surveillance, geoscience and conservation of plant genetic resources for food and agriculture."),
    tags$p(align="justify", "Much of SPC's focus is on major cross-cutting issues, such as climate change, disaster risk management, food security, gender equality, human rights, non-communicable diseases and youth employment. Using a multi-sector approach in responding to its members' development priorities, SPC draws on skills and capabilities from around the region and internationally, and supports the empowerment of Pacific communities and sharing of expertise and skills between countries and territories."),
    tags$p(align="justify", "With over 600 staff, SPC has its headquarters in Noumea, regional offices in Suva and Pohnpei, a country office in Honiara and field staff in other Pacific locations. Its working languages are English and French. See: ", a("https://www.spc.int", href="www.spc.int"))
  )
}

# The big PI tables for comparing HCRs
#' pitable
#'
#' pitable() is not a plot but a table comparing PIs across HCRs and periods. Only pass in 1 time period at a time.
#'
#' @param signif Number of significent digits for table. Default is 3.
#'
#' @return A data.frame to be shown as a table.
#' @rdname comparison_plots
#' @name Comparison plots
#' @export
# Added signif argument to be added to AMPLE
pitable <- function(dat, percentile_range = c(10,90), signif=3){
  # Rows are the PIs, columns are the HCRs
  colmin <- paste("X",percentile_range[1],".",sep="")
  colmax <- paste("X",percentile_range[2],".",sep="")
  percentile_min <- dat[, get(colmin)]
  percentile_max <- dat[, get(colmax)]
  dat$value <- paste(signif(dat$X50.,signif), " (", signif(percentile_min, signif), ",", signif(percentile_max, signif), ")", sep="")
  # Fix pi1
  dat[dat$pi=="pi1", "value"] <- signif(dat[dat$pi=="pi1", "X50."],signif)
  tabdat <- dat[,c("hcrref", "piname", "value")]
  tabdat[tabdat$name=="Biomass","piname"] <- "SB/SBF=0"
  tabdat <- as.data.frame(spread(tabdat, key="hcrref", value="value"))
  # Have rownames?
  #rnames <- tabdat[,1]
  #tabdat <- tabdat[,-1]
  #rownames(tabdat) <- rnames
  colnames(tabdat)[1] <- "Indicator"
  return(tabdat)
}


