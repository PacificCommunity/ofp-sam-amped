#--------------------------------------------------------------
# PIMPLE
# Performance Indicators and Management Procedures Explorer
# Main app

# Copyright 2019 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC
#--------------------------------------------------------------

# Load packages
library(AMPLE)

# Load the data
load("data/preWCPFC2019_results.Rdata")


#------------------------------------------------------------------------------------------------------
# HACKS 

# Overwrite colour palette from AMPLE - fuck - how to overwrite?
#source("~/Work/NZ_project/ofp-sam-amped/AMPLE/R/plots.R")


# HACK drop SQ HCRs
hcr_points <- subset(hcr_points, !(hcrref %in% c("SQ", "SQ +10%", "SQ +20%", "SQ +30%")))
hcr_shape <- subset(hcr_shape, !(hcrref %in% c("SQ", "SQ +10%", "SQ +20%", "SQ +30%")))
periodqs <- subset(periodqs, !(hcrref %in% c("SQ", "SQ +10%", "SQ +20%", "SQ +30%")))
worms <- subset(worms, !(hcrref %in% c("SQ", "SQ +10%", "SQ +20%", "SQ +30%")))
yearqs <- subset(yearqs, !(hcrref %in% c("SQ", "SQ +10%", "SQ +20%", "SQ +30%")))

# Drop MWI indicator
# Add back in if we get mean weight in the catch
periodqs <- subset(periodqs, pi != "mw")
yearqs <- subset(yearqs, pi != "mw")
worms <- subset(worms, pi != "mw")

# Set area NA to area 0 (for subsetting)
#periodqs[is.na(periodqs$area),"area"] <- as.factor("0")


# Rename some indicators
# Catch to Relative catch
oldpi3name <- "PI 3: Catch"
newpi3name <- "PI 3: Catch (rel. to 2013-2015)"
periodqs[periodqs$piname == oldpi3name, "piname"] <- newpi3name
yearqs[yearqs$piname == oldpi3name, "piname"] <- newpi3name
worms[worms$piname == oldpi3name, "piname"] <- newpi3name
# Relative CPUE
oldpi4name <- "PI 4: Relative CPUE"
newpi4name <- "PI 4: CPUE (rel. to 2010)\n(PS in areas 2,3,5 only)"
periodqs[periodqs$piname == oldpi4name, "piname"] <- newpi4name
yearqs[yearqs$piname == oldpi4name, "piname"] <- newpi4name
worms[worms$piname == oldpi4name, "piname"] <- newpi4name
# Relative effort to effort
oldpi7name <- "PI 7: Relative effort stability"
newpi7name <- "PI 7: Effort stability\n(PS in areas 2,3,5 only)"
periodqs[periodqs$piname == oldpi7name, "piname"] <- newpi7name
yearqs[yearqs$piname == oldpi7name, "piname"] <- newpi7name
worms[worms$piname == oldpi7name, "piname"] <- newpi7name


#------------------------------------------------------------------------------------------------------
# Can we move all this down to the server side?
# Data processing


# Data for the histogram plots
# Move all this to the data preparation stage
breaks <- seq(from=0,to=2,by=0.05)
freqs <- cut(hcr_points$scaler, breaks, labels=FALSE)
hcr_points$bin <- breaks[freqs]
histodat <- dplyr::group_by(hcr_points, hcrref, hcrname, period, bin)
histodat <- dplyr::summarise(histodat, sum=dplyr::n())
nobs <- dplyr::group_by(hcr_points,hcrref, hcrname, period)
nobs <- dplyr::summarise(nobs, tnobs=dplyr::n())

histodat <- dplyr::left_join(histodat, nobs)
histodat$prop <- histodat$sum / histodat$tnobs

# Sort everything by HCR - probably should already be OK
histodat <- histodat[order(histodat$hcrref),]
periodqs <- periodqs[order(periodqs$hcrref),]
yearqs <- yearqs[order(yearqs$hcrref),]
worms <- worms[order(worms$hcrref),]
hcr_points <- hcr_points[order(hcr_points$hcrref),]
hcr_shape <- hcr_shape[order(hcr_shape$hcrref),]

# Fix names of PI selector - remove PS note
pis_list <- unique(periodqs[!periodqs$upsidedown,"piname"])
piselector <- as.list(pis_list)
pis_text <- unlist(lapply(strsplit(pis_list,"\n"),'[',1))
names(piselector) <- pis_text

worms$wormid <- paste(worms$msectrl, worms$iter, sep="_")

main_panel_width <- 10
side_panel_width <- 12 - main_panel_width 

# When is short, medium and long-term?

short <- range(subset(yearqs, period=="Short")$year)
shorttext <- paste(short, collapse="-")
medium <- range(subset(yearqs, period=="Medium")$year)
mediumtext <- paste(medium, collapse="-")
long <- range(subset(yearqs, period=="Long")$year)
longtext <- paste(long, collapse="-")
yearrangetext <- paste("Short-term is: ", shorttext, ", medium-term is: ", mediumtext, " and long-term is: ", longtext,".",sep="")
pi47text <- "Note that PIs 4 and 7 are for the purse seines in model areas 2, 3 and 5 only (excluding the associated purse seines in area 5.)"
pi36text <- "The grouping for PIs 3 and 6 is given by the drop down menu on the left."
biotext <- "PIs 1, 8 and SB/SBF=0 are calculated over all model areas."


#------------------------------------------------------------------------------------------------------

# Navbarpage insidea fluidpage?
# Pretty nasty but it means we get the power of the navparPage and can have common side panel
ui <- fluidPage(id="top",
  #titlePanel("Performance Indicators and Management Procedures Explorer"),
  sidebarLayout(
    sidebarPanel(width=side_panel_width,
      br(),
      img(src = "spc.png", height = 100),
      br(),
      conditionalPanel(condition="input.nvp == 'compareMPs' ||input.nvp == 'explorePIs'",
        checkboxGroupInput(inputId = "hcrchoice", label="HCR selection", selected = unique(periodqs$hcrref), choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref))
      ),
      # PI choice - only shown in the compare PIs tab
      conditionalPanel(condition="input.nvp== 'compareMPs'",
        checkboxGroupInput(inputId = "pichoice", label="PI selection",choices = piselector, selected=sort(unique(periodqs$piname)))
      ),
      # Show spaghetti on the time series plots - do not show on the HCR tab
      # Currently shown on all explorePIs tabs - even if no timeseries plot - is this OK?
      conditionalPanel(condition="input.nvp== 'explorePIs' || input.comptab == 'timeseries'",
        checkboxInput("showspag", "Show trajectories", value=FALSE) 
      ),
      # Catch choice - only in the catch and catch stability PI tabs
      conditionalPanel(condition="(input.nvp== 'compareMPs' || input.pitab == 'pi3' || input.pitab == 'pi6')",
        #selectInput(inputId = "catchareachoice", label="Catch grouping", choices = list("Total"="total", "Purse seine in regions 2,3 & 5"="ps235", "Area 1"="1", "Area 2"="2", "Area 3"="3", "Area 4"="4", "Area 5"="5"), selected="total")
        selectInput(inputId = "catchareachoice", label="Catch grouping (for PIs 3 and 6 only)", choices = list("Total"="total", "Purse seine in regions 2,3 & 5"="ps235"), selected="total")
        #selectInput(inputId = "catchrelchoice", label="Catch type", choices = list("Absolute catch"="catch", "Relative to average catch in 2013-15"="relative catch"), selected="catch")
      )
    ),
    mainPanel(width=main_panel_width,
      tags$style(type="text/css", "body {padding-top: 70px;}"), # padding - as we use fixed-top for position, applies to all tabs
      navbarPage(id="nvp",
        collapsible=TRUE,  # Should help if using small screens like tablets
        windowTitle="PIMPLE",
        position="fixed-top",
        #title="PIMPLE", # Needed or "" else first tab vanishes
        #title="", # Needed or "" else first tab vanishes
        title="Performance Indicators and Management Procedures Explorer",
        tabPanel("Introduction", value="intro",
          h1("PIMPLE"),
          p("PIMPLE is a tool for exploring and comparing the performance of alternative candidate harvest control rules (HCRs).
          The performance can be explored using a range of different plots and tables.
          This allows trade-offs between the different HCRs to be evaluated.
          The performance of each HCR is measured using different performance indicators (PIs). More details of the PIs are given in the PIMPLE user guide."), 
          br(),
          h2("The Indicators"),
          br(),
          h3("SB/SBF=0"),
          p("The depletion of the stock, i.e. the ratio of the current adult biomass to the adult biomass in the absence of fishing. It is comparable to the Limit Reference Point (0.2) and Target Reference Point (0.5, interim)."),
          br(),
          h3("Indicator 1. Maintain SKJ biomass at or above levels that provide fishery sustainability throughout their range"),
          p("This is calculated as the probability of the SB/SBF=0 being above the Limit Reference Point (0.2)"),
          br(),
          h3("Indicator 3. Maximise economic yield from the fishery (average expected catch)"),
          p("This indicator is based on the average expected catch. It is calculated either as the absolute level of expected catch, or relative the average catch level in 2013-15"),
          p("The catch is either the total catch in the whole region, or only the purse seine catch from stock assessment regions 2,3 and 5."),
          br(),
          h3("Indicator 4. Maintain acceptable CPUE"),
          p("This indicator is based on the average deviation of predicted skipjack CPUE from reference period levels.  It is calculated as the CPUE relative to the CPUE in a reference period.  Here the reference period is taken to be 2010."),
          br(),
          h3("Indicator 6. Catch stability"),
          p("This indicator is concerned with the average annual variation in catch. It is calculated over the whole region and for the combined purse seine fisheries in regions 2, 3 and 5. The indicator is calculated by taking the absolute annual difference of the catch for each simulation and in each year."),
          p("As well as the variability in the catch, the stability (the inverse of the variability) is calculated.  This involves rescaling the variability so that it is between 0 and 1. A stability of 1 implies that the catch does not change at all over time, i.e. it is completely stable. A stability of 0 means that the catch is very variable in time."),
          br(),
          h3("Indicator 7. Stability and continuity of market supply (effort variation relative to a reference period)"),
          p("This indicator is concerned with effort variation relative to the effort in a reference period, i.e. stability of the relative effort. Here the reference period is taken to be 2010. This indicator is calculated for the purse seine fisheries operating in regions 2, 3 and 5 excluding the associated purse seine fishery in region 5 which has a standardised effort index. The indicator is calculated in a similar way to performance indicator 6. The absolute annual difference of the effort relative to the base effort (in 2010) is calculated for each simulation in each year"), 
          p("As well as the variability, the stability is calculated. A stability of 1 implies that the relative effort does not change at all over time, i.e. it is completely stable. A stability of 0 means that the relative effort is very variable in time. In PIMPLE only the stability is shown."),
          br(),
          h3("Indicator 8. Stability and continuity of market supply"),
          p("This indicator is concerned with maintaining the stock size around the TRP levels (where the interim TRP for skipjack is SB/SBF=0 = 0.5).  It is assumed that the further away SB/SBF=0 is from 0.5, the worse the HCR can be thought to be performing, i.e. it is better to have SB/SB_F=0 close to 0.5 on average."),
          p("An indicator value of 1 implies that SB/SB_F=0 is exactly at the TRP and a value of 0 is as far from the TRP as possible. This means that you want this indicator to be close to 1. In PIMPLE only the stability is shown."),
          br(),
          h3("Mean weight of an individual in the population"),
          p("This indicator measures the mean weight of an individual in the population, not the catch. It is calulated by taking the total weight of individuals across the region and dividing it by the total number of individuals across the region. These kind of indicators are important because they can provide information on changes to the size structure of a population as a result of fishing and changes in environmental conditions."), 
          br(),
          h2("The Plots"),
          br(),
          h3("Bar plots"),
          p("The bar plots show the median values of each of the indicators, averaged over the three time periods."),
          h3("Box plots"),
          p("The bar plots show the distribution of values of each of the indicators, averaged over the three time periods.
             The box contains the 20-80 percentiles, the tails the 5-95 percentiles. The solid horizontal line is the median value."),
          h3("Time series plots"), 
          p("The ribbons show the 10-90 percentiles. The dashed line shows the median value.")
        ),
        tabPanel("Compare performance", value="compareMPs",
          tabsetPanel(id="comptab",
            tabPanel("Bar charts", value="bar",
              plotOutput("plot_bar_comparehcr", height="600px"),
              p(yearrangetext),
              p(pi47text),
              p(biotext),
              p(pi36text)
            ),
            tabPanel("Box plots", value="box",
              plotOutput("plot_box_comparehcr", height="600px"),
              p(yearrangetext),
              p(pi47text),
              p(biotext),
              p(pi36text)
            ),
            tabPanel("Radar plots", value="radar",
              fluidRow(
                #selectInput(inputId = "radarscaling", label="Radar plot scaling", choices = list("Scale by max"="scale", "Rank"="rank"), selected="scale"),
                tags$span(title="Note that only the indicators for which 'bigger is better' are shown in the radar plots.",
                  plotOutput("plot_radar_comparehcr", height="600px")),
                p("Note that only the indicators for which 'bigger is better' are shown in the radar plots."),
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text)
              )
            ),
            tabPanel("Time series plots", value="timeseries",
              tags$span(title="Note that not all indicators have time series plots. The widths of the ribbons are the 10-90 percentiles. The dashed, black line is the median value.",
                plotOutput("plot_timeseries_comparehcr") # height is variable
              )
            ),
            tabPanel("Table", value="bigtable",
              tags$span(title="Median indicator values. The values inside the parentheses are the 10-90 percentiles",
                tableOutput("table_pis_short"),
                tableOutput("table_pis_medium"),
                tableOutput("table_pis_long"),
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text)
              )
            )
          )
        ),
        tabPanel("Explore indicators", value="explorePIs",
          tabsetPanel(id="pitab",
          # --- SBSBF0 and PI 1 & 8 ---
            tabPanel("PI 1 & 8: Biomass",value="pi1",
              column(12, fluidRow(
                # TS of SBSBF0
                plotOutput("plot_ts_sbsbf0")
              )),
              column(4, fluidRow(
                # Bar plot of median SBSBF0
                plotOutput("plot_bar_sbsbf0"),
                # Bar of PI 8
                plotOutput("plot_bar_pi8")
              )),
              column(4, fluidRow(
                # Box of SBSBF0
                plotOutput("plot_box_sbsbf0"),
                # Box of PI 8
                plotOutput("plot_box_pi8")
              )),
              column(4, fluidRow(
                # Bar plot of prob
                plotOutput("plot_bar_problrp")
              )),
              column(12, fluidRow(
                p(yearrangetext),
                p(biotext)
              ))
            ),
            # *** PI 3: Catch based ones ***
            tabPanel("PI 3: Catches",value="pi3",
              column(12, fluidRow(
                plotOutput("plot_ts_catch")
              )),
              column(6, fluidRow(
                plotOutput("plot_bar_catch")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_catch")
              )),
              p("Note that the catches are relative to the average catch in the years 2013-2015."),
              p(yearrangetext)
            ),
            # *** PI 4: Relative CPUE***
            tabPanel("PI 4: Relative CPUE",value="pi4",
              column(12, fluidRow(
                plotOutput("plot_ts_relcpue")
              )),
              column(6, fluidRow(
                plotOutput("plot_bar_relcpue")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_relcpue")
              )),
              p("Note that the CPUE only includes purse seines in model regions 2, 3 and 5, excluding the associated purse seines in region 5. Relative CPUE is the CPUE relative to that in 2010."),
              p(yearrangetext),
              p(pi47text)
            ),
            # *** PI 6: Catch stability ***
            tabPanel("PI 6: Catch stability",value="pi6",
              column(6, fluidRow(
                plotOutput("plot_bar_catchstab"),
                plotOutput("plot_bar_catchvar")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_catchstab"),
                plotOutput("plot_box_catchvar")
              )),
              p(yearrangetext)
            ),
            # *** PI 7: Relative effort variability***
            tabPanel("PI 7: Effort stability",value="pi7",
              column(6, fluidRow(
                plotOutput("plot_bar_pi7stab"),
                plotOutput("plot_bar_pi7var")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_pi7stab"),
                plotOutput("plot_box_pi7var")
              )),
              p("Note that the effort only includes purse seines in model regions 2, 3 and 5, excluding the associated purse seines in region 5. Relative effort is the effort relative to that in 2010."),
              p(yearrangetext),
              p(pi47text)
            )
            # *** Mean weight individual ***
            #tabPanel("Mean weight of individual",value="mw",
            #  column(12, fluidRow(
            #    plotOutput("plot_ts_mw")
            #  )),
            #  column(6, fluidRow(
            #    plotOutput("plot_bar_mw")
            #  )),
            #  column(6, fluidRow(
            #    plotOutput("plot_box_mw")
            #  ))
            #)
          )
        ),
        ## The HCRs
        #tabPanel(title="HCRs", value="hcrs",
        #  column(12, fluidRow(
        #    tags$span(title="Shape of the HCRs under consideration",
        #      plotOutput("plot_hcrshape", height="600px")),
        #    tags$span(title="Histograms of which parts of the HCRs were active during the evaluations",
        #      plotOutput("plot_hcrhistograms")))
        #  )
        #),
        tabPanel("About", value="about",
          spc_about()
        )
      )
    )
  )
)



# Server function
server <- function(input, output, session) {
  # General plotting parameters
  # Get these from the data rather than fixing them here
  short_term <- sort(unique(subset(worms, period=="Short")$year))
  medium_term <- sort(unique(subset(worms, period=="Medium")$year))
  long_term <- sort(unique(subset(worms, period=="Long")$year))
  last_plot_year <- max(long_term)
  first_plot_year <- 1985
  # Maybe make this an option in the future?
  pi_percentiles <- c(10,90)

  # Trim out years for tight time series plots
  yearqs <- subset(yearqs, year %in% first_plot_year:last_plot_year)
  worms <- subset(worms, year %in% first_plot_year:last_plot_year)

  # Careful with these - they are only used for plotting lines, NOT for calculating the indicators
  lrp <- 0.2
  trp <- 0.5
  # For the worms - same worms for all plots
  # This can be increased to 20 - maybe make as option?
  nworms <- 10
  # worms are a unique combination of OM and iter
  # (same om / iter should be in all hcrs)
  wormiters <- sample(unique(worms$iter), nworms)

  #-------------------------------------------------------------------
  # Make the checkbox inputs match each other
  observeEvent(input$hcrchoice_pitab, {
    newsel <- input$hcrchoice_pitab
    updateCheckboxGroupInput(session, "hcrchoice", 
      choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref),
      selected = newsel)
  })

  observeEvent(input$hcrchoice, {
    newsel <- input$hcrchoice
    updateCheckboxGroupInput(session, "hcrchoice_pitab", 
      choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref),
      selected = newsel)
  })


  #-------------------------------------------------------------------
  # Individual PI plots

  # Timeseries
  # SBSBF0
  output$plot_ts_sbsbf0  <- renderPlot({
    show_spaghetti <- input$showspag
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    # SBSBF0 - Just combined area
    dat <- subset(yearqs, pi=="biomass" & metric=="SBSBF0" & area=="all") 
    # Add Option for worms
    wormdat <- subset(worms, pi=="biomass" & metric=="SBSBF0" & area=="all" & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=lrp), linetype=3)
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=trp), linetype=3)
    p <- p + ggplot2::ylab("SB/SBF=0")
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    return(p)
  })

  # Bar and box plot 
  # SBSBF0
  plot_barbox_sbsbf0 <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      dat <- subset(periodqs, period != "Rest" & pi=="biomass" & metric=="SBSBF0" & area=="all") 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab("SB/SBF=0") + ggplot2::ylim(c(0,1))
      p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=lrp), linetype=2) + ggplot2::geom_hline(ggplot2::aes(yintercept=trp), linetype=2)
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_sbsbf0 <- plot_barbox_sbsbf0(plot_type="median_bar")
  output$plot_box_sbsbf0 <- plot_barbox_sbsbf0(plot_type="box")
    
  # Bar plot 
  # PI 1: Prob of SBSBF0 > LRP
  output$plot_bar_problrp <- renderPlot({
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    dat <- subset(periodqs, period != "Rest" & pi=="pi1" & area=="all") 
    p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type="median_bar")
    p <- p + ggplot2::ylab("PI 1: Prob. SB/SBF=0 > LRP") + ggplot2::ylim(c(0,1))
    return(p)
  })

  # Bar and box plot 
  # PI 8: SB/SBF=0 proximity to TRP
  plot_barbox_pi8 <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      dat <- subset(periodqs, period != "Rest" & pi=="pi8" & area=="all") 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab("PI 8: Average proximity to TRP") + ggplot2::ylim(c(0,1))
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_pi8 <- plot_barbox_pi8(plot_type="median_bar")
  output$plot_box_pi8 <- plot_barbox_pi8(plot_type="box")

  # Timeseries
  # PI 3: Catch
  output$plot_ts_catch <- renderPlot({
    show_spaghetti <- input$showspag
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }


    #browser()

    catch_area_choice <- input$catchareachoice
    # Choose if relative to year X
    #catch_rel_choice <- input$catchrelchoice # or relative catch
    catch_rel_choice <- "relative catch"
    dat <- subset(yearqs, pi=="pi3" & area==catch_area_choice & metric == catch_rel_choice) 
    wormdat <- subset(worms, pi=="pi3" & area==catch_area_choice & metric == catch_rel_choice & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
    p <- p + ggplot2::ylab("Catch")
    p <- p +ggplot2:: ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    return(p)
  })

  # Bar and box plot 
  # PI 3: Catch
  plot_barbox_catch <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      # Choose the aggregation (set: total area or PS235 - maybe by area too?)
      catch_area_choice <- input$catchareachoice
      # Choose if relative to year X
      #catch_rel_choice <- input$catchrelchoice # or relative catch
      catch_rel_choice <- "relative catch"
      dat <- subset(periodqs, period != "Rest" & pi=="pi3" & area==catch_area_choice & metric == catch_rel_choice) 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab("PI 3: Catch") + ggplot2::ylim(c(0,NA))
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_catch <- plot_barbox_catch(plot_type="median_bar")
  output$plot_box_catch <- plot_barbox_catch(plot_type="box")

  # Timeseries
  # PI: 4
  output$plot_ts_relcpue <- renderPlot({
    show_spaghetti <- input$showspag
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    dat <- subset(yearqs, pi=="pi4") 
    # Add Option for worms
    wormdat <- subset(worms, pi=="pi4" & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
    p <- p + ggplot2::ylab("Relative CPUE")
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    return(p)
  })

  # Bar and box plot 
  # PI 4: Catch
  plot_barbox_relcpue<- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      dat <- subset(periodqs, period != "Rest" & pi=="pi4") 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab("PI 4: Relative CPUE") + ggplot2::ylim(c(0,NA))
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_relcpue <- plot_barbox_relcpue(plot_type="median_bar")
  output$plot_box_relcpue <- plot_barbox_relcpue(plot_type="box")

  # Bar and box plot 
  # PI 6: Catch variability and stability
  plot_barbox_catchvarstab <- function(plot_type="median_bar", metric_choice="catch variability", ylab="PI 6: Catch variability", ylim=c(0,NA)){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      # Choose the aggregation (set: total area or PS235 - maybe by area too?)
      catch_area_choice <- input$catchareachoice
      # Choose if relative to year X
      dat <- subset(periodqs, period != "Rest" & pi=="pi6" & area==catch_area_choice & metric==metric_choice) 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab(ylab) + ggplot2::ylim(ylim)
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_catchvar <- plot_barbox_catchvarstab(plot_type="median_bar", metric_choice="catch variability", ylab="PI 6: Catch variability", ylim=c(0,NA))
  output$plot_box_catchvar <- plot_barbox_catchvarstab(plot_type="box", metric_choice="catch variability", ylab="PI 6: Catch variability", ylim=c(0,NA))
  output$plot_bar_catchstab <- plot_barbox_catchvarstab(plot_type="median_bar", metric_choice="catch stability", ylab="PI 6: Catch stability", ylim=c(0,1))
  output$plot_box_catchstab <- plot_barbox_catchvarstab(plot_type="box", metric_choice="catch stability", ylab="PI 6: Catch stability", ylim=c(0,1))


  # Bar and box plot 
  # PI 7: Catch variability and stability
  plot_barbox_pi7varstab <- function(plot_type="median_bar", metric_choice="relative effort variability", ylab="PI 7: Relative effort variability", ylim=c(0,NA)){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      # Choose if relative to year X
      dat <- subset(periodqs, period != "Rest" & pi=="pi7" & metric==metric_choice) 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab(ylab) + ggplot2::ylim(ylim)
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_pi7var <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort variability", ylab="PI 7: Variability of relative effort", ylim=c(0,NA))
  output$plot_box_pi7var <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort variability", ylab="PI 7: Variability of relative effort", ylim=c(0,NA))
  output$plot_bar_pi7stab <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort stability", ylab="PI 7: Stability", ylim=c(0,1))
  output$plot_box_pi7stab <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort stability", ylab="PI 7: Stability", ylim=c(0,1))





  # Mean weight of individual
  # PI: 4
  output$plot_ts_mw<- renderPlot({
    show_spaghetti <- input$showspag
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    dat <- subset(yearqs, pi=="mw") 
    # Add Option for worms
    wormdat <- subset(worms, pi=="mw" & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
    p <- p + ggplot2::ylab("Mean weight of an individual")
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    return(p)
  })

  # Bar and box plot 
  # Mean weight of individual
  plot_barbox_mw <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      dat <- subset(periodqs, period != "Rest" & pi=="mw") 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab("Mean weight of an individual") + ggplot2::ylim(c(0,NA))
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_mw <- plot_barbox_mw(plot_type="median_bar")
  output$plot_box_mw <- plot_barbox_mw(plot_type="box")




  #-------------------------------------------------------------------
  # Comparison plots

  # Bar or box plot - facetting on PI
  plot_barbox_comparehcr <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      # It gets complicated because each PI has suboption in area, metric columns
      # biomass: area == all and metric == SBSBF0
      # pi1: area == all
      # pi3: area == catchareachoice and metric == catchrelchoice
      # pi4: metric == relative_cpue
      # pi6: area == catchareachoice and metric == stability or variability - just stab
      # pi7: metric == stability or variability - just stab
      # pi8: area == all
      # mw: mean_weight

      # Put these together
      # area is catchareachoice
      # metric is catchrelchoice, stability, or NA
      hcr_choices <- input$hcrchoice
      pi_choices <- input$pichoice
      if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
        return()
      }
      catch_area_choice <- input$catchareachoice
      other_area_choice <- c(as.character(NA), "all")
      catch_rel_choice <- "relative catch"
      metric_choices <- c(catch_rel_choice, "mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
      # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
      dat <- subset(periodqs, ((pi %in% c("pi3","pi6") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6")) & area %in% other_area_choice)) & period != "Rest" & piname %in% pi_choices & metric %in% metric_choices)
      #dat <- subset(periodqs, period != "Rest" & piname %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
      # Need to hack pi1 so that all quantiles = X50., else NA
      dat[dat$pi=="pi1",c("X5.", "X20.", "X80.", "X95.")] <- dat[dat$pi=="pi1","X50."]
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylim(0,NA)
      p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
      # Add LRP and TRP lines
      # Only if SB/SBF=0 is in dat
      if ("SB/SBF=0" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
        p <- p + ggplot2::geom_hline(data=data.frame(yint=trp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_comparehcr <- plot_barbox_comparehcr(plot_type="median_bar")
  output$plot_box_comparehcr <- plot_barbox_comparehcr(plot_type="box")

  # Radar plot
  # Some repetition of metric, set, area combos
  # Add border to radar
  output$plot_radar_comparehcr <- renderPlot({
    # Subsetting out as above
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    #set_choices <- c(input$catchsetchoice, as.character(NA))
    #metric_choices <- c(input$catchrelchoice,"mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    #area_choices <- c("all", as.character(NA))
    catch_area_choice <- input$catchareachoice
    other_area_choice <- c(as.character(NA), "all")
    catch_rel_choice <- "relative catch"
    metric_choices <- c(catch_rel_choice, "mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    # We have 8 PIs - but not all are appropriate for a radar plot as bigger is not better.
    # Drop SB/SBF=0 and Size based one 
    not_radar_pinames <- c("SB/SBF=0", "Mean weight of individual")
    pi_choices <- pi_choices[!(pi_choices %in% not_radar_pinames)]
    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }
    #dat <- subset(periodqs, period != "Rest" & piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)

    # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
    dat <- subset(periodqs, ((pi %in% c("pi3","pi6") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6")) & area %in% other_area_choice)) & period != "Rest" & piname %in% pi_choices & metric %in% metric_choices)

    #scaling_choice <- input$radarscaling
    scaling_choice <- "scale"
    p <- myradar(dat=dat, hcr_choices=hcr_choices, scaling_choice)
    return(p)
  })

  # Time series comparisons
  height_per_pi <- 300
  #pinames_ts <- c("PI 1: Prob. above LRP", "SB/SBF=0", "PI 3: Catch","PI 4: Relative CPUE", "PI 8: Proximity to TRP", "Mean weight of individual")
  pinames_ts <- c("SB/SBF=0", newpi3name, newpi4name, "PI 8: Proximity to TRP")
  output$plot_timeseries_comparehcr <- renderPlot({
    show_spaghetti <- input$showspag
    hcr_choices <- input$hcrchoice

    # Only these ones allowed
    pi_choices <- input$pichoice
    pi_choices <- pi_choices[pi_choices %in% pinames_ts]

    #set_choices <- c(input$catchsetchoice, as.character(NA))
    #metric_choices <- c(input$catchrelchoice,"mean_weight",  "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    #metric_choices <- c("relative catch","mean_weight",  "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    #area_choices <- c("all", as.character(NA))
    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }

    catch_area_choice <- input$catchareachoice
    other_area_choice <- c(as.character(NA), "all")
    catch_rel_choice <- "relative catch"
    metric_choices <- c(catch_rel_choice, "mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
    dat <- subset(yearqs, ((pi %in% c("pi3","pi6") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6")) & area %in% other_area_choice)) & period != "Rest" & piname %in% pi_choices & metric %in% metric_choices)
    wormdat <- subset(worms, ((pi %in% c("pi3","pi6") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6")) & area %in% other_area_choice)) & period != "Rest" & piname %in% pi_choices & metric %in% metric_choices & iter %in% wormiters)

    #dat <- subset(yearqs, piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)
    #wormdat <- subset(worms, piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices & iter %in% wormiters)

    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
    #p <- p + ylab("Catch")
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("Value")
    # Add LRP and TRP if SB/SBF=0 is plotted
    if ("SB/SBF=0" %in% pi_choices){
      p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      p <- p + ggplot2::geom_hline(data=data.frame(yint=trp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
    }
    return(p)
  }, height=function(){max(height_per_pi*1.5, (height_per_pi * length(input$pichoice[input$pichoice %in% pinames_ts])))})

  # Plot the HCR shapes and the bits that were active
  output$plot_hcrshape <- renderPlot({
    # Able to choose which HCRs
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    p <- hcr_plot(hcr_choices=hcr_choices, hcr_shape=hcr_shape, hcr_points=hcr_points, lrp=lrp, trp=trp)
    return(p)
  })

  # Plot histogram of effort multipliers
  output$plot_hcrhistograms <- renderPlot({
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    p <- hcr_histo_plot(hcr_choices, histodat)
    return(p)
  })


  get_pi_table <- function(period_choice="Short"){
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    #set_choices <- c(input$catchsetchoice, as.character(NA))
    #metric_choices <- c(input$catchrelchoice, "mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    #metric_choices <- c("relative catch", "mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    #area_choices <- c("all", as.character(NA))
    catch_area_choice <- input$catchareachoice
    other_area_choice <- c(as.character(NA), "all")
    catch_rel_choice <- "relative catch"
    metric_choices <- c(catch_rel_choice, "mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")

    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }
    #dat <- subset(periodqs, hcrref %in% hcr_choices & period == period_choice & piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)
    # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
    dat <- subset(periodqs, hcrref %in% hcr_choices & ((pi %in% c("pi3","pi6") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6")) & area %in% other_area_choice)) & period == period_choice & piname %in% pi_choices & metric %in% metric_choices)
    tabdat <- pitable(dat, percentile_range = pi_percentiles)
    return(tabdat)
  }

  output$table_pis_short <- renderTable({
      tabdat <- get_pi_table(period_choice="Short")
    },
    rownames = FALSE,
    caption= "Performance indicators in the short-term",
    auto=TRUE
  )

  output$table_pis_medium <- renderTable({
      tabdat <- get_pi_table(period_choice="Medium")
    },
    rownames = FALSE,
    caption= "Performance indicators in the medium-term",
    auto=TRUE
  )

  output$table_pis_long <- renderTable({
      tabdat <- get_pi_table(period_choice="Long")
    },
    rownames = FALSE,
    caption= "Performance indicators in the long-term",
    auto=TRUE
  )




}

# Run the app
shinyApp(ui, server)
