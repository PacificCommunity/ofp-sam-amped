#--------------------------------------------------------------
# SPAMPLE
# SPA Performance Indicators and Management Procedures Explorer
# Main app
# Updated for SC16

# Copyright 2020 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC
#--------------------------------------------------------------
#rsconnect::deployApp("C:/Work/ShinyMSE/ofp-sam-amped/PIMPLE") 
# Load packages

# Note: get AMPLE from github, not from CRAN
# Run the below command then comment out
#devtools::install_github("PacificCommunity/ofp-sam-amped/AMPLE", ref="SC16")
library(AMPLE)

library(ggplot2)
library(RColorBrewer)
library(markdown)

# Load the data
# Demo data from SKJ
#load("data/SC16_results.Rdata")
load("data/albpimple_test.Rdata")

#----------------------------------------------------------------------------------------------------
# Data structure
# Three data.frames: periodqs (for bar charts / box plots / radar plots), yearqs (times series), worms (spaghetti for time series)

# Common columns of the data.frames and what they are:

# msectrl - internal reference for HCR - not really used and not important
# hcrref - shorter HCR name for display
# hcrname - longer HCR name for display
# pi - internal reference for PI ("biomass", "pi1", "pi3" etc)
# piname - longer PI name for display ("SB/SBF=0", "PI 3: Catch")
# area - character string of area. Can be the region number, "all" for the biomass PIs, "total" and other categories (e.g. SKJ has "ps678") for catch indicators and NA for effort ones
# area_name - area name for display
# metric - subcategory of PI (e.g. pi "catch" can have metric of "catch" and "relative catch")
# period - Short, Medium, Long, Rest - Factor so order is OK

# Periodqs then has these additional columns
# X1. - X99. - Columns of the data - the quantiles
# upsidedown - Is a bigger value better (e.g. catch) FALSE, or worse (variability) TRUE

# yearqs has these additional columns
# X1. - X99. - Columns of the data - the quantiles
# year - the year

# worms has these additional columns
# year - year
# data - data
# wormid - unique combination of msectrl (or hcrref) and iter, e.g. 


#-----------------------------------------------------
# Fix column name
#periodqs$hcrref <- periodqs$hrcref
#yearqs$hcrref <- yearqs$msename
#yearqs$hcrname <- yearqs$msename
#yearqs$piname<- yearqs$name

#---------------------------------------------------------------------------------------------------

# Can we move all this down to the server side?
# Data processing
# Sort everything by HCR - probably should already be OK
periodqs <- periodqs[order(periodqs$hcrref),]
yearqs <- yearqs[order(yearqs$hcrref),]
worms <- worms[order(worms$hcrref),]

# Fix names of PI selector - remove PS note
pis_list <- as.character(unique(periodqs[!periodqs$upsidedown,"piname"]))
piselector <- as.list(pis_list)
pis_text <- unlist(lapply(strsplit(pis_list,"\n"),'[',1))
names(piselector) <- pis_text

# -------------------------------------------
# Stuff that could be in server()

# General plotting parameters
# Get these from the data rather than fixing them here
# Change lowercase to uppercase
short_term <- sort(unique(subset(worms, period=="Short")$year))
medium_term <- sort(unique(subset(worms, period=="Medium")$year))
long_term <- sort(unique(subset(worms, period=="Long")$year))
last_plot_year <- max(long_term)

# Options
first_plot_year <- 1985
# Maybe make this an option in the future?
pi_percentiles <- c(10,90)

# Trim out years for tight time series plots
yearqs <- subset(yearqs, year %in% first_plot_year:last_plot_year)
worms <- subset(worms, year %in% first_plot_year:last_plot_year)

# Careful with these - they are only used for plotting lines, NOT for calculating the indicators
lrp <- 0.2
trp <- 0.56
# For the worms - same worms for all plots
# This can be increased to 20 - maybe make as option?
nworms <- 5
# worms are a unique combination of OM and iter
# (same om / iter should be in all hcrs)
wormiters <- sample(unique(worms$iter), nworms)

# -------------------------------------------
# General settings for app

main_panel_width <- 10
side_panel_width <- 12 - main_panel_width 

# When is short, medium and long-term?
# Change lowercase to uppercase
short <- range(subset(yearqs, period=="Short")$year)
medium <- range(subset(yearqs, period=="Medium")$year)
long <- range(subset(yearqs, period=="Long")$year)

shorttext <- paste(short, collapse="-")
mediumtext <- paste(medium, collapse="-")
longtext <- paste(long, collapse="-")
yearrangetext <- paste("Short-term is: ", shorttext, ", medium-term is: ", mediumtext, " and long-term is: ", longtext,".",sep="")

# These character strings are notes that get put on the app in various places
# Some of this will need to be updated for SPA or new ones added
pi47text <- "Note that PIs 4 and 7 are for the purse seines in model areas 2, 3 and 5 only (excluding the associated purse seines in area 5.)"
relcatchtext <- "Note that the catches are relative to the average catch in that area grouping in the years 2013-2015."

# These might be OK
pi36text <- "The grouping for PIs 3 and 6 can be selected with the drop down menu on the left."
biotext <- "PIs 1, 8 and SB/SBF=0 are calculated over all model areas."
boxplottext <- "For box plots the box contains the 20-80 percentiles, the whiskers the 5-95 percentiles and the horizontal line is the median."
tabletext <- "The tables show the median indicator values in each time period. The values inside the parentheses are the 10-90 percentiles."
stabtext <- "Note that the stability can only be compared between time periods, not between areas or area groups, i.e. it is the relative stability in that area."

#----------------------------------------------------------------------------------------------------

# The User Interface
# Navbarpage inside a fluidpage?
# Pretty nasty but it means we get the power of the navparPage and can have common side panel
ui <- fluidPage(id="top",
  #tags$head(includeHTML("google-analytics.html")), 
  #titlePanel("Performance Indicators and Management Procedures Explorer"),
  sidebarLayout(
    sidebarPanel(width=side_panel_width,
      br(),
      img(src = "spc.png", height = 60),
      br(),
      br(),
      conditionalPanel(condition="input.nvp == 'about'",
        tags$html(
          tags$h1("PIMPLE"),
          tags$p("Performance Indicators and Management Procedures Explorer"),
          tags$footer(
            tags$p("version 0.4.0 Stick In A Five And Go"),
            tags$p("Copyright 2020 OFP SPC MSE Team."),
            tags$p("Distributed under the GPL 3")
          )
      )),
      # The input devices
      # It is messy because we only want some of them to appear on certain tabs
      # So there are lots of conditional statements
      conditionalPanel(condition="input.nvp == 'compareMPs' || input.nvp == 'explorePIs'",
        checkboxGroupInput(inputId = "hcrchoice", label="HCR selection", selected = unique(periodqs$hcrref), choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref))
      ),
      # PI choice - only shown in the compare PIs tab
      conditionalPanel(condition="input.nvp == 'compareMPs'",
        checkboxGroupInput(inputId = "pichoice", label="PI selection",choices = piselector, selected=sort(unique(periodqs$piname)))
      ),
      # Show spaghetti on the time series plots - do not show on the HCR tab
      # Currently shown on all explorePIs tabs - even if no timeseries plot - is this OK?
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab=='pibiomass' || input.pitab=='pi32' || input.pitab=='pi4')) || input.comptab == 'timeseries'",
        checkboxInput("showspag", "Show trajectories", value=FALSE) 
      ),
      
      # Catch choice - only in the catch and catch stability PI tabs
      # Careful with conditional
      conditionalPanel(condition="(input.nvp == 'compareMPs' || (input.nvp == 'explorePIs' && (input.pitab == 'pi3' || input.pitab == 'pi6')))",
        selectInput(inputId = "catchareachoice", label="Catch / effort grouping (PIs 3, 4, 6 and 7 only)", choices = list("All areas"="total", "DWFN total"="DWFN_total", "PICT total" = "PICT_total"), selected="total")
        # I had this as an additional choice - do you want absolute or relative catches
        # I ended up only using relative
        #selectInput(inputId = "catchrelchoice", label="Catch type", choices = list("Absolute catch"="catch", "Relative to average catch in 2013-15"="relative catch"), selected="catch")
      ),

      # Fix this for the number of areas
      # For selecting catch plots by area 
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab== 'pi32' || input.pitab== 'pi62' || input.pitab=='pi7'))",
        checkboxGroupInput(inputId = "areachoice", label="Area selection",choices = list("All areas" = "total", "Area 1" = "1", "Area 2" = "2", "Area 3"="3", "Area 4"="4","Area 5"="5", "DWFN total" = "DWFN_total", "PICT total"="PICT_total"), selected="total")
      ),
      
      # Conditional panel for CPUE tab
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab== 'pi4'))",
        checkboxGroupInput(inputId = "pi4areachoice", label="Area selection",choices = list("All areas" = "total", "DWFN total" = "DWFN_total", "PICT total"="PICT_total", "Area 2 (PICT)" = "pict2"), selected="total")
      ),

      # Select plot type by bar, box, time
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab== 'pi32' || input.pitab== 'pi4'))",
        radioButtons(inputId = "plotchoicebarboxtime", label="Plot selection",choices = list("Bar chart" = "median_bar", "Box plot" ="box", "Time series" = "time"), selected="median_bar")
      ),

      # Select plot type by bar, box
      # Need to include the NVP input too, because the the pitab input still has a value even if not seen
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab== 'pibiomass' || input.pitab== 'pi62' || input.pitab== 'pi7'))",
        radioButtons(inputId = "plotchoicebarbox", label="Plot selection",choices = list("Bar chart" = "median_bar", "Box plot" ="box"), selected="median_bar")
      ),

      # Stability or variability
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab== 'pi62'))",
        radioButtons(inputId = "stabvarchoice", label="Stability or variability",choices = list("Stability" = "stability", "Variability" ="variability"), selected="stability")
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
        #----------- Introduction page ----------------------------------
        #tabPanel("Introduction", value="intro",
        #         # How to use PIMPLE - Add to top
        #  fluidRow(column(8, 
        #    includeMarkdown("introtext/introduction.md")
        #  )),
        #  fluidRow(
        #    column(4, 
        #      includeMarkdown("introtext/barcharttext.md")
        #    ),
        #    column(8,
        #      plotOutput("demobarchart")
        #    )
        #  ),
        #  fluidRow(
        #    column(4, 
        #      includeMarkdown("introtext/boxplottext.md")
        #    ),
        #    column(8,
        #      plotOutput("demoboxplot")
        #    )
        #  ),
        #  fluidRow(
        #    column(4, 
        #      includeMarkdown("introtext/timeseriestext.md")
        #    ),
        #    column(8,
        #      plotOutput("demotimeseriesplot")
        #    )
        #  ),
        #  fluidRow(
        #    column(4, 
        #      includeMarkdown("introtext/radarplottext.md")
        #    ),
        #    column(8,
        #      plotOutput("demoradarplot", height="600px")
        #    )
        #  )
        #),

        #----------------------------------------------------------------------------
        tabPanel("Compare performance", value="compareMPs",
          tabsetPanel(id="comptab",
            tabPanel("Bar charts", value="bar",
              fluidRow(column(12,
                plotOutput("plot_bar_comparehcr", height="auto") # Needs function in the plotOutput() function
              )),
              fluidRow(column(12,
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text)
              ))
            ),
            tabPanel("Box plots", value="box",
              fluidRow(column(12,
                plotOutput("plot_box_comparehcr", height="auto") # Needs function in the plotOutput() function
              )),
              fluidRow(column(12,
                p(boxplottext),
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text)
              ))
            ),
            tabPanel("Time series plots", value="timeseries",
              fluidRow(column(12,
                p("Note that not all indicators have time series plots. The widths of the ribbons are the 10-90 percentiles. The dashed, black line is the median value."),
                plotOutput("plot_timeseries_comparehcr", height="auto") # height is variable
              ))
            ),
            tabPanel("Radar plots", value="radar",
              fluidRow(
                tags$span(title="Note that only the indicators for which 'bigger is better' are shown in the radar plots.",
                  plotOutput("plot_radar_comparehcr", height="600px")),
                p("Note that only the indicators for which 'bigger is better' are shown in the radar plots."),
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text)
              )
            ),
            tabPanel("Table", value="bigtable",
              tags$span(title="Median indicator values. The values inside the parentheses are the 10-90 percentiles",
                tableOutput("table_pis_short"),
                tableOutput("table_pis_medium"),
                tableOutput("table_pis_long"),
                p(tabletext),
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text)
              )
            )
          )
        ),
        # Start to put this tab back in, bit by bit
        tabPanel("Explore indicators", value="explorePIs",
          tabsetPanel(id="pitab",
          # --- SBSBF0 and PI 1 & 8 ---
            tabPanel("PI 1 & 8: Biomass",value="pibiomass",
              fluidRow(
                column(12,
                  # TS of SBSBF0
                  plotOutput("plot_ts_sbsbf0")
              )),
              fluidRow(
                column(6,
                  # Bar or box of SB/SBF0
                  plotOutput("plot_barbox_sbsbf0")
                ),
                column(6,
                  #  PI 1
                  plotOutput("plot_bar_problrp")
                )
              ),
              fluidRow(
                column(6,
                  # PI 8 - bar or box
                  plotOutput("plot_barbox_pi8")
              )),
              fluidRow(
                column(12,
                  p(yearrangetext),
                  p(biotext)
                )
              )
            ),

            tabPanel("PI 3: Relative catches by area",value="pi32",
              column(12, fluidRow(
                # Can't put text at end as not very fluid
                #p("Note that the catches are relative to the average catch in the years 2013-2015."),
                #p(yearrangetext),
                plotOutput("plot_pi3", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
                p(relcatchtext),
                p(yearrangetext)
              ))
            ),
          
            tabPanel("PI 4: Relative CPUE",value="pi4",
              column(12, fluidRow(
                # Can't put text at end as not very fluid
                #p("Note that the catches are relative to the average catch in the years 2013-2015."),
                #p(yearrangetext),
                plotOutput("plot_pi4", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
                p(relcatchtext),
                p(yearrangetext)
              ))
            ),

            tabPanel("PI 6: Catch stability by area",value="pi62",
              column(12, fluidRow(
                plotOutput("plot_pi6", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
                p(relcatchtext),
                p(yearrangetext),
                p(stabtext)
              ))
            ),

            # *** PI 7: Relative effort variability***
            tabPanel("PI 7: Effort stability",value="pi7",
            #  #column(6, fluidRow(
            #  #  plotOutput("plot_bar_pi7stab"),
            #  #  plotOutput("plot_bar_pi7var")
            #  #)),
            #  #column(6, fluidRow(
            #  #  plotOutput("plot_box_pi7stab"),
            #  #  plotOutput("plot_box_pi7var")
            #  #)),
              column(12, fluidRow(
                plotOutput("plot_pi7", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
              p("Note that the effort only includes purse seines in model regions 2, 3 and 5, excluding the associated purse seines in region 5. Relative effort is the effort relative to that in 2012."),
              p(yearrangetext),
              p(pi47text)
              )) # end of column
            ) # end of tabPanel
          
          
          ) # End of tabset panel  
        ), # End of Explore Indicators tab
        
        tabPanel("About", value="about",
          fluidRow(column(8, 
            #includeMarkdown("introtext/introduction.md")
            spc_about()
          ))
        )
      )
    )
  )
)

#---------------------------------------------------------------------------------------------------

# Server function
server <- function(input, output, session) {

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
  # Intro plots
  
output$demobarchart <- renderPlot({
  # Demo bar plot
  pi_choices <- c("pi3")
  metric_choices <- c("relative catch")
  area_choices <- "total"
  dat <- dplyr::filter(periodqs, period != "Rest" & pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
  dat$hcrname <- as.character(dat$hcrname)
  dat$hcrref <- as.character(dat$hcrref)
  hcr_choices <- c("HCR 1", "HCR 4")
  p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type="median_bar")
  p <- p + ggplot2::ylim(0,NA)
  p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
  return(p)
})

output$demoboxplot <- renderPlot({
  # Demo bar plot
  pi_choices <- c("pi3")
  metric_choices <- c("relative catch")
  area_choices <- "total"
  dat <- dplyr::filter(periodqs, period != "Rest" & pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
  dat$hcrname <- as.character(dat$hcrname)
  dat$hcrref <- as.character(dat$hcrref)
  hcr_choices <- c("HCR 1", "HCR 4")
  p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type="box")
  p <- p + ggplot2::ylim(0,NA)
  p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
  return(p)
})

output$demotimeseriesplot <- renderPlot({
  # Demo time series plot
  hcr_choices <- c("HCR 1", "HCR 4")
  pi_choices <- c("pi3")
  metric_choices <- c("relative catch")
  area_choices <- "total"
  dat <- dplyr::filter(yearqs, pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
  wormdat <- dplyr::filter(worms, pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
  p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=FALSE, percentile_range = pi_percentiles)
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("PI 3: Catch (rel. to 2013-2015)")
  return(p)
})

output$demoradarplot <- renderPlot({
  hcr_choices <- c("HCR 1", "HCR 4")
  catch_area_choice <- "total"
  other_area_choice <- c(as.character(NA), "all")
  catch_rel_choice <- "relative catch"
  metric_choices <- c(catch_rel_choice, "relative catch stability", "SBSBF0", "relative effort stability", "relative cpue")
  # Drop SB/SBF=0 and Size based one 
  pi_choices <- unique(periodqs[!periodqs$upsidedown,"piname"])
  not_radar_pinames <- c("SB/SBF=0")
  pi_choices <- pi_choices[!(pi_choices %in% not_radar_pinames)]
  # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
  dat <- subset(periodqs, ((pi %in% c("pi3","pi6") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6")) & area %in% other_area_choice)) & period == "Short" & piname %in% pi_choices & metric %in% metric_choices)
  scaling_choice <- "scale"
  p <- myradar(dat=dat, hcr_choices=hcr_choices, scaling_choice)
  return(p)
})


  #-------------------------------------------------------------------
  # Comparison plots
  no_facets_row <- 2#2
  height_per_pi <- 300
  height_per_area <- 300

  # Bar or box plot - facetting on PI
  plot_barbox_comparehcr <- function(plot_type="median_bar"){
    rPlot <- renderPlot({

      # It gets complicated because each PI has suboption in area, metric columns
      # biomass: area == all and metric == SBSBF0
      # pi1: area == all
      # pi3: area == catchareachoice and metric == 'catch' or 'relative catch'
      # pi4: metric == relative_cpue
      # pi6: area == catchareachoice and metric == 'catch stability' or 'catch variability' - just stab
      # pi7: metric == 'relative effort stability' or 'relative effort variability' - just stab
      # pi8: area == all

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
      metric_choices <- c(catch_rel_choice, "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
      # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
      # Important but complicated
      dat <- subset(periodqs, ((pi %in% c("pi3","pi6","pi7","pi4") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6", "pi7", "pi4")) & area %in% other_area_choice)) & period != "Rest" & piname %in% pi_choices & metric %in% metric_choices)

      # Need to hack pi1 so that all quantiles = X50., else NA
      # Careful! If pi1 not in dat, error
      # Probably better to do this at the top
      if("pi1" %in% dat$pi){
        dat[dat$pi=="pi1",c("X1.", "X5.", "X10.", "X15.", "X20.", "X80.", "X85.", "X90.","X95.","X99.")] <- dat[dat$pi=="pi1","X50."]
      }
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylim(0,NA)
      p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
      # Add LRP and TRP lines
      # Only if SB/SBF=0 is in dat
      if ("SB/SBF=0" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
        p <- p + ggplot2::geom_hline(data=data.frame(yint=trp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      p <- p + ggplot2::facet_wrap(~piname, scales="free", ncol=no_facets_row)
      return(p)
    },
      height=function(){
        return(max(height_per_pi*1.5, (height_per_pi * ceiling(length(input$pichoice) / no_facets_row))))
      }
    )
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
    
    catch_area_choice <- input$catchareachoice
    other_area_choice <- c(as.character(NA), "all")
    catch_rel_choice <- "relative catch"
    metric_choices <- c(catch_rel_choice, "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    
    # We have 8 PIs - but not all are appropriate for a radar plot as bigger is not better.
    # Drop SB/SBF=0 and Size based one 
    not_radar_pinames <- c("SB/SBF=0")
    pi_choices <- pi_choices[!(pi_choices %in% not_radar_pinames)]
    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }
    # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
    dat <- subset(periodqs, ((pi %in% c("pi3","pi6", "pi7", "pi4") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6", "pi7", "pi4")) & area %in% other_area_choice)) & period != "Rest" & piname %in% pi_choices & metric %in% metric_choices)

    scaling_choice <- "scale"
    p <- myradar(dat=dat, hcr_choices=hcr_choices, scaling_choice)
    return(p)
  })

  # Time series comparisons
  # Which TS to plot
  pinames_ts <- c("SB/SBF=0", "PI 3: Catch", "PI 4: Cpue", "PI 8: Proximity to TRP")
  output$plot_timeseries_comparehcr <- renderPlot({
    show_spaghetti <- input$showspag
    hcr_choices <- input$hcrchoice

    # Only these ones allowed
    pi_choices <- input$pichoice
    pi_choices <- pi_choices[pi_choices %in% pinames_ts]

    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }

    catch_area_choice <- input$catchareachoice
    other_area_choice <- c(as.character(NA), "all")
    catch_rel_choice <- "relative catch"
    metric_choices <- c(catch_rel_choice, "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    
    # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
    dat <- subset(yearqs, ((pi %in% c("pi3","pi6","pi4","pi7") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6","pi4","pi7")) & area %in% other_area_choice)) & piname %in% pi_choices & metric %in% metric_choices)
    wormdat <- subset(worms, ((pi %in% c("pi3","pi6","pi4","pi7") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6","pi4","pi7")) & area %in% other_area_choice)) & piname %in% pi_choices & metric %in% metric_choices & iter %in% wormiters)
    
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



  get_pi_table <- function(period_choice="Short"){
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    catch_area_choice <- input$catchareachoice
    other_area_choice <- c(as.character(NA), "all")
    catch_rel_choice <- "relative catch"
    metric_choices <- c(catch_rel_choice, "catch stability", "SBSBF0", "relative effort stability", "relative cpue")

    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }
    # pi3 and pi6 areas are given by user choice, other pi areas are all or NA
    dat <- subset(periodqs, hcrref %in% hcr_choices & ((pi %in% c("pi3","pi6","pi4","pi7") & area == catch_area_choice) | (!(pi %in% c("pi3", "pi6","pi4","pi7")) & area %in% other_area_choice)) & period == period_choice & piname %in% pi_choices & metric %in% metric_choices)
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

  output$plot_barbox_sbsbf0 <- renderPlot({
    plot_type <- input$plotchoicebarbox
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

  output$plot_barbox_pi8 <- renderPlot({
    plot_type <- input$plotchoicebarbox
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    dat <- subset(periodqs, period != "Rest" & pi=="pi8" & metric=="SBSBF0" & area=="all") 
    p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
    # Average closeness to TRP
    p <- p + ggplot2::ylab("PI 8: Proximity to TRP") + ggplot2::ylim(c(0,1))
    return(p)
  })

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

  # For exploring the catches in different regions
  output$plot_pi3 <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }

    ylabel <- "PI 3: Catch (rel. to 2013-2015)"
    plot_choice <- input$plotchoicebarboxtime
    area_choice <- input$areachoice
    if(length(area_choice) < 1){
      return()
    }
    # Choose if relative to year X
    #catch_rel_choice <- input$catchrelchoice # or relative catch
    catch_rel_choice <- "relative catch"
    #catch_rel_choice <- "catch"

    if (plot_choice %in% c("median_bar","box")){
      # The individual areas are also split by fishery group" all, DWFN and PICT (see set column)
      # To start with, just go with all
      dat <- subset(periodqs, period != "Rest" & pi=="pi3" & area %in% area_choice & metric == catch_rel_choice & set %in% c("total", "area", "PICT", "DWFN")) 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice)
      p <- p + ggplot2::ylab(ylabel) + ggplot2::ylim(c(0,NA))
      p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    }

    if(plot_choice == "time"){
      show_spaghetti <- input$showspag
      dat <- subset(yearqs, pi=="pi3" & area %in% area_choice & metric == catch_rel_choice & set %in% c("total", "area")) 
      wormdat <- subset(worms, pi=="pi3" & area %in% area_choice & metric == catch_rel_choice & iter %in% wormiters & set %in% c("total", "area", "PICT", "DWFN")) 
      p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
      p <- p + ggplot2::ylab(ylabel)
      p <- p + ggplot2:: ylim(c(0,NA))
      # Axes limits set here or have tight?
      p <- p + ggplot2::facet_wrap(~area_name, scales="free", ncol=1)
      p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    }
    return(p)
  }, height=function(){
    if(input$plotchoicebarboxtime=="time"){return(max(height_per_area*1.5, (height_per_area * length(input$areachoice))))}
    if(input$plotchoicebarboxtime %in% c("median_bar","box")){return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$areachoice) / no_facets_row))))}
  })
  
  # For exploring the CPUE in different regions
  output$plot_pi4 <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }

    ylabel <- "PI 4: CPUE (rel. to 2013 + 8%)"
    plot_choice <- input$plotchoicebarboxtime
    area_choice <- input$pi4areachoice
    if(length(area_choice) < 1){
      return()
    }
    
    if (plot_choice %in% c("median_bar","box")){
      # The individual areas are also split by fishery group" all, DWFN and PICT (see set column)
      # To start with, just go with all
      dat <- subset(periodqs, period != "Rest" & pi=="pi4" & area %in% area_choice) 
      if("pict2" %in% area_choice){
        p2_periodqs <- subset(periodqs, area=="2" & set =="PICT_area" & period != "Rest" & pi=="pi4")
        dat <- rbind(dat, p2_periodqs)
      }
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice)
      p <- p + ggplot2::ylab(ylabel) + ggplot2::ylim(c(0,NA))
      p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    }

    if(plot_choice == "time"){
      show_spaghetti <- input$showspag
      dat <- subset(yearqs, pi=="pi4" & area %in% area_choice) 
      wormdat <- subset(worms, pi=="pi4" & area %in% area_choice & iter %in% wormiters) 
      if("pict2" %in% area_choice){
        p2_yearqs <- subset(yearqs, area=="2" & set =="PICT_area" & pi=="pi4")
        dat <- rbind(dat, p2_yearqs)
        p2_worms <- subset(worms, area=="2" & set =="PICT_area" & pi=="pi4" & iter %in% wormiters)
        wormdat <- rbind(wormdat, p2_worms)
      }
      p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
      p <- p + ggplot2::ylab(ylabel)
      p <- p + ggplot2:: ylim(c(0,NA))
      p <- p + geom_hline(aes(yintercept=1.0), linetype=2)
      # Axes limits set here or have tight?
      p <- p + ggplot2::facet_wrap(~area_name, scales="free", ncol=1)
      p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    }
    return(p)



  }, height=function(){
    if(input$plotchoicebarboxtime=="time"){return(max(height_per_area*1.5, (height_per_area * length(input$pi4areachoice))))}
    if(input$plotchoicebarboxtime %in% c("median_bar","box")){return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$pi4areachoice) / no_facets_row))))}
  })
  
  output$plot_pi6 <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    plot_choice <- input$plotchoicebarbox
    area_choice <- input$areachoice
    # Need additional option of stability or variability
    if(length(area_choice) < 1){
      return()
    }

    # Options:
    #   bar or box - handled in the plot_choice
    #   stab or variability
    stabvar_choice <- input$stabvarchoice
    #metric_choice <-  paste("relative catch", stabvar_choice, sep=" ") # or stability
    metric_choice <-  paste("catch", stabvar_choice, sep=" ") # or stability
    ylabel <- paste("PI 6: ",   paste0(toupper(substr(stabvar_choice, 1, 1)), substr(stabvar_choice, 2, nchar(stabvar_choice))), " of catch", sep="")
    dat <- subset(periodqs, period != "Rest" & pi=="pi6" & area %in% area_choice & metric == metric_choice & set %in% c("total", "area", "PICT", "DWFN")) 
    p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice)
    p <- p + ggplot2::ylab(ylabel) + ggplot2::ylim(c(0,NA))
    p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    return(p)
  }, height=function(){
    return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$areachoice) / no_facets_row))))
  })

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
  # PI 7: Effort variability and stability
  output$plot_pi7 <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    plot_choice <- input$plotchoicebarbox
    area_choice <- input$areachoice
    # Need additional option of stability or variability
    if(length(area_choice) < 1){
      return()
    }
    
    # Options:
    #   bar or box - handled in the plot_choice
    #   stab or variability
    stabvar_choice <- input$stabvarchoice
    metric_choice <-  paste("relative effort", stabvar_choice, sep=" ") # or stability
    ylabel <- paste("PI 7: ",   paste0(toupper(substr(stabvar_choice, 1, 1)), substr(stabvar_choice, 2, nchar(stabvar_choice))), " of relative effort", sep="")
    dat <- subset(periodqs, period != "Rest" & pi=="pi7" & area %in% area_choice & metric == metric_choice & set %in% c("total", "area", "PICT", "DWFN")) 
    #subset(periodqs, period != "Rest" & pi=="pi7" & area %in% area_choice & metric == metric_choice) 
    p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice)
    p <- p + ggplot2::ylab(ylabel) + ggplot2::ylim(c(0,NA))
    p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    return(p)
  }, height=function(){
    return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$areachoice) / no_facets_row))))
  })

}

# Run the app
shinyApp(ui, server)
