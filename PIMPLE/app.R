#--------------------------------------------------------------
# PIMPLE
# Performance Indicators and Management Procedures Explorer
# Main app
# Updated for SC17 (July 2021)
# Updated for SMD 2022 (Started March 2022)

# Copyright 2020 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC
# Soundtrack: People and Industry by Warrington-Runcorn New Town Development Plan
#--------------------------------------------------------------

#rsconnect::deployApp("C:/Work/ShinyMSE/ofp-sam-amped/PIMPLE") 
# Load packages

# Drop AMPLE - make independent
# Note: get AMPLE this from github - then comment out before uploading to server
# Make sure that the branch is correct
#devtools::install_github("PacificCommunity/ofp-sam-amped/AMPLE", ref="SC17dev")
#library(AMPLE)


# Move to data.table if any of the processes are taking too long?
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(markdown)

source("funcs.R")
source("plots.R")

# Load the indicator data - including Kobe and Majuro data
data_files <- load("data/SMD2022_results.Rdata")

#------------------------------------------------------------------------------------------------------
# Additional data processing

# Data for the HCR histogram plots
# Move to data.table instead of dplyr?
breaks <- seq(from=0,to=2,by=0.05)
freqs <- cut(hcr_points$scaler, breaks, labels=FALSE)
hcr_points$bin <- breaks[freqs]
histodat <- dplyr::group_by(hcr_points, hcrref, hcrname, period, bin)
histodat <- dplyr::summarise(histodat, sum=dplyr::n())
nobs <- dplyr::group_by(hcr_points,hcrref, hcrname, period)
nobs <- dplyr::summarise(nobs, tnobs=dplyr::n())
histodat <- dplyr::left_join(histodat, nobs)
histodat$prop <- histodat$sum / histodat$tnobs
# Correct the bin so that we use the right hand side of the bin from cut() - means effort of 1 shows an effort of 1
histodat$bin <- histodat$bin + 0.05

# General plotting parameters
short_term <- sort(unique(subset(worms, period=="Short")$year))
medium_term <- sort(unique(subset(worms, period=="Medium")$year))
long_term <- sort(unique(subset(worms, period=="Long")$year))
last_plot_year <- max(long_term)
first_plot_year <- 1990
inner_percentiles <- c(25,75) # Used in tables
outer_percentiles <- c(10,90)
#and time series plots, to match other SPC plots. These need to be in the data

# Trim out years for tight time series plots
yearqs <- subset(yearqs, year %in% first_plot_year:last_plot_year)
worms <- subset(worms, year %in% first_plot_year:last_plot_year)

# For the worms - same worms for all plots
nworms <- 5
wormiters <- sample(unique(worms$iter), nworms)

# Careful with these - they are only used for plotting lines, NOT for calculating the indicators
lrp <- 0.2
trp1 <- 0.5
trp2 <- 0.425 # SB/SBF0 TRP from the 2019 assessment

# Find common iters between HCRs - used for HCR plots so we can directly compare
# Not all HCRs have the same iterations
common_iters <- Reduce(intersect, split(hcr_points$iter, hcr_points$hcrname, drop=TRUE))

# Sort everything by HCR - should already have been done in the PI preparation script
# Could do this in preparation script 
histodat <- histodat[order(histodat$hcrref),]
periodqs <- periodqs[order(periodqs$hcrref),]
yearqs <- yearqs[order(yearqs$hcrref),]
worms <- worms[order(worms$hcrref),]
hcr_points <- hcr_points[order(hcr_points$hcrref),]
hcr_shape <- hcr_shape[order(hcr_shape$hcrref),]
majuro_summary_tabs <- lapply(majuro_summary_tabs, function(x) x[order(x$HCR),])
kobe_summary_tabs <- lapply(kobe_summary_tabs, function(x) x[order(x$HCR),])

# Fix names of PI selector - used for selecting desired PIs - need to remove PS note
# piselector is the one used in the main comparison tab
# i.e. not used in the more detailed analysis
pis_list <- unique(periodqs[,"piname"])
# Drop the variability ones (they point the wrong way and are alternatives to the stability indicators)
pis_list <- pis_list[!grepl("variability", pis_list)]
# Also drop SB the Proximity 0.5 one until we fix the TRP selector, only look at relative catches
pis_list <- pis_list[!(pis_list %in% c("SB", "PI 8: Proximity to SB/SBF=0 (0.5)", "PI 3: Catch"))] 
piselector <- as.list(pis_list)
pis_text <- unlist(lapply(strsplit(pis_list,"\n"),'[',1))
names(piselector) <- pis_text

# -------------------------------------------
# General settings for app

# Whole thing is 12 units wide
main_panel_width <- 10
side_panel_width <- 12 - main_panel_width 

# Get ranges of short, medium and long to make labels with
short <- range(short_term)
medium <- range(medium_term)
long <- range(long_term)

# Text notes for the app
shorttext <- paste(short, collapse="-")
mediumtext <- paste(medium, collapse="-")
longtext <- paste(long, collapse="-")
yearrangetext <- paste("Short-term is: ", shorttext, ", medium-term is: ", mediumtext, " and long-term is: ", longtext,".",sep="")
pi47text <- "Note that PIs 4 and 7 are for the purse seines in model areas 2, 3 and 5 only (excluding the associated purse seines in area 5.)"
pi36text <- "The grouping for PIs 3 and 6 can be selected with the drop down menu on the left."
biotext <- "PIs 1, 82 and SB/SBF=0 are calculated over all model areas."
relcatchtext <- "Note that the catches are relative to the average catch in that area grouping in the years 2013-2015."
barchartplottext <- "The height of each bar shows the median expected value. Note that the bar charts do not show any uncertainty which can be important (see the box plots)."
boxplottext <- "For box plots the box contains the 50th percentile, the whiskers show the 80th percentile and the horizontal line is the median. The wider the range, the less certain we are about the expected value."
tabletext <- "The tables show the median indicator values in each time period. The values inside the parentheses are the 80th percentile range."
timeseriesplottext <- "The outer ribbons show the 80th percentile range and the inner ribbons show the 50th percentile range. The dashed, black line is the median value."
timeseriesplottext2 <-  "The dashed vertical lines show, from left, the start of the MSE evaluation with three years of 2012 assumptions, the start of the HCR operating with the short-, medium- and long-term periods."
stabtext <- "Note that the stability can only be compared between time periods, not between areas or area groups, i.e. it is the relative stability in that area."
sbsbf02012text <- "On the SB/SBF=0 plot, the lower dashed line is the Limit Reference Point and the upper dashed line is the mean SB/SBF=0 in 2012."

#----------------------------------------------------------------------------------------------------
# The actual app!

# Navbarpage inside a fluidpage?
# Pretty nasty but it means we get the power of the navparPage and can have common side panel
ui <- fluidPage(id="top",
  tags$head(includeHTML("google-analytics.html")),  # google analytics
  #titlePanel("Performance Indicators and Management Procedures Explorer"),
  sidebarLayout(
    sidebarPanel(width=side_panel_width,
      br(),
      img(src = "spc.png", height = 60),
      br(),
      br(),
      #-----------------------------------------------------------------------
      # A shit-tonne of conditional panels to make the sidebar do what we want
      #-----------------------------------------------------------------------
      conditionalPanel(condition="input.nvp == 'about'",
        tags$html(
          tags$h1("PIMPLE"),
          tags$p("Performance Indicators and Management Procedures expLorEr"),
          tags$footer(
            tags$p("version 0.5.0 Mork n Mindy"),
            tags$p("Copyright 2021 OFP SPC MSE Team."),
            tags$p("Distributed under the GPL 3")
          )
      )), # End of about condition
      # HCR selection - can select multiples
      # Only for the main Compare MPs tab, the MPs tab and SOME of the explorePIs tabs
      conditionalPanel(condition="input.nvp == 'compareMPs' || (input.nvp == 'explorePIs' && (input.pitab == 'pi3' || input.pitab == 'pi6')) || input.nvp == 'mps'",
        checkboxGroupInput(inputId = "hcrchoice", label="HCR selection", selected = unique(periodqs$hcrref), choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref))
      ),
      # PI choice - only shown in the compare PIs tab
      conditionalPanel(condition="input.nvp == 'compareMPs'",
        checkboxGroupInput(inputId = "pichoice", label="PI selection",choices = piselector, selected=sort(pis_list))
      ),
      # Show spaghetti on the time series plots - only show when you get time series plots
      conditionalPanel(condition="(input.nvp == 'explorePIs' && input.pitab=='pi3') || input.comptab == 'timeseries'",
        checkboxInput("showspag", "Show trajectories", value=FALSE) 
      ),
      # Catch grouping choice (all, PS in 678, PL in 1234) - show in compare MPs 
      #conditionalPanel(condition="(input.nvp == 'compareMPs' || (input.nvp == 'explorePIs' && (input.pitab == 'pi3' || input.pitab == 'pi6')))",
      conditionalPanel(condition="input.nvp == 'compareMPs'",
        selectInput(inputId = "catchareachoice", label="Catch grouping (PIs 3 & 6 only)", choices = list("All areas"="total", "Purse seines in areas 6,7 & 8"="ps678", "Pole & line in areas 1,2,3 & 4" = "pl_jp"), selected="total")
      ),
      # Selecting catch by area 
      conditionalPanel(condition="input.nvp == 'explorePIs' && (input.pitab== 'pi3' || input.pitab== 'pi6')",
        checkboxGroupInput(inputId = "areachoice", label="Area selection",choices = list("All areas" = "total", "Purse seines in areas 6,7 & 8" = "ps678", "Pole & line in areas 1,2,3 & 4" = "pl_jp", "Area 1" = "1", "Area 2" = "2", "Area 3"="3", "Area 4"="4","Area 5"="5","Area 6"="6","Area 7"="7","Area 8"="8" ), selected="total")
      ),
      
      # Select plot type by bar, box or time
      conditionalPanel(condition="input.nvp == 'explorePIs' && input.pitab== 'pi3'",
        radioButtons(inputId = "plotchoicebarboxtime", label="Plot selection",choices = list("Bar chart" = "median_bar", "Box plot" ="box", "Time series" = "time"), selected="median_bar")
      ),
      # Select plot type by bar or box (Note - need to include the NVP input as the the pitab input still has value even if not seen)
      conditionalPanel(condition="input.nvp == 'explorePIs' && input.pitab== 'pi6'",
        radioButtons(inputId = "plotchoicebarbox", label="Plot selection",choices = list("Bar chart" = "median_bar", "Box plot" ="box"), selected="median_bar")
      ),
      # Stability or variability
      conditionalPanel(condition="input.nvp == 'explorePIs' && input.pitab== 'pi6'",
        radioButtons(inputId = "stabvarchoice", label="Stability or variability",choices = list("Stability" = "stability", "Variability" ="variability"), selected="stability")
      ),
      # In Management Procedures tab, show the points and trajectories
      # Change false to true to show performance options (can hide from users)
      conditionalPanel(condition="((input.nvp == 'mps') && false)",
        #p("Secret HCR performance options"),
        # If this is checked the other ones should show
        checkboxInput("showpoints", "Show points", value=FALSE),
        conditionalPanel(condition="input.showpoints==true",
          checkboxInput("showpath", "Show paths", value=FALSE) 
        ),
        conditionalPanel(condition="input.showpoints==true",
          numericInput("mppointiters", label = "No. of replicates (randomly picked)", value = 5, min=1, max=length(common_iters), step=1)
        )
      ),
      # Kobe plot HCR selection - one at a time only
      #conditionalPanel(condition="input.nvp == 'majurokobeplot'",
      conditionalPanel(condition="input.pitab == 'majurokobeplot'",
        radioButtons(inputId = "hcrchoicekobe", label="HCR selection", selected = unique(periodqs$hcrref)[1], choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref)),
        radioButtons(inputId = "majurokobe", label="Kobe or Majuro plot", selected = "Kobe", choiceNames = c("Kobe", "Majuro"), choiceValues = c("Kobe", "Majuro"))
      )
    ),
    
    #---------------------------------------------
    # Main panel
    #---------------------------------------------
    mainPanel(width=main_panel_width,
      tags$style(type="text/css", "body {padding-top: 70px;}"), # padding - as we use fixed-top for position, applies to all tabs
      navbarPage(id="nvp",
        collapsible=TRUE,  # Should help if using small screens like tablets
        windowTitle="PIMPLE",
        position="fixed-top",
        title="Performance Indicators and Management Procedures Explorer",
        
        #--------------------------------------------
        # Introduction page 
        #--------------------------------------------
        
        tabPanel("Introduction", value="intro",
          # How to use PIMPLE - Add to top
          fluidRow(column(8, 
            includeMarkdown("introtext/introduction.md")
          )),
          fluidRow(
            column(4, 
              includeMarkdown("introtext/barcharttext.md")
            ),
            column(8,
              plotOutput("demobarchart")
            )
          ),
          fluidRow(
            column(4, 
              includeMarkdown("introtext/boxplottext.md")
            ),
            column(8,
              plotOutput("demoboxplot")
            )
          ),
          fluidRow(
            column(4, 
              includeMarkdown("introtext/timeseriestext.md")
            ),
            column(8,
              plotOutput("demotimeseriesplot")
            )
          )
        ), # End of Intro tab

        #----------------------------------------------------------------------------
        # Comparing performance across all indicators
        #----------------------------------------------------------------------------
        tabPanel("Compare performance", value="compareMPs",
          tabsetPanel(id="comptab",
            tabPanel("Bar charts", value="bar",
              fluidRow(column(12,
                p(barchartplottext),
                plotOutput("plot_bar_comparehcr", height="auto") # Needs function in the plotOutput() function
              )),
              fluidRow(column(12,
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text),
                p(sbsbf02012text)
              ))
            ),
            tabPanel("Box plots", value="box",
              fluidRow(column(12,
                p(boxplottext),
                plotOutput("plot_box_comparehcr", height="auto") # Needs function in the plotOutput() function
              )),
              fluidRow(column(12,
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text),
                p(sbsbf02012text)
              ))
            ),
            tabPanel("Time series plots", value="timeseries",
              fluidRow(column(12,
                p("Note that not all indicators have time series plots."),
                p(timeseriesplottext),
                p(timeseriesplottext2),
                plotOutput("plot_timeseries_comparehcr", height="auto") # height is variable
              )),
              fluidRow(column(12,
                p(pi47text),
                p(biotext),
                p(pi36text),
                p(sbsbf02012text)
              ))
            ),
            tabPanel("Table", value="bigtable",
              tags$span(title="Median indicator values. The values inside the parentheses are the 80th percentile range.",
                p(tabletext),
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
        ), # End of Compare PIs tab
        #----------------------------------------------------------------------------
        # Tabs for exploring specific indicators in more detail
        #----------------------------------------------------------------------------
        tabPanel("Other indicators", value="explorePIs",
          tabsetPanel(id="pitab",
            ## --- SBSBF0 and PI 1 & 8 ---
            # -------- Drop this - adds nothing -----------
            #tabPanel("PI 1 & 8: Biomass",value="pibiomass",
            #  fluidRow(
            #    column(12,
            #      # TS of SBSBF0
            #      plotOutput("plot_ts_sbsbf0")
            #  )),
            #  fluidRow(
            #    column(6,
            #      # Bar or box of SB/SBF0
            #      plotOutput("plot_barbox_sbsbf0")
            #    ),
            #    column(6,
            #      #  PI 1
            #      plotOutput("plot_bar_problrp")
            #    )
            #  ),
            #  fluidRow(
            #    column(6,
            #      # PI 8 - bar or box
            #      #plotOutput("plot_barbox_pi8")
            #      plotOutput("plot_barbox_pi82")
            #  )),
            #  fluidRow(
            #    column(12,
            #      p(yearrangetext),
            #      p(biotext),
            #      p(sbsbf02012text)
            #    )
            #  )
            #),

            # *** PI 3: Catch based ones ***
            tabPanel("PI 3: Relative catches by area",value="pi3",
              column(12, fluidRow(
                plotOutput("plot_pi3", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
                p(relcatchtext),
                p(yearrangetext)
              ))
            ),
            # *** PI 4: Relative CPUE***
            # -------- Drop this - adds nothing -----------
            #tabPanel("PI 4: Relative CPUE",value="pi4",
            #  column(12, fluidRow(
            #    plotOutput("plot_ts_relcpue")
            #  )),
            #  column(6, fluidRow(
            #    plotOutput("plot_bar_relcpue")
            #  )),
            #  column(6, fluidRow(
            #    plotOutput("plot_box_relcpue")
            #  )),
            #  p("Note that the CPUE only includes purse seines in model regions 2, 3 and 5, excluding the associated purse seines in region 5. Relative CPUE is the CPUE relative to that in 2012."),
            #  p(yearrangetext),
            #  p(pi47text)
            #),
            # *** PI 6: Catch stability ***
            tabPanel("PI 6: Catch stability by area",value="pi6",
              column(12, fluidRow(
                p("It is possible to see the variability instead of the stability using the checkbox on the left."),
                plotOutput("plot_pi6", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
                p(relcatchtext),
                p(yearrangetext),
                p(stabtext)
              ))
            ), # End of PI6 Catch stability tab
            
            tabPanel(title="Majuro and Kobe plots", value="majurokobeplot",
              fluidRow(column(12, 
                p("Majuro or Kobe plots for a single HCR (chosen from the input menu on the left) in each time period." ),
                p(paste(yearrangetext, "The historical period covers 2000-2018."),sep=" "),
                p("The contour colours show the approximate probability of being in that area. Each coloured band represents a 25% chance, e.g. there is a 25% chance of being in the blue zone and a 25% chance of being in the yellow zone etc. A random sample of points are shown as an illustration."),
                p("The percentage of points falling in each plot quadrant is also shown in the small white box. This represents the chance of being in that quadrant in that time period. A table of the percentages for all HCRs is next to the plot.")
              )),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_short")),
                       column(6, tableOutput("table_kobesummary_short"))),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_medium")),
                       column(6, tableOutput("table_kobesummary_medium"))),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_long")),
                       column(6, tableOutput("table_kobesummary_long"))),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_hist")),
                       column(6, tableOutput("table_kobesummary_hist")))
            ) # End of Majuro Kobe tab
            
            # *** PI 7: Relative effort variability***
            # -------- Drop this - adds nothing -----------
            #tabPanel("PI 7: Effort stability",value="pi7",
            #  column(6, fluidRow(
            #    plotOutput("plot_bar_pi7stab"),
            #    plotOutput("plot_bar_pi7var")
            #  )),
            #  column(6, fluidRow(
            #    plotOutput("plot_box_pi7stab"),
            #    plotOutput("plot_box_pi7var")
            #  )),
            #  p("Note that the effort only includes purse seines in model regions 2, 3 and 5, excluding the associated purse seines in region 5. Relative effort is the effort relative to that in 2012."),
            #  p(yearrangetext),
            #  p(pi47text)
            #)
          )                                
        ), # End of Other indicators tab                                
        #------------------------------------------
        # The Management Procedures
        #------------------------------------------
        tabPanel(title="Management procedures", value="mps",
          column(12,
            fluidRow(            
              p("Currently all the candidate management procedures have the same estimation method (an 8-region MULTIFAN-CL stock assessment model)."),
              p("This means that we are only comparing the performance of the HCRs. However, this may not always be the case."),
              p("The current HCRs use a value of estimated depletion (SB/SBF=0) to set a multiplier. This multipler is applied to the catch or effort in 2012 for each fishery to set a new catch or effort limit for the next time period."),
            #tags$span(title="Shape of the  HCRs under consideration",
              plotOutput("plot_hcrshape",  height="600px")),
        #    tags$span(title="Histograms of which parts of the HCRs were active during the evaluations",
              fluidRow(
                p("The histograms below shows how often each HCR set a particular value for the catch or effort scalar in each time period."),
              # Uncomment the following line to show the histograms
                plotOutput("plot_hcrhistograms")
              )
          )
        ), # End of MPs tab

        #------------------------------------------
        # Kobe . Majuro plot
        #------------------------------------------
        # Show plot of only a single HCR but table of all of them? - Feels awkward
        #tabPanel(title="Majuro and Kobe plots", value="majurokobeplot",
        #         fluidRow(column(12, 
        #             p("Majuro or Kobe plots for a single HCR (chosen from the input menu on the left) in each time period." ),
        #             p(paste(yearrangetext, "The historical period covers 2000-2018."),sep=" "),
        #             p("The contour colours show the approximate probability of being in that area. Each coloured band represents a 25% chance, e.g. there is a 25% chance of being the blue zone and a 25% chance of being in the yellow zone etc. A random sample of points are shown as an illustration."),
        #             p("The percentage of points falling in each plot quadrant is also shown in the small white box. This represents the chance of being in that quadrant in that time period. A table of the percentages for all HCRs is next to the plot.")
        #         )),
        #         fluidRow(column(6, plotOutput("plot_kobe_ptables_short")),
        #                  column(6, tableOutput("table_kobesummary_short"))),
        #         fluidRow(column(6, plotOutput("plot_kobe_ptables_medium")),
        #                  column(6, tableOutput("table_kobesummary_medium"))),
        #         fluidRow(column(6, plotOutput("plot_kobe_ptables_long")),
        #                  column(6, tableOutput("table_kobesummary_long"))),
        #         fluidRow(column(6, plotOutput("plot_kobe_ptables_hist")),
        #                  column(6, tableOutput("table_kobesummary_hist")))
        #), # End of Kobe and Majuro tab
        
        #------------------------------------------------------
        # About
        #------------------------------------------------------
        tabPanel("About", value="about",
          fluidRow(column(8, 
            spc_about()
          ))
        ) # End of About
      ) # End of navbarPage()
    ) # End of mainPanel()
  ) # End of sidebarLayout()
) # End of fluidPage()


#-------------------------------------------------
# Server function
#-------------------------------------------------

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
    hcr_choices <- c("HCR 3", "HCR 11")
    p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type="median_bar")
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
    hcr_choices <- c("HCR 3", "HCR 11")
    p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type="box")
    p <- p + ggplot2::ylim(0,NA)
    p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
    return(p)
  })

  output$demotimeseriesplot <- renderPlot({
    # Demo time series plot
    hcr_choices <- c("HCR 3", "HCR 11")
    pi_choices <- c("pi3")
    metric_choices <- c("relative catch")
    area_choices <- "total"
    dat <- dplyr::filter(yearqs, pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
    wormdat <- dplyr::filter(worms, pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
    p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=FALSE, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("PI 3: Catch (rel. to 2013-2015)")
    p <- p + facet_grid(piname ~ hcrref, scales="free")#, ncol=1)
    # Size of labels etc
    p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    return(p)
  })

  #-------------------------------------------------------------------
  # Comparison plots
  no_facets_row <- 2
  height_per_pi <- 300
  height_per_area <- 300

  # Bar or box plot - facetting on PI
  plot_barbox_comparehcr <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      pi_choices <- input$pichoice
      if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
        return()
      }
      # There are 7 panels
      # pi1, pi3, pi4, pi6, pi7, pi8, sbsbf0
      # They have different groupings based on metric and area
      # pi1: metric = SBSBF0, area = 1-8, all
      # pi3: area = 1-8, total, ps678, pl_jp
      # pi4: area = ps678x
      # pi6: area = as pi3 and metric catch stability / relative catch stability (not in piname so need additional subset)
      # pi7: area = ps678x
      # pi8: area = all
      # sbsbsf0: area = 1-8, all
      catch_area_choice <- input$catchareachoice
      dat <- subset(periodqs, period != "Rest" & area %in% c("all", catch_area_choice, "ps678x") & piname %in% pi_choices & metric != "catch stability")

      ## Need to hack pi1 so that all quantiles = X50., else NA
      ## Probably better to do this at the top
      #colnames(periodqs)[substr(colnames(periodqs),1,1)=="X"]
      #if("pi1" %in% dat$pi){
      #  dat[dat$pi=="pi1",c("X1.", "X5.", "X10.", "X15.", "X20.", "X80.", "X85.", "X90.","X95.","X99.")] <- dat[dat$pi=="pi1","X50."]
      #}

      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylim(0,NA)
      p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
      # Add LRP and TRP lines
      # Only if SB/SBF=0 is in dat
      if ("SB/SBF=0" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
        #p <- p + ggplot2::geom_hline(data=data.frame(yint=trp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
        p <- p + ggplot2::geom_hline(data=data.frame(yint=trp2, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
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

  # Time series comparisons - just three plots
  pinames_ts <- c("SB/SBF=0", "PI 3: Catch (rel. to 2013-2015)" ,"PI 4: Relative CPUE")
  # pis to plot time series of
  
  # Try facetting rather than plotting one on top of the other
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
    dat <- subset(yearqs, area %in% c("all", catch_area_choice, "ps678x") & piname %in% pi_choices & metric != "catch stability")
    wormdat <- subset(worms, area %in% c("all", catch_area_choice, "ps678x") & piname %in% pi_choices & metric != "catch stability" & iter %in% wormiters)

    p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
    # Facet by PI
    p <- p + facet_grid(piname ~ hcrref, scales="free")#, ncol=1)
    #p <- p + ylab("Catch")
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("Value")
    # Add LRP and TRP if SB/SBF=0 is plotted
    if ("SB/SBF=0" %in% pi_choices){
      p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      #p <- p + ggplot2::geom_hline(data=data.frame(yint=trp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      p <- p + ggplot2::geom_hline(data=data.frame(yint=trp2, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
    }
    # Size of labels etc
    p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    return(p)
  }, height=function(){max(height_per_pi*1.5, (height_per_pi * length(input$pichoice[input$pichoice %in% pinames_ts])))})

  get_pi_table <- function(period_choice="Short"){
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    catch_area_choice <- input$catchareachoice
    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }
    catch_area_choice <- input$catchareachoice
    dat <- subset(periodqs, hcrref %in% hcr_choices & period == period_choice & area %in% c("all", catch_area_choice, "ps678x") & piname %in% pi_choices & metric != "catch stability")
    tabdat <- pitable(dat, percentile_range = inner_percentiles)
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
  #output$plot_ts_sbsbf0  <- renderPlot({
  #  show_spaghetti <- input$showspag
  #  hcr_choices <- input$hcrchoice
  #  if(length(hcr_choices) < 1){
  #    return()
  #  }
  #  # SBSBF0 - Just combined area
  #  dat <- subset(yearqs, pi=="sbsbf0" & metric=="SBSBF0" & area=="all") 
  #  # Add Option for worms
  #  wormdat <- subset(worms, pi=="sbsbf0" & metric=="SBSBF0" & area=="all" & iter %in% wormiters) 
  #  # Else wormdat <- NULL
  #  p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
  #  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=lrp), linetype=3)
  #  #p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=trp), linetype=3)
  #  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=trp2), linetype=3)
  #  p <- p + ggplot2::ylab("SB/SBF=0")
  #  p <- p + ggplot2::ylim(c(0,NA))
  #  # Axes limits set here or have tight?
  #  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
  #  # Size of labels etc
  #  p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
  #  return(p)
  #})

  # Bar and box plot 
  # SBSBF0
  #output$plot_barbox_sbsbf0 <- renderPlot({
  #  plot_type <- input$plotchoicebarbox
  #  hcr_choices <- input$hcrchoice
  #  if(length(hcr_choices) < 1){
  #    return()
  #  }
  #  dat <- subset(periodqs, period != "Rest" & pi=="sbsbf0" & metric=="SBSBF0" & area=="all") 
  #  p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
  #  p <- p + ggplot2::ylab("SB/SBF=0") + ggplot2::ylim(c(0,1))
  #  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=lrp), linetype=2)
  #  #p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=trp), linetype=2)
  #  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=trp2), linetype=2)
  #  return(p)
  #})

  # Not used at moment
  # Could add in option for TRP
  #output$plot_barbox_pi8 <- renderPlot({
  #  plot_type <- input$plotchoicebarbox
  #  hcr_choices <- input$hcrchoice
  #  if(length(hcr_choices) < 1){
  #    return()
  #  }
  #  dat <- subset(periodqs, period != "Rest" & pi=="pi8" & metric=="trp_05" & area=="all") 
  #  p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
  #  # Average closeness to TRP
  #  p <- p + ggplot2::ylab("PI 8: Proximity to TRP") + ggplot2::ylim(c(0,1))
  #  return(p)
  #})
  
  #output$plot_barbox_pi82 <- renderPlot({
  #  plot_type <- input$plotchoicebarbox
  #  hcr_choices <- input$hcrchoice
  #  if(length(hcr_choices) < 1){
  #    return()
  #  }
  #  dat <- subset(periodqs, period != "Rest" & pi=="pi8" & metric=="trp_0425" & area=="all") 
  #  p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
  #  # Average closeness to TRP
  #  p <- p + ggplot2::ylab("PI 82: Proximity to SB/SBF=0 in 2012") + ggplot2::ylim(c(0,1))
  #  return(p)
  #})

  # Bar plot 
  # PI 1: Prob of SBSBF0 > LRP
  #output$plot_bar_problrp <- renderPlot({
  #  hcr_choices <- input$hcrchoice
  #  if(length(hcr_choices) < 1){
  #    return()
  #  }
  #  dat <- subset(periodqs, period != "Rest" & pi=="pi1" & area=="all") 
  #  p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type="median_bar")
  #  p <- p + ggplot2::ylab("PI 1: Prob. SB/SBF=0 > LRP") + ggplot2::ylim(c(0,1))
  #  return(p)
  #})

  # For exploring the catches in different regions
  output$plot_pi3 <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }

    plot_choice <- input$plotchoicebarboxtime
    area_choice <- input$areachoice
    if(length(area_choice) < 1){
      return()
    }
    # Choose if relative to year X
    catch_rel_choice <- "relative catch"
    ylabel <- "PI 3: Catch (rel. to 2013-2015)"

    if (plot_choice %in% c("median_bar","box")){
      dat <- subset(periodqs, period != "Rest" & pi=="pi3" & area %in% area_choice & metric == catch_rel_choice) 
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice)
      p <- p + ggplot2::ylab(ylabel) + ggplot2::ylim(c(0,NA))
      p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    }

    if(plot_choice == "time"){
      show_spaghetti <- input$showspag
      dat <- subset(yearqs, pi=="pi3" & area %in% area_choice & metric == catch_rel_choice) 
      wormdat <- subset(worms, pi=="pi3" & area %in% area_choice & metric == catch_rel_choice & iter %in% wormiters) 
      p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
      p <- p + facet_grid(area_name ~ hcrref, scales="free")#, ncol=1)
      p <- p + ggplot2::ylab(ylabel)
      p <- p + ggplot2:: ylim(c(0,NA))
      # Axes limits set here or have tight?
      p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
      # Size of labels etc
      p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    }
    return(p)
  }, height=function(){
    # Height of each facet is a little complicated
    if(input$plotchoicebarboxtime=="time"){return(max(height_per_area*1.5, (height_per_area * length(input$areachoice))))}
    if(input$plotchoicebarboxtime %in% c("median_bar","box")){return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$areachoice) / no_facets_row))))}
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
    metric_choice <-  paste("relative catch", stabvar_choice, sep=" ") # or stability
    ylabel <- paste("PI 6: ",   paste0(toupper(substr(stabvar_choice, 1, 1)), substr(stabvar_choice, 2, nchar(stabvar_choice))), " of relative catch", sep="")
    dat <- subset(periodqs, period != "Rest" & pi=="pi6" & area %in% area_choice & metric == metric_choice) 
    p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice)
    p <- p + ggplot2::ylab(ylabel) + ggplot2::ylim(c(0,NA))
    p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    return(p)

  }, height=function(){
    return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$areachoice) / no_facets_row))))
  })

  # Timeseries
  # PI: 4
  #output$plot_ts_relcpue <- renderPlot({
  #  show_spaghetti <- input$showspag
  #  hcr_choices <- input$hcrchoice
  #  if(length(hcr_choices) < 1){
  #    return()
  #  }
  #  dat <- subset(yearqs, pi=="pi4") 
  #  # Add Option for worms
  #  wormdat <- subset(worms, pi=="pi4" & iter %in% wormiters) 
  #  # Else wormdat <- NULL
  #  # May need to subset over area in the future if we add additional groupings
  #  p <- time_ser_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, percentile_range = pi_percentiles)
  #  p <- p + ggplot2::ylab("Relative CPUE")
  #  p <- p + ggplot2::ylim(c(0,NA))
  #  # Axes limits set here or have tight?
  #  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
  #  # Size of labels etc
  #  p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
  #  return(p)
  #})

  # Bar and box plot 
  # PI 4: Catch
  #plot_barbox_relcpue<- function(plot_type="median_bar"){
  #  rPlot <- renderPlot({
  #    hcr_choices <- input$hcrchoice
  #    if(length(hcr_choices) < 1){
  #      return()
  #    }
  #    dat <- subset(periodqs, period != "Rest" & pi=="pi4") 
  #    p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
  #    p <- p + ggplot2::ylab("PI 4: Relative CPUE") + ggplot2::ylim(c(0,NA))
  #    return(p)
  #  })
  #  return(rPlot)
  #}

  #output$plot_bar_relcpue <- plot_barbox_relcpue(plot_type="median_bar")
  #output$plot_box_relcpue <- plot_barbox_relcpue(plot_type="box")

  # Bar and box plot 
  # PI 7: Effort variability and stability
  plot_barbox_pi7varstab <- function(plot_type="median_bar", metric_choice="relative effort variability", ylab="PI 7: Relative effort variability", ylim=c(0,NA)){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      # Choose if relative to year X
      dat <- subset(periodqs, period != "Rest" & pi=="pi7" & metric==metric_choice) 
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab(ylab) + ggplot2::ylim(ylim)
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_pi7var <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort variability", ylab="PI 7: Variability of relative effort", ylim=c(0,NA))
  output$plot_box_pi7var <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort variability", ylab="PI 7: Variability of relative effort", ylim=c(0,NA))
  output$plot_bar_pi7stab <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort stability", ylab="PI 7: Stability", ylim=c(0,1))
  output$plot_box_pi7stab <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort stability", ylab="PI 7: Stability", ylim=c(0,1))

  # Plot the HCR shapes and the bits that were active
  output$plot_hcrshape <- renderPlot({
    # Able to choose which HCRs
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }

    # Point and path options
    # Subset out only a limited number of iters to plot on hcr_points
    # Which iters are common
    hcr_points_sub <- hcr_points
    if(input$showpoints==TRUE){
      niters <- input$mppointiters
      if(is.na(niters)){
        niters <- 1
      }
      nhcriters <- min(niters, length(common_iters))
      hcriters <- sample(common_iters, nhcriters)
      hcr_points_sub <- subset(hcr_points, iter %in% hcriters)
    }

    p <- hcr_plot(hcr_choices=hcr_choices, hcr_shape=hcr_shape, hcr_points=hcr_points_sub, lrp=lrp, trp=trp2, add_points=input$showpoints, add_path=input$showpath)
    p <- p + ylab("Catch or effort multiplier")
    return(p)
  })
  
  # Plot histogram of effort multipliers
  output$plot_hcrhistograms <- renderPlot({
    
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    # Don't show historical
    pdat <- subset(histodat, period != "Rest")
    p <- hcr_histo_plot(hcr_choices, pdat)
    return(p)
  })

  # Kobe / Majuro stuff
  majuro_kobe_table <- function(period="Short", period_label="Short-term"){
    outfunc <- renderTable({
      plot_choice <- input$majurokobe
      if(plot_choice == "Kobe"){
        dat <- kobe_summary_tabs[[period]]
      }
      else {
        dat <- majuro_summary_tabs[[period]]
      }
      return(dat)
    },
    rownames = FALSE,
    caption = paste(period_label, " summary. Table values show the percentage (%) of observations in each plot quadrant.", sep=""),
      auto=TRUE)
    return(outfunc)
  }

  output$table_kobesummary_short <- majuro_kobe_table(period="Short", period_label="Short-term")
  output$table_kobesummary_medium <- majuro_kobe_table(period="Medium", period_label="Medium-term")
  output$table_kobesummary_long <- majuro_kobe_table(period="Long", period_label="Long-term")
  output$table_kobesummary_hist <- majuro_kobe_table(period="Historical", period_label="Historical")

  majuro_kobe_plot <- function(period){
    out <- renderPlot({
      hcr_choice <- input$hcrchoicekobe
      plot_choice <- input$majurokobe
      if(plot_choice == "Kobe"){
        dat <- kobe_ptables_indiv[[hcr_choice]]
      }
      else {
        dat <- majuro_ptables_indiv[[hcr_choice]]
      }
      grid::grid.draw(dat[[period]])
    })
    return(out)
  }

  output$plot_kobe_ptables_hist <- majuro_kobe_plot(period="Historical")
  output$plot_kobe_ptables_long <- majuro_kobe_plot(period="Long")
  output$plot_kobe_ptables_medium <- majuro_kobe_plot(period="Medium")
  output$plot_kobe_ptables_short <- majuro_kobe_plot(period="Short")

} # end of server

# Run the app
shinyApp(ui, server)
