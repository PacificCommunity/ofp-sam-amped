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
load("data/preSC15_results.Rdata")

#------------------------------------------------------------------------------------------------------
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

piselector <- unique(periodqs[!periodqs$upsidedown,"piname"])

worms$wormid <- paste(worms$msectrl, worms$iter, sep="_")

#------------------------------------------------------------------------------------------------------
# UI
ui <- navbarPage(
  tags$head(includeHTML("google-analytics.html")),
  title="Performance Indicators and Management Procedures Explorer",
  sidebarLayout(
    sidebarPanel(width=3, 
      br(),
      img(src = "spc.png", height = 100),
      br(),
      # HCR selection - all selected initially
      conditionalPanel(condition="input.top!='notes'",
        checkboxGroupInput(inputId = "hcrchoice", label="HCR selection", selected = unique(periodqs$hcrref), choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref))
      ),
      # Only show this if compare MPs tab is selected
      conditionalPanel(condition="input.top == 'compareMPs'",
        checkboxGroupInput(inputId = "pichoice", label="PI selection", choices = piselector, selected=sort(unique(periodqs$piname)))
      ),
      # Areas and relative options for the catch metrics
      conditionalPanel(condition="(input.top != 'about') & (input.top != 'notes') & (input.top != 'hcrs') & ((input.piinspector =='pi3') || (input.piinspector =='pi6') || (input.top == 'compareMPs'))",
        selectInput(inputId = "catchsetchoice", label="Catch grouping", choices = list("Total"="total", "Purse seine in regions 2,3 & 5"="ps235"), selected="area")
      ),
      conditionalPanel(condition="(input.top != 'about') & (input.top != 'notes') & (input.top != 'hcrs') & ((input.piinspector =='pi3') || (input.piinspector =='pi6') || (input.top == 'compareMPs'))",
        selectInput(inputId = "catchrelchoice", label="Catch type", choices = list("Absolute catch"="catch", "Relative to average catch in 2013-15"="relative catch"), selected="catch")
      ),
      # Show spaghetti option on some tabs - unseen tabs are still live the conditional is weird
      #conditionalPanel(condition ="(input.top != 'about') & (input.top != 'notes') & (input.top != 'hcrs') & ((input.piinspector=='pi1') || (input.piinspector=='pi3') || (input.piinspector=='pi4') || (input.piinspector=='mw') || (input.picomparitor=='timeseriesplots'))",
      #  checkboxInput(inputId="showspag", label="Show trajectories", value=FALSE)), 
      conditionalPanel(condition="input.top=='notes'",
          pimple_maintainer_and_licence()
      )
    ),
    mainPanel(width=9,
      tabsetPanel(id="top",
        #------------------------------------------------------------
        # HCRs
        #------------------------------------------------------------
        tabPanel(title="The HCRs", value="hcrs",
          column(12, fluidRow(
            plotOutput("plot_hcrshape", height="600px"),
            plotOutput("plot_hcrhistograms"))
          )
        ),

        #------------------------------------------------------------
        # PI Inspector tab
        #------------------------------------------------------------

        tabPanel(title="PI Inspector",
          tabsetPanel(id="piinspector",

            # *** SBSBF0 and PI 1 & 8 ***
            tabPanel(title="PI 1 & 8: Biomass",value="pi1",
              column(12, fluidRow(
                # TS of SBSBF0
                plotOutput("plot_ts_sbsbf0"),
                checkboxInput("showspagsb", "Show trajectories", value=FALSE) 
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
              ))
            ),

            # *** PI 3: Catch based ones ***
            tabPanel(title="PI 3: Catches", value="pi3",
              column(12, fluidRow(
                plotOutput("plot_ts_catch"),
                checkboxInput("showspagc", "Show trajectories", value=FALSE) 
              )),
              column(6, fluidRow(
                plotOutput("plot_bar_catch")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_catch")
              ))
            ),

            # *** PI 4: Relative CPUE***
            tabPanel(title="PI 4: Relative CPUE", value="pi4",
              column(12, fluidRow(
                plotOutput("plot_ts_relcpue"),
                checkboxInput("showspagrc", "Show trajectories", value=FALSE) 
              )),
              column(6, fluidRow(
                plotOutput("plot_bar_relcpue")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_relcpue")
              ))
            ),

            # *** PI 6: Catch stability ***
            tabPanel(title="PI 6: Catch stability", value="pi6",
              column(6, fluidRow(
                plotOutput("plot_bar_catchvar"),
                plotOutput("plot_bar_catchstab")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_catchvar"),
                plotOutput("plot_box_catchstab")
              ))
            ),

            # *** PI 7: Relative effort variability***
            tabPanel(title="PI 7: Relative effort variability", value="pi7",
              column(6, fluidRow(
                plotOutput("plot_bar_pi7var"),
                plotOutput("plot_bar_pi7stab")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_pi7var"),
                plotOutput("plot_box_pi7stab")
              ))
            ),

            # *** Mean weight individual ***
            tabPanel(title="Mean weight of individual", value="mw",
              column(12, fluidRow(
                plotOutput("plot_ts_mw"),
                checkboxInput("showspagmw", "Show trajectories", value=FALSE) 
              )),
              column(6, fluidRow(
                plotOutput("plot_bar_mw")
              )),
              column(6, fluidRow(
                plotOutput("plot_box_mw")
              ))
            )
          )
        ),

        #------------------------------------------------------------
        # Comparison tab
        #------------------------------------------------------------

        tabPanel(title="Comparing MPs", value="compareMPs",
          tabsetPanel(id="picomparitor",
            tabPanel(title="Bar charts", value="barchart",
              plotOutput("plot_bar_comparehcr", height="600px")
            ),
            tabPanel(title="Box plots", value="boxplot",
              plotOutput("plot_box_comparehcr", height="600px")
            ),
            tabPanel(title="Radar plots", value="radarplot",
              fluidRow(
                selectInput(inputId = "radarscaling", label="Radar plot scaling", choices = list("Scale by max"="scale", "Rank"="rank"), selected="scale"),
                plotOutput("plot_radar_comparehcr", height="600px")
              )
            ),
            tabPanel(title="Time series plots", value="timeseriesplots",
              plotOutput("plot_timeseries_comparehcr", height="600px"),
              checkboxInput("showspagts", "Show trajectories", value=FALSE) 
            ),
            #tabPanel(title="Kobe / Majuro", value="kobemajuro"),
            tabPanel(title="Table", value="bigtable", 
                     tableOutput("table_pis_short"),
                     tableOutput("table_pis_medium"),
                     tableOutput("table_pis_long")
            )
          )
        ),
        tabPanel(title="Notes", value="notes",
        h1("PIMPLE"),
        p("PIMPLE is a tool for exploring and comparing the performance of alternative candidate harvest control rules (HCRs).
        The performance can be explored using a range of different plots and tables.
        This allows trade-offs between the different HCRs to be evaluated.
        The performance of each HCR is measured using different performance indicators (PIs). More details of the PIs are given in the PIMPLE user guide."), 
        br(),
        h2("The indicators"),
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
        p("As well as the variability, the stability is calculated. A stability of 1 implies that the relative effort does not change at all over time, i.e. it is completely stable. A stability of 0 means that the relative effort is very variable in time."),
        br(),
        h3("Indicator 8. Stability and continuity of market supply"),
        p("This indicator is concerned with maintaining the stock size around the TRP levels (where the interim TRP for skipjack is SB/SBF=0 = 0.5).  It is assumed that the further away SB/SBF=0 is from 0.5, the worse the HCR can be thought to be performing, i.e. it is better to have SB/SB_F=0 close to 0.5 on average."),
        p("An indicator value of 1 implies that SB/SB_F=0 is exactly at the TRP and a value of 0 is as far from the TRP as possible. This means that you want this indicator to be close to 1."),
        br(),
        h3("Mean weight of an individual in the population"),
        p("This indicator measures the mean weight of an individual in the population, not the catch. It is calulated by taking the total weight of individuals across the region and dividing it by the total number of individuals across the region. These kind of indicators are important because they can provide information on changes to the size structure of a population as a result of fishing and changes in environmental conditions.") 
        ),
        tabPanel("About",value="about",
                 mainPanel(width=12,
                           HTML("<p style='opacity: 0.5;' class='caption' align='center'>&copy; Pacific Community, 2019</P>
                                <h1>About us:</h1>
                                <p align='center'><img src='spc.png'></p>
                                <p align='justify'>The Pacific Community (SPC) is the principal scientific and technical organisation in the Pacific region, proudly supporting development since 1947. It is an international development organisation owned and governed by its 26 country and territory members. The members are: American Samoa, Australia, Cook Islands, Federated States of Micronesia, Fiji, France, French Polynesia, Guam, Kiribati, Marshall Islands, Nauru, New Caledonia, New Zealand, Niue, Northern Mariana Islands, Palau, Papua New Guinea, Pitcairn Islands, Samoa, Solomon Islands, Tokelau, Tonga, Tuvalu, United States of America, Vanuatu, and Wallis and Futuna.</P> 
                                <p align='justify'>In pursuit of sustainable development to benefit Pacific people, this unique organisation works across more than 25 sectors. SPC is renowned for its knowledge and innovation in such areas as fisheries science, public health surveillance, geoscience and conservation of plant genetic resources for food and agriculture.</p>
                                <p align='justify'>Much of SPC's focus is on major cross-cutting issues, such as climate change, disaster risk management, food security, gender equality, human rights, non-communicable diseases and youth employment. Using a multi-sector approach in responding to its members' development priorities, SPC draws on skills and capabilities from around the region and internationally, and supports the empowerment of Pacific communities and sharing of expertise and skills between countries and territories.</p>
                                <p align='justify'>With over 600 staff, SPC has its headquarters in Noumea, regional offices in Suva and Pohnpei, a country office in Honiara and field staff in other Pacific locations. Its working languages are English and French. See <a href=\"https://www.spc.int\">www.spc.int</a>."
                           )
                 )
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

  # Trim out years for tight time series plots
  yearqs <- subset(yearqs, year %in% first_plot_year:last_plot_year)
  worms <- subset(worms, year %in% first_plot_year:last_plot_year)

  # Careful with these - they are only used for plotting lines, NOT for calculating the indicators
  lrp <- 0.2
  trp <- 0.5
  # For the worms - same worms for all plots
  # This can be increased to 20 - maybe make as option?
  nworms <- 5
  # worms are a unique combination of OM and iter
  # (same om / iter should be in all hcrs)
  wormiters <- sample(unique(worms$iter), nworms)

  #-------------------------------------------------------------------
  # Individual PI plots

  # Timeseries
  # SBSBF0
  output$plot_ts_sbsbf0  <- renderPlot({
    show_spaghetti <- input$showspagsb
    hcr_choices <- input$hcrchoice
    # SBSBF0 - Just combined area
    dat <- subset(yearqs, pi=="biomass" & metric=="SBSBF0" & area=="all") 
    # Add Option for worms
    wormdat <- subset(worms, pi=="biomass" & metric=="SBSBF0" & area=="all" & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti)
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
    show_spaghetti <- input$showspagc
    hcr_choices <- input$hcrchoice
    # Choose the aggregation (set: total area or PS235 - maybe by area too?)
    catch_set_choice <- input$catchsetchoice
    # Choose if relative to year X
    catch_rel_choice <- input$catchrelchoice # or relative catch
    dat <- subset(yearqs, pi=="pi3" & set==catch_set_choice & metric == catch_rel_choice) 
    # Add Option for worms
    wormdat <- subset(worms, pi=="pi3" & set==catch_set_choice & metric == catch_rel_choice & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti)
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
      # Choose the aggregation (set: total area or PS235 - maybe by area too?)
      catch_set_choice <- input$catchsetchoice
      # Choose if relative to year X
      catch_rel_choice <- input$catchrelchoice # or relative catch
      dat <- subset(periodqs, period != "Rest" & pi=="pi3" & set==catch_set_choice & metric == catch_rel_choice) 
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
    show_spaghetti <- input$showspagrc
    hcr_choices <- input$hcrchoice
    # Choose the aggregation (set: total area or PS235 - maybe by area too?)
    #catch_set_choice <- input$catchsetchoice
    # Choose if relative to year X
    #catch_rel_choice <- input$catchrelchoice # or relative catch
    dat <- subset(yearqs, pi=="pi4") 
    # Add Option for worms
    wormdat <- subset(worms, pi=="pi4" & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti)
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
      # Choose the aggregation (set: total area or PS235 - maybe by area too?)
      catch_set_choice <- input$catchsetchoice
      # Choose if relative to year X
      dat <- subset(periodqs, period != "Rest" & pi=="pi6" & set==catch_set_choice & metric==metric_choice) 
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
      # Choose if relative to year X
      dat <- subset(periodqs, period != "Rest" & pi=="pi7" & metric==metric_choice) 
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylab(ylab) + ggplot2::ylim(ylim)
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_pi7var <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort variability", ylab="PI 7: Relative effort variability", ylim=c(0,NA))
  output$plot_box_pi7var <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort variability", ylab="PI 7: Relative variability", ylim=c(0,NA))
  output$plot_bar_pi7stab <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort stability", ylab="PI 7: Relative stability", ylim=c(0,1))
  output$plot_box_pi7stab <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort stability", ylab="PI 7: Relative effort stability", ylim=c(0,1))





  # Mean weight of individual
  # PI: 4
  output$plot_ts_mw<- renderPlot({
    show_spaghetti <- input$showspagmw
    hcr_choices <- input$hcrchoice
    dat <- subset(yearqs, pi=="mw") 
    # Add Option for worms
    wormdat <- subset(worms, pi=="mw" & iter %in% wormiters) 
    # Else wormdat <- NULL
    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti)
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

  plot_barbox_comparehcr <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      # It gets complicated because each PI has suboption in area, set, metric columns
      # biomass: area == all and metric == SBSBF0
      # pi1: area == all
      # pi3: area == NA and set == catchsetchoice and metric == catchrelchoice (if set != area)
      # pi4: metric == relative_cpue
      # pi6: area == NA and set == catchsetchoice and metric == stability or variability - just stab (if set != area)
      # pi7: metric == stability or variability - just stab
      # pi8: area == all
      # mw: mean_weight

      # Put these together
      # area is all or NA
      # set is catchsetchoice or NA
      # metric is catchrelchoice, stability, or NA
      hcr_choices <- input$hcrchoice
      pi_choices <- input$pichoice
      set_choices <- c(input$catchsetchoice, as.character(NA))
      metric_choices <- c(input$catchrelchoice,"mean_weight",  "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
      area_choices <- c("all", as.character(NA))
      dat <- subset(periodqs, period != "Rest" & piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)
      # Need to hack pi1 so that all quantiles = X50., else NA
      dat[dat$pi=="pi1",c("X5.", "X20.", "X80.", "X95.")] <- dat[dat$pi=="pi1","X50."]
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylim(0,NA)
      p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
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
    set_choices <- c(input$catchsetchoice, as.character(NA))
    metric_choices <- c(input$catchrelchoice,"mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    area_choices <- c("all", as.character(NA))
    dat <- subset(periodqs, period != "Rest" & piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)
    scaling_choice <- input$radarscaling
    p <- myradar(dat=dat, hcr_choices=hcr_choices, scaling_choice)
    return(p)
  })

  # Time series comparisons
  output$plot_timeseries_comparehcr <- renderPlot({
    show_spaghetti <- input$showspagts
    hcr_choices <- input$hcrchoice
    #pi_choices <- c("biomass", "pi3", "pi4")
    pi_choices <- c("SB/SBF=0", "PI 3: Catch","PI 4: Relative CPUE")
    set_choices <- c(input$catchsetchoice, as.character(NA))
    metric_choices <- c(input$catchrelchoice,"mean_weight",  "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    area_choices <- c("all", as.character(NA))
    dat <- subset(yearqs, piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)
    wormdat <- subset(worms, piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices & iter %in% wormiters)

    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti)
    #p <- p + ylab("Catch")
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("Value")
    return(p)


  })

  # Plot the HCR shapes and the bits that were active
  output$plot_hcrshape <- renderPlot({
    # Able to choose which HCRs
    hcr_choices <- input$hcrchoice
    p <- hcr_plot(hcr_choices=hcr_choices, hcr_shape=hcr_shape, hcr_points=hcr_points, lrp=lrp, trp=trp)
    return(p)
  })

  # Plot histogram of effort multipliers
  output$plot_hcrhistograms <- renderPlot({
    hcr_choices <- input$hcrchoice
    p <- hcr_histo_plot(hcr_choices, histodat)
    return(p)
  })


  get_pi_table <- function(period_choice="Short"){
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    set_choices <- c(input$catchsetchoice, as.character(NA))
    metric_choices <- c(input$catchrelchoice, "mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    area_choices <- c("all", as.character(NA))
    dat <- subset(periodqs, hcrref %in% hcr_choices & period == period_choice & piname %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)
    tabdat <- pitable(dat)
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
