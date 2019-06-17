# Copyright 2018 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC

library(shinyjs) # For greying out buttons
library(AMPLE)

ui <- navbarPage(
  title="Measuring performance",
  tabPanel("HCR Selection", 
    useShinyjs(),  # Include shinyjs
    sidebarLayout(          
      sidebarPanel(width=3, 
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        # HCR options
        mp_params_setterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch", "Threshold effort", "Constant effort")),
        br(),
        actionButton("project", "Project", icon=icon("fish")), # Careful with fish icon - needs particular version of R
        actionButton("add_basket", "Add HCR to basket", icon=icon("shopping-basket")),
        # How many HCRs do we have in the store
        textOutput("nstoredstocks"),
        br(),
        # This should reset everything - empty the stores
        actionButton("empty_basket", "Empty basket")
      ),
      mainPanel(width=9,
        column(6,
          fluidRow(
            tags$span(title="The HCR. The grey points show the inputs and outputs from all years from all iterations. This enables you to see which parts of the HCR shape are most used.", plotOutput("plothcr")
            )
          ),
          fluidRow(
            tags$span(title="A table of various performance indicators calculated over the short-, medium- and long- term. The value is the median. The values in the brackets are the 20th and 80th percentiles respectively. See the information tab for more details", tableOutput("currenthcrpis"), style = "font-size:100%")
          )
        ),
        # Column 3 - has 3 rows
        column(6,
          fluidRow(
            tags$span(title="Plot of SB/SBF=0. The grey envelope contains the 20-80 percentiles. The blue dashed line is median. The black lines are several iterations to illustrate the dynamics. The histogram shows the range of values in the final year.",
            plotOutput("plotbiomasshisto",height="250px")
          )
        ),
          fluidRow(
            tags$span(title="Plot of the catch. The grey envelope contains the 20-80 percentiles. The blue dashed line is median. The black lines are several iterations to illustrate the dynamics. The histogram shows the range of values in the final year.",
              plotOutput("plotcatchhisto",height="250px")
            )
          ),
          fluidRow(
            tags$span(title="Plot of the CPUE relative to the CPUE in the year 2000.  The grey envelope contains the 20-80 percentiles. The blue dashed line is median. The black lines are several iterations to illustrate the dynamics. The histogram shows the range of values in the final year.",
              plotOutput("plotrelcpuehisto",height="250px")
            )
          )
        )
      )
    )  
  ),
  tabPanel("Compare performance",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        # PI selection
        # Keep this updated with the available PIs
        checkboxGroupInput(inputId = "pichoice", label="PI selection", inline=TRUE, 
          # character(0) means no choice is available
          choices = character(0)),
        br(),
        # Dynamic HCR selection
        # See: https://shiny.rstudio.com/reference/shiny/1.0.0/updateCheckboxGroupInput.html
        checkboxGroupInput(inputId = "hcrchoice", label="HCR selection",
          # character(0) means no choice is available
          #choices = character(0)),
          choiceNames = character(0), choiceValues = character(0)),
        br()
      ),
      mainPanel(width=9,
        tabsetPanel(id="comparisontabs",
          tabPanel(title="Performance indicators - medians",
                   value="PImeds",
                   column(12, fluidRow( 
                    tags$span(title="Bar plot of the median values of the performance indicators over the three time periods. Note that the PIs for effort and variability have been transformed so that the larger the value, the better the HCR is thought to be performing.",
                    plotOutput("plotpimed", height="600px"))
                  ))
          ),
          tabPanel(title="Performance indicators - boxplots",
                   value="PIbox",
                   column(12, fluidRow(
                  tags$span(title="Box plot of the values of the performance indicators over the three time periods. The box contains the 20-80 percentiles, the tails the 5-95 percentils.",
                   plotOutput("plotpibox", height="600px"))
           ))
          ),
          tabPanel(title="Performance indicators - radar",
                   value="PIradar",
                   column(12, fluidRow(
                    tags$span(title="Radar plot of the median values of the performance indicators over the three time periods. Note that the PIs for effort and variability have been transformed so that the larger the value, the better the HCR is thought to be performing.",
                 plotOutput("plotpiradar", height="600px"))#,
                 #"Note that stability PIs and relative effort are not shown on the radar plot."
           ))
          ),
          tabPanel(title="Performance indicators - table",
                   value="PItable",
                   column(12, fluidRow(
                   #"Performance indicators in the long-term",
                   "Performance indicators in the short-, medium- and long-term",
                   div(tags$span(title="Peformance indicators in the short-term. The value is the median, the values in the parentheses are the 20-80 percentiles.", tableOutput("bigpitable_short"), style = "font-size:85%")),
                   div(tags$span(title="Peformance indicators in the medium-term. The value is the median, the values in the parentheses are the 20-80 percentiles.", tableOutput("bigpitable_medium"), style = "font-size:85%")),
                   div(tags$span(title="Peformance indicators in the long-term. The value is the median, the values in the parentheses are the 20-80 percentiles.", tableOutput("bigpitable_long"), style = "font-size:85%"))
           ))
          ),
          tabPanel(title="Majuro plots",
                   value="majuroall",
                   column(12, fluidRow(
                    tags$span(title="Majuro plot of the trajectories of the stocks under the different HCRs.",
                  plotOutput("plotmajuroall", height="600px"))
           ))
          ),
          tabPanel(title="Time series",
                   value="timeseries",
                    tags$span(title="Time series plots of various metrics for the stocks under the different HCRs. The envelope contains the 20-80 percentiles of the distribution. The dashed line is the median value. Some individual trajectories can be shown by selecting the 'Show spaghetti' option.",
                  fluidRow(column(12, plotOutput("plottimeseries", height="600px")))),
                  fluidRow(column(12, checkboxInput("spaghetti", "Show spaghetti", FALSE)))
          )
        )
      )
    )
  ),
  # Tab for choosing stock parameters, stock history, no. iterations etc
  # Changing any of these will empty the basket
  tabPanel("Settings",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        "Note that changing any of these settings will reset the current stock and empty the basket",
        br()
      ),
      mainPanel(width=9,
        fluidRow(column=12,
          stoch_params_setterUI("stoch", init_prod_sigma=0.2, init_est_sigma=0.2, init_est_bias=0.0),
          stock_params_setterUI("stock"),
          # Total number of years (including historical years)
          numericInput("nyears", "Number of years", value = 30, min=20, max=100, step=1),
          # Number of iteration
          numericInput("niters", "Number of iterations", value = 1000, min=10, max=1000, step=10)
        )
      )
    )
  ),
  tabPanel("Information",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        amped_maintainer_and_licence()
      ),
      mainPanel(width=9,
        h1("General idea"),
        p("Choose HCRs from the drop down menu and set the parameters."),
        p("Project the stock forward under the chosen HCR by clicking on the ", strong("Project HCR"), " button. If you like the look of the output, add the HCR to the basket by clicking on the ", strong("Add to basket"), " button"),
        p("Keep adding HCRs to your basket until you are ready to compare them."),
        h1("Performance indicators"),
        p("There are 8 PIs in the table. ", em("SB/SBF=0"), " and ", em("Catch"), " are fairly self explanatory. ", em("Effort (rel. 2018)"), " and ", em("CPUE (rel. 2018)"), " are the fishing effort and CPUE relative to their values in 2018 respectively. ", em("Prob. SB > LRP"), " is the probability of of SB/SBF=0 being above the LRP. ", em("Catch variability"), ",", em("Effort variability"), " and ", em("CPUE variability"), " measure the variability in the catch, relative effort and relative CPUE respectively. The variability PIs measure the bumpiness over time. The higher the value, the more the value changes over time."),
        p("It should be noted that these PIs don't all point the same way.  It is generally thought that the higher the value of ", em("SB/SBF=0"), ",", em("Prob. SB > LRP"), ",", em("Catch"), " and ", em("CPUE (rel. 2018)"), " the better the HCR is performing. However, for ", em("Effort (rel. 2018)"), "and the ", em("variability"), " PIs, lower values are preferred. The higher the effort, the greater the costs. Stable catches and effort are preferred to catches and effort that varying strongly between years. Care must therefore be taken when using PIs to compare performance of HCRs."),
        h1("Comparing performance"),
        p("Choose the ", strong("Compare performance"), " tab for a range of plots and tables that allow the comparison of the performance of the HCRs through performance indicators and other metrics."),
        p("The performance indicators and HCRs can be selected and delselected to help with the comparison.")
      )
    )
  )
)

server <- function(input, output,session) {
  # Global parameters
  app_params <- list(initial_year = 2009, last_historical_timestep = 10)
  # pitemp is the pis for a single stock (HCR) - shown on the front page
  pitemp <- reactiveVal(NULL)
  OKtostore <- reactiveVal(FALSE)
  quantiles <- c(0.01, 0.05, 0.20, 0.5, 0.80, 0.95, 0.99)
  # Objects for the PI summaries
  worms <- reactiveVal(data.frame())
  periodqs <- reactiveVal(data.frame())
  yearqs <- reactiveVal(data.frame())

  # Modules for the stochasticity and MP parameters!
  get_mp_params <- callModule(mp_params_setter, "mpparams") 
  get_stoch_params <- callModule(stoch_params_setter, "stoch") 
  get_lh_params <- callModule(stock_params_setter, "stock") 
  # Join stoch and lh together into a single object to be passed to funcs
  get_stock_params <- reactive({
    sp <- get_stoch_params()
    lh <- get_lh_params()
    out <- c(sp, lh)
    return(out)
  })
  
  # The stock - only one needed at a time - just pull out data for PIs and plots
  stock <- create_stock()
  # Use isolate else error (function tries to be reactive to changes in get_stock_params() and stock itself
  isolate(reset_stock(stock = stock, stock_params = get_stock_params(), mp_params = get_mp_params(), app_params = app_params, initial_biomass = get_stock_params()$b0, nyears = input$nyears, niters = input$niters))
  
  # Only allow Add HCR to Basket if Project has been pressed
  observe({
    shinyjs::toggleState("add_basket", OKtostore()==TRUE)
  })

  # Store the stock in the basket and create a new empty one
  observeEvent(input$add_basket, {
    # Check if project has been pressed first or you may store empty results
    if(OKtostore() == FALSE){
      return()
    }
    # Store the stock, stockparams and mpparams
    # Add summaries to existing summaries
    stock_params <- get_stock_params()
    mp_params <- get_mp_params()
    hcrno <- length(unique(worms()$hcrref)) + 1
    hcrname <- paste(hcrno, ". ",mp_params$name,sep="")
    hcrref <- paste("HCR ", hcrno, sep="") # Used for legends
    worms(rbind(worms(), cbind(pitemp()$worms, hcrref=hcrref, hcrname=hcrname)))
    periodqs(rbind(periodqs(), cbind(pitemp()$periodqs, hcrref=hcrref, hcrname=hcrname)))
    yearqs(rbind(yearqs(), cbind(pitemp()$yearqs, hcrref=hcrref, hcrname=hcrname)))

    # Update the available PIs checkboces - although this doesn't really dynamically change
    # It just saves having to maintain a list in the UI at the top AND in the PI calculation function
    # Because the options come from the pistore and if no pistore yet, no names
    # Drop F/FMSY and others from list
    drop_pis <- c("ffmsy") 
    drop_pinames <- unique(subset(periodqs(), pi %in% drop_pis)$piname)
    pi_choices <- unique(periodqs()$piname)
    pi_choices <- pi_choices[!(pi_choices %in% drop_pinames)]
    updateCheckboxGroupInput(session, "pichoice",
                             choices = pi_choices,
                             selected = pi_choices
                             )

    # You can't store again until you project again
    OKtostore(FALSE)
  })
  
  # Reset the current stock if you touch the MP parameters
  observeEvent(get_mp_params(),{
    stock_params <- get_stock_params()
    reset_stock(stock=stock, stock_params=stock_params, mp_params=get_mp_params(), app_params=app_params, initial_biomass=stock_params$b0, nyears=input$nyears, niters=input$niters)
    # Reset the show table variable
    pitemp(NULL)
    OKtostore(FALSE)
  })

  # Update HCR choice in comparison tab (if HCR basket gets added to or emptied)
  # Alternative is to make the stock store reactive - could get a bit tricky
  observe({
    input$empty_basket
    input$add_basket
    selected <- NULL
    choiceNames <- character(0)
    choiceValues <- character(0)

    if(length(unique(periodqs()$hcrname)) > 0){
      selected <- unique(periodqs()$hcrref)
      choiceNames <- as.character(unique(periodqs()$hcrname))
      choiceValues <- unique(periodqs()$hcrref)
    }

    updateCheckboxGroupInput(session, "hcrchoice",
      selected = selected,
      choiceNames = choiceNames,
      choiceValues = choiceValues
    )
  })
  
  # Empty the basket and reset the current stock
  # Basket gets emptied when:
  #   stochasticity options change
  #   nyears changes
  #   niters changes
  #   The empty basket button is pushed
  # Changing the MP parameters does not trigger it
  observe({
    req(input$niters, input$nyears) # Can be NA due to numericInput
    # React to
    input$empty_basket
    stock_params <- get_stock_params() # Includes LH and stoch options
    niters <- input$niters
    nyears <- input$nyears
    # Do not react to changes in MP params
    mp_params <- isolate(get_mp_params())
    # Reset the stock
    isolate(reset_stock(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, initial_biomass=stock_params$b0, nyears=nyears, niters=niters))
    # Empty the storage
    pitemp(NULL)
    worms(data.frame())
    periodqs(data.frame())
    yearqs(data.frame())
    OKtostore(FALSE)
  })
  
  # Project all timesteps
  observeEvent(input$project, {
    # Do all iters at the same time
    out <-  project(stock,
                    timesteps=c((app_params$last_historical_timestep+1),dim(stock$biomass)[2]),
                    stock_params=get_stock_params(),
                    mp_params=get_mp_params(),
                    app_params=app_params)
    pitemp(get_summaries(stock=stock, stock_params=get_stock_params(), app_params=app_params, quantiles=quantiles))
    #pitemp(piqs)
    OKtostore(TRUE)
  })
  
  #------------------------------------------------------------
  # Front page plotting events

  # Call the HCR plot function
  output$plothcr <- renderPlot({
    plot_hcr(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, show_last = FALSE)
  })
  
  output$plotbiomasshisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="biomass", show_last = FALSE, quantiles=quantiles[c(3,5)])
  })

  output$plotcatchhisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="catch", show_last = FALSE, quantiles=quantiles[c(3,5)])
  })

  output$plotrelcpuehisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="relcpue", app_params=app_params, show_last = FALSE, quantiles=quantiles[c(3,5)])
  })


  output$nstoredstocks <- renderText({
    paste("Number of HCRs in basket: ", length(unique(worms()$hcrref)), sep="")
  })

  output$currenthcrpis <- renderTable({
    # Don't print table unless project has been pressed
    if (is.null(pitemp())){
      return()
    }
    # Use pitemp() to fill table
    #current_pi_table(pitemp())
    current_pi_table(pitemp()$periodqs)
    },
    rownames = TRUE,
    caption= "Performance indicators",
    auto=TRUE
  )

  #------------------------------------------------------------
  # Comparison plotting events

  get_pi_table <- function(period_choice="Short"){
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    # If no HCR or PI is selected then don't do anything
    if(is.null(hcr_choices) | is.null(pi_choices)){
      return()
    }
    dat <- subset(periodqs(), hcrref %in% hcr_choices & period == period_choice & piname %in% pi_choices)
    tabdat <- pitable(dat)
    return(tabdat)
  }

  output$bigpitable_short <- renderTable({
      tabdat <- get_pi_table(period_choice="Short")
    },
    rownames = FALSE,
    caption= "Performance indicators in the short-term",
    auto=TRUE
  )

  output$bigpitable_medium <- renderTable({
      tabdat <- get_pi_table(period_choice="Medium")
    },
    rownames = FALSE,
    caption= "Performance indicators in the medium-term",
    auto=TRUE
  )

  output$bigpitable_long <- renderTable({
      tabdat <- get_pi_table(period_choice="Long")
    },
    rownames = FALSE,
    caption= "Performance indicators in the long-term",
    auto=TRUE
  )

  plot_barbox_comparehcr <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      pi_choices <- input$pichoice
      # If no HCR or PI is selected then don't do anything
      if(is.null(hcr_choices) | is.null(pi_choices)){
        return()
      }
      # Subset out variability / stability
      dat <- subset(periodqs(), period != "Rest" & piname %in% pi_choices)
      p <- myboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type)
      p <- p + ggplot2::ylim(0,NA)
      p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
      return(p)
    })
    return(rPlot)
  }

  output$plotpimed <- plot_barbox_comparehcr(plot_type="median_bar")
  output$plotpibox <- plot_barbox_comparehcr(plot_type="box")

  # Radar plot
  output$plotpiradar <- renderPlot({
    # Subsetting out as above
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    # If no HCR or PI is selected then don't do anything
    if(is.null(hcr_choices) | is.null(pi_choices)){
      return()
    }
    #set_choices <- c(input$catchsetchoice, as.character(NA))
    #metric_choices <- c(input$catchrelchoice,"mean_weight", "catch stability", "SBSBF0", "relative effort stability", "relative cpue")
    #area_choices <- c("all", as.character(NA))
    #dat <- periodqs %>% filter(period != "Rest" & pi %in% pi_choices & set %in% set_choices & metric %in% metric_choices & area %in% area_choices)
    dat <- subset(periodqs(), period != "Rest" & piname %in% pi_choices)
    #scaling_choice <- input$radarscaling
    p <- myradar(dat=dat, hcr_choices=hcr_choices)#, scaling_choice)
    return(p)
  })

  # Time series comparisons
  output$plottimeseries <- renderPlot({
    hcr_choices <- input$hcrchoice
    pi_choices <- c("sbsbf0", "catch", "ffmsy")
    # If no HCR is selected then don't do anything
    if(is.null(hcr_choices)){
      return()
    }

    dat <- subset(yearqs(), pi %in% pi_choices)
    wormdat <- subset(worms(), pi %in% pi_choices)

    time_periods <- get_time_periods(app_params, nyears=dim(stock$biomass)[2])
    years <- as.numeric(dimnames(stock$biomass)$year)
    short_term <- years[time_periods[["short_term"]]]
    medium_term <- years[time_periods[["medium_term"]]]
    long_term <- years[time_periods[["long_term"]]]

    p <- quantile_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, time_period_lines=FALSE, show_spaghetti=input$spaghetti)
    p <- p + ggplot2::ylim(c(0,NA))
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("Value")
    return(p)


  })


  ## Ouputs for the PI plotting tab
  ## You can't have the same output ID but you can set up mutltiple output ids to have the same renderPlot() function
  #renderPIplot <- renderPlot({
  #  # Names of the HCRs and PIs in the pistore that we want to plot
  #  hcr_choices <- input$hcrchoice
  #  pi_choices <- input$pichoice
  #  # If no HCR or PI is selected then don't do anything
  #  if(is.null(hcr_choices) | is.null(pi_choices)){
  #    return()
  #  }
  #  plot_pi_choice(pis=pistore(), hcr_choices=hcr_choices, pi_choices=pi_choices, plot_choice=input$comparisontabs)
  #})
  #output$plotpibox <- renderPIplot
  #output$plotpimed <- renderPIplot
  #output$plotpiradar <- renderPIplot

  output$plotmajuroall <- renderPlot({
    hcr_choices <- input$hcrchoice
    if(is.null(hcr_choices)){
      return()
    }
  #  plot_majuro_all_stocks(timeseries=tsstore(), hcr_choices=hcr_choices, stock_params=get_stock_params())
    plot_majuro(dat=yearqs(), hcr_choices=hcr_choices, stock_params=get_stock_params())
  })

  #output$plottimeseries <- renderPlot({
  #  # Names of the HCRs that we want to plot
  #  hcr_choices <- input$hcrchoice
  #  # If no HCR is selected then don't do anything
  #  if(is.null(hcr_choices)){
  #    return()
  #  }
  #  plot_timeseries(timeseries=tsstore(), hcr_choices=hcr_choices, stock_params=get_stock_params(), show_spaghetti=input$spaghetti)
  #})

  # Termination script - needed when running from bat file
  session$onSessionEnded(function() {
      stopApp()
  })
}

# Run the app
shinyApp(ui, server)
