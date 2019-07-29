# Copyright 2018 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC

library(AMPLE)

# User interface ----
ui <- navbarPage(
  title="Introducing Uncertainty and Performance Indicators",
  tabPanel("Projections",
  sidebarLayout(
    sidebarPanel(width=3,
      br(),
      img(src = "spc.png", height = 100),
      br(),
      br(),
      mp_params_setterUI("mpparams", mp_visible=c("Threshold catch", "Constant catch", "Threshold effort", "Constant effort"), init_thresh_max_catch=140, init_thresh_belbow=0.5),
      br(),
      tags$span(title="Run the projection for one more iteration.",
        actionButton("project", "Run projection")
      ),
      tags$span(title="Reset the projection.",
        actionButton("reset", "Reset")
      ),
      br(),
      # Stochasticity options
      stoch_params_setterUI("stoch", init_prod_sigma=0.0, init_est_sigma=0.0, init_est_bias=0.0, show_var=FALSE)
    ),
    mainPanel(width=9,
      # Column 1 - 2 rows
      # I want the height of these two rows to be the same as the height of the 3 rows in the previous column
      # So use fluidRow
      column(6,
        fluidRow(
          tags$span(title="The HCR. The blue points show the inputs and outputs from all years of the the last iteration. The grey points show the inputs and outputs from all years from all iterations. This enables you to see which parts of the HCR shape are most used.",
            plotOutput("plothcr")
          )
        ),
        fluidRow(
          #div(tableOutput("hcrpis"), style = "font-size:100%"),
          tags$span(title="The number of iterations run so far.",
          textOutput("itercount")),
          checkboxInput("show_pis", label = "Show performance indicators", value=FALSE),
          conditionalPanel(condition="input.show_pis == true",
            tags$span(title="A table of various performance indicators calculated over the short-, medium- and long- term. The value is the median. The values in the brackets are the 20th and 80th percentiles respectively. See the information tab for more details", tableOutput("hcrpis"), style = "font-size:100%")
          )
        )
      ),
      # Column 3 - has 3 rows
      column(6,
        fluidRow(
          tags$span(title="Plot of SB/SBF=0. The black line shows the 'true' biomass in the current iteration. The blue line shows the estimated biomass in the current iteration. The grey lines show the previous iterations. The histogram shows the range of values in the final year.",
            plotOutput("plotbiomasshisto",height="250px")
          )
        ),
        fluidRow(
          tags$span(title="Plot of the catch. The black line shows the current iteration. The grey lines show the previous iterations. The histogram shows the range of values in the final year.",
            plotOutput("plotcatchhisto",height="250px")
          )
        ),
        fluidRow(
          tags$span(title="Plot of the CPUE relative to the CPUE in the year 2000. The black line shows the current iteration. The grey lines show the previous iterations. The histogram shows the range of values in the final year.",
            plotOutput("plotrelcpuehisto",height="250px")
          )
        )
      )
    )
  )), 
  # Tab for choosing stock parameters, stock history, no. iterations etc
  tabPanel("Settings",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br()
      ),
      mainPanel(width=9,
        fluidRow(column=12,
          stock_params_setterUI("stock"),
          # Total number of years (including historical years)
          numericInput("nyears", "Number of years", value = 30, min=20, max=100, step=1)
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
        h1("Instructions"),
        p("Variability can be included in the projection in two ways: through variability in the stock productivity and through the estimated level of stock biomass being different to the true level of the stock biomass. These options are initially turned off. The options can be seen by clicking on the ", strong("Show variability options"), "box."),
        p("Biological productivity variability represents the variability of the natural procesess of the stock, for example growth and natural mortality. Increasing the variability will increase the 'bumpiness' of the stock trajectory. As biological variability is always encountered in fisheries it is essential that a selected HCR is robust to the variability."),
        p("Estimation error simulates the difference between the true level of the stock biomass and the estimated level. Unfortunately, the true abundance of a fish stock is never known. Instead, estimates of abundance are made, for example using stock assessment models. The HCR uses the estimated biomass, not the true biomass. This means that the catch limit that is set by the HCR is based on estimated biomass. If the biomass is estimated poorly the resulting catch limit set by the HCR may not be appropriate."),
        p("Here, estimation error is modelled using two different processes: random error and consistent bias (positive or negative). The bias represents situations where the biomass is consistently over or under estimated."),
        p("When estimation error is active the biomass plot shows two lines. The black line shows the true biomass, the blue line shwows the estimated biomass. It is the blue line that feeds the HCR. Increasing the estiamation bias and variability will increase the difference between these lines.")
      )
    )
  ),
  tabPanel("About",
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

server <- function(input, output,session) {
  
  # Here niters is the initial number of iters in a new stock object when reset is triggered

  # App parameters
  app_params <- list(initial_year = 2009, last_historical_timestep = 10)
  app_params$historical_timesteps = 1:app_params$last_historical_timestep
  #quantiles <- c(0.20, 0.80)
  quantiles <- c(0.01, 0.05, 0.20, 0.5, 0.80, 0.95, 0.99)
  # Object to store the performance indicators - gets updated through time
  pitemp <- reactiveVal(NULL)
  get_mp_params <- callModule(mp_params_setter, "mpparams") 
  get_stoch_params <- callModule(stoch_params_setter, "stoch") 
  get_lh_params <- callModule(stock_params_setter, "stock") 
  # Join these together into a single object to be passed to the funcs
  get_stock_params <- reactive({
    sp <- get_stoch_params()
    lh <- get_lh_params()
    out <- c(sp, lh)
    return(out)
  })
  
  # Create the stock
  stock <- create_stock()
  initial_niters <- 1 # We have 1 initial iter. More are added to it but this is accounted for using the iter() function
  isolate(reset_stock(stock = stock, stock_params = get_stock_params(), mp_params = get_mp_params(), app_params = app_params, initial_biomass = get_stock_params()$b0, nyears = input$nyears, niters = initial_niters))
  
  # Set iter as a reactive value. If this changes, it triggers other stuff
  iter <- reactiveVal(0) 
  
  # Reset 
  observe({
    req(input$nyears)
    # If any of the following change the observer gets triggered
    input$reset
    stock_params <- get_stock_params() # The output is stored here because we need it for the reset_stock() function
    mp_params <- get_mp_params()
    # Reset iterations and inidicators
    iter(0)
    pitemp(NULL)
    # Isolate here because calling reset_stock() causes stock to change which triggers this observe() resulting in infinite loop
    isolate(reset_stock(stock = stock, stock_params = stock_params, mp_params = mp_params, app_params = app_params, initial_biomass = stock_params$b0, nyears = input$nyears, niters = initial_niters))
  })
  
  # Project works by binding new rows (iterations) to the existing ones
  # Project all timesteps
  # Preallocate instead of adding rows - inefficient
  observeEvent(input$project, {
    iter(iter() + 1)
    # If nrow stock < iter, add new rows for biomass, effort, catch etc.
    # This new will contain the values for the new iteration
    if (nrow(stock$biomass) < iter()){
      # rbind is dropping names of dimnames - really annoying - so hack
      dnames <-names(dimnames(stock$biomass)) 
      stock$biomass <- rbind(stock$biomass,stock$biomass[1,])
      #stock$biomass_obs <- rbind(stock$biomass_obs,stock$biomass_obs[1,])
      stock$hcr_ip<- rbind(stock$hcr_ip,stock$hcr_ip[1,])
      stock$hcr_op<- rbind(stock$hcr_op,stock$hcr_op[1,])
      stock$catch <- rbind(stock$catch,stock$catch[1,])
      stock$effort <- rbind(stock$effort,stock$effort[1,])
      # Bit hacky but rbind is dropping the dimnames names
      names(dimnames(stock$biomass)) <- dnames 
      names(dimnames(stock$effort)) <- dnames 
      names(dimnames(stock$hcr_ip)) <- dnames 
      names(dimnames(stock$hcr_op)) <- dnames 
      names(dimnames(stock$catch)) <- dnames 
    }

    # Pass in current iter
    # project() takes entire stock
    # Here we only want one iteration - subset out using fancy lapply with [ operator
    stock_iter <- lapply(reactiveValuesToList(stock), '[', iter(),,drop=FALSE) 
    out <-  project(stock_iter,
                    timesteps=c((app_params$last_historical_timestep+1),dim(stock$biomass)[2]),
                    stock_params=get_stock_params(),
                    mp_params=get_mp_params(),
                    app_params=app_params)
    stock$biomass[iter(),] <- out$biomass
    stock$effort[iter(),] <- out$effort
    stock$hcr_ip[iter(),] <- out$hcr_ip
    stock$hcr_op[iter(),] <- out$hcr_op
    stock$catch[iter(),] <- out$catch

    # Get summary PIs 
    #piqs <- get_summary_pis(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params,
    #                        quantiles=c(0.05,quantiles, 0.95), pi_choices=c("bk", "problrp", "catch", "diffcatch", "relcpue"))
    piout <- get_summaries(stock=stock, stock_params=get_stock_params(), app_params=app_params, quantiles=quantiles)
    pitemp(piout)
  })
  
  # Call the HCR plot function
  output$plothcr <- renderPlot({
    plot_hcr(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params)
  })
  
  output$plotbiomasshisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="biomass", quantiles=quantiles[c(3,5)])
  })

  output$plotcatchhisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="catch", quantiles=quantiles[c(3,5)])
  })

  output$plotrelcpuehisto <- renderPlot({
    plot_metric_with_histo(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), metric="relcpue", app_params=app_params, quantiles=quantiles[c(3,5)])
  })

  output$itercount <- renderText({
    paste("Number of iterations: ", iter(), sep="")
  })


  output$hcrpis <- renderTable({
    # Don't print table unless project has been pressed
    if (is.null(pitemp())){
      return()
    }
    # Use pitemp() to fill table
    current_pi_table(pitemp()$periodqs)
    },
    rownames = TRUE,
    caption= "Performance indicators",
    auto=TRUE
  )

  # Termination script - needed when running from bat file
  session$onSessionEnded(function() {
      stopApp()
  })
  
}

# Run the app
shinyApp(ui, server)
