# Copyright 2018 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC

# Load packages ----
library(AMPLE)
library(shinyjs)

#-------------------------------------------------------------------------
# Some tables and plots before we get started

get_projection_pis <- function(stock, stock_params, app_params, current_timestep){
  # Return a data.frame of:
  # Projection (1,2,3,..)
  # Last catch
  # Av. catch
  # Last effort
  # Av. effort
  # Last SB/SBF=0
  # Av. SB/SBF=0
  # Prop. years in green

  # Current values are that last value that isn't NA
  final_catch <- apply(stock$catch, 1, function(x)x[max(which(!is.na(x)))])
  bk <- stock$biomass / stock_params$k
  final_sbsbf0 <- apply(bk, 1, function(x)x[max(which(!is.na(x)))])
  rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  final_effort <- apply(rel_effort, 1, function(x)x[max(which(!is.na(x)))])

  # Average values over projection period only
  proj_period <- (app_params$last_historical_timestep+1):dim(stock$biomass)[2]
  mean_catch <- apply(stock$catch[,proj_period,drop=FALSE],1,mean,na.rm=TRUE)
  mean_sbsbf0 <- apply(bk[,proj_period,drop=FALSE],1,mean,na.rm=TRUE)
  mean_effort <- apply(rel_effort[,proj_period,drop=FALSE],1,mean,na.rm=TRUE)
  prop_sb_lrp <- apply(bk[,proj_period,drop=FALSE] > stock_params$lrp,1,mean,na.rm=TRUE)
  bmsy <- stock_params$k / 2
  prop_sb_bmsy <- apply(stock$biomass[,proj_period,drop=FALSE] > bmsy,1,mean,na.rm=TRUE)
  dat <- data.frame(Projection = 1:nrow(stock$biomass),
                    final_catch = final_catch,
                    average_catch = mean_catch,
                    final_effort = final_effort,
                    average_effort = mean_effort,
                    final_sbsbf0 = final_sbsbf0,
                    average_sbsbf0 = mean_sbsbf0,
                    prop_sb_lrp = prop_sb_lrp,
                    prop_sb_bmsy = prop_sb_bmsy)
  # Trim the digits a bit
  #dat <- signif(dat,3)
  dat <- round(dat,3)
  final_year <- app_params$initial_year + current_timestep - 1
  # Better column names
  colnames(dat) <- c("Projection",
                     "Last catch",
                     "Average catch",
                     "Last effort",
                     "Average effort",
                     "Last SB/SBF=0",
                     "Average SB/SBF=0",
                     "Prop. SB/SBF=0>LRP",
                     "Prop. B > BMSY")
  return(dat)
}

plot_projection <- function(stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, ...){
  current_col <- "blue"
  prev_col <- "black"
  par(mfrow=c(3,1))
  
  # And then cock about with margins
  # Could do with a better layout?
  # Try as a dashboard?
  par(mar=c(0, 4.1, 5, 2.1))
  plot_biomass(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=quantiles, max_spaghetti_iters=max_spaghetti_iters, xaxt='n', xlab="", ghost_col=prev_col, last_col=current_col, ylim=c(0,1.1), ...)

  par(mar=c(2.5, 4.1, 2.5, 2.1))
  plot_catch(stock=stock, stock_params=stock_params, mp_params=mp_params, show_last=show_last, quantiles=quantiles, max_spaghetti_iters=max_spaghetti_iters, xaxt='n', xlab="", ghost_col=prev_col, true_col=current_col, ...)

  par(mar=c(5, 4.1, 0, 2.1))
  plot_releffort(stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, quantiles=quantiles, max_spaghetti_iters=max_spaghetti_iters, ghost_col=prev_col, true_col=current_col, ...)
}



# Combine the following two functions
plot_kobe_majuro_stock <- function(dat, stock_params, choice="kobe"){
  ymax <- max(c(2.0, 1.1*subset(dat, metric=="ffmsy" & level=="upper")$value, 1.1*subset(dat, metric=="ffmsy" & level=="median")$value), na.rm=TRUE)
  ymax <- min(ymax, 5.0) # In case of stock collapse

  # xmax and metric depends on choice
  if(choice=="kobe"){
    xmax <- max(c(2.0, 1.1*subset(dat, metric=="bbmsy" & level=="upper")$value, 1.1*subset(dat, metric=="bbmsy" & level=="median")$value), na.rm=TRUE)
    xlab <- "B / BMSY"
    xmetric <- "bbmsy"
  }
  if(choice=="majuro"){
    xmax <- 1.0
    xlab <- "SB / SBF=0"
    xmetric <- "bk"
  }
  # Set up the axes
  plot(x=c(0,xmax), y=c(0,ymax), type="n", xlim=c(0,xmax), ylim=c(0,ymax), xlab = xlab, ylab = "F / FMSY", xaxs="i", yaxs="i")
  # Set the colour panels
  if (choice=="kobe"){
    # The red one - top right
    rect(0.0,1.0,1.0,ymax, border="black", col="red", lty=1, lwd=1)
    # Bottom left - yellow
    rect(0.0, 0.0, 1.0, 1.0, border="black", col="yellow", lty=1, lwd=1)
    # Top right - yellow
    rect(1.0, 1.0, xmax, ymax, border="black", col="yellow", lty=1, lwd=1)
    # Bottom right - green
    rect(1.0, 0.0, xmax, 1.0, border="black", col="green", lty=1, lwd=1)
  }
  if (choice=="majuro"){
    # The big red one
    rect(0.0,0.0,stock_params$lrp,ymax, border="black", col="red", lty=1, lwd=1)
    # The orange one
    rect(stock_params$lrp,1.0,1.0,ymax, border="black", col="orange", lty=1, lwd=1)
    # The other one - leave white for now
    rect(stock_params$lrp,0.0,1.0,1.0, border="black", col="white", lty=1, lwd=1)
  }
  # Loop over HCRs
  # This is all redundant
  hcrs <- unique(dat$hcr)
  for (hcrcount in hcrs){
    hcrdat <- subset(dat, hcr==hcrcount)
    colour <- hcrdat$colour[1]
    # Add the medians as points and a line to tell the story
    medmetric <- subset(hcrdat, metric == xmetric & level=="median")$value
    medffmsy <- subset(hcrdat, metric == "ffmsy" & level=="median")$value
    points(x=medmetric, y=medffmsy, pch=16, col=colour)
    # Do a thick black line then overlay the real line
    lines(x=medmetric, y=medffmsy, lty=1, lwd=4, col="black")
    lines(x=medmetric, y=medffmsy, lty=1, lwd=3, col=colour)
    # Add quantile lines
    lowermetric <- subset(hcrdat, metric == xmetric & level=="lower")$value
    lowerffmsy <- subset(hcrdat, metric == "ffmsy" & level=="lower")$value
    uppermetric <- subset(hcrdat, metric == xmetric & level=="upper")$value
    upperffmsy <- subset(hcrdat, metric == "ffmsy" & level=="upper")$value
    # We always have one more B value so use FFMsy for final year
    finalyr <- max(which(!is.na(medffmsy))) 
    for (yr in 1:finalyr){
      lines(x=c(medmetric[yr],medmetric[yr]), y=c(lowerffmsy[yr],upperffmsy[yr]), lty=1, lwd=4, col="black")
      lines(x=c(medmetric[yr],medmetric[yr]), y=c(lowerffmsy[yr],upperffmsy[yr]), lty=1, lwd=3, col=colour)
      lines(x=c(lowermetric[yr], uppermetric[yr]),y=c(medffmsy[yr], medffmsy[yr]), lty=1, lwd=4, col="black")
      lines(x=c(lowermetric[yr], uppermetric[yr]),y=c(medffmsy[yr], medffmsy[yr]), lty=1, lwd=3, col=colour)
    }
    # Blob at the end
    finalbk <- medmetric[finalyr]
    finalffmsy <- medffmsy[finalyr]
    points(x=finalbk, y=finalffmsy, pch=21, col=colour, bg="white")
  }
}


plot_kobe_majuro_projections <- function(stock, stock_params, choice="kobe"){
  # F/FMSY for Kobe and Majuro
  fmsy <- stock_params$r / 2
  f <- stock$catch / stock$biomass 
  ffmsy <- f / fmsy
  ffmsy <- as.data.frame(ffmsy)
  ffmsy <- cbind(ffmsy, hcr=1:nrow(ffmsy))
  ffmsy <- tidyr::gather(ffmsy, key="year", value="value", -hcr)
  ffmsy <- cbind(ffmsy, metric="ffmsy", name="F / FMSY", level="median", colour="black",hcrlegend=NA, stringsAsFactors=FALSE)
  # BMSY = K / 2 for Kobe
  bmsy <- stock_params$k / 2
  bbmsy <- stock$biomass / bmsy
  bbmsy <- as.data.frame(bbmsy)
  bbmsy <- cbind(bbmsy, hcr=1:nrow(bbmsy))
  bbmsy <- tidyr::gather(bbmsy, key="year", value="value", -hcr)
  bbmsy <- cbind(bbmsy, metric="bbmsy", name="B / BMSY", level="median", colour="black",hcrlegend=NA, stringsAsFactors=FALSE)
  # SBSBF=0 for Majuro
  bk <- stock$biomass / stock_params$k
  bk <- as.data.frame(bk)
  bk <- cbind(bk, hcr=1:nrow(bk))
  bk <- tidyr::gather(bk, key="year", value="value", -hcr)
  bk <- cbind(bk, metric="bk", name="SB / SBF=0", level="median", colour="black",hcrlegend=NA, stringsAsFactors=FALSE)
  # Need to shunt the years by 1 as B in year Y is the result of F in year Y-1 
  # So add 1 to the F years
  bbmsy$year <- as.numeric(bbmsy$year)
  bk$year <- as.numeric(bk$year)
  ffmsy$year <- as.numeric(ffmsy$year)
  ffmsy$year <- ffmsy$year + 1
  # Only include common years between the three sets
  common_years <- intersect(intersect(bbmsy$year, ffmsy$year), bk$year)
  # Stick altogether
  dat <- rbind(subset(bbmsy, year %in% common_years), subset(ffmsy, year %in% common_years), subset(bk, year %in% common_years))
  # Set last hcr to blue
  last_hcr <- nrow(stock$catch)
  dat[dat$hcr==last_hcr,"colour"] <- "blue"
  # Call the actual plotting routine
  plot_kobe_majuro_stock(dat, stock_params, choice)
}

plot_yieldcurve_projections <- function(stock, stock_params, app_params, draw_trajectories){
  # x-axis = Effort
  # y-axis = Catch
  # In final year
  # Only plot this if running a long term projection?
  final_ts <- dim(stock$catch)[2]
  rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  final_rel_effort <- apply(rel_effort, 1, function(x)x[max(which(!is.na(x)))])
  final_catch <- apply(stock$catch, 1, function(x)x[max(which(!is.na(x)))])
  # Set x and y range depending on whether to show trjaectories or not
  if (draw_trajectories){
    xrange <- c(0,min(max(rel_effort, na.rm=TRUE)*1.1,5, na.rm=TRUE))
    yrange <- c(0, max(stock$catch, na.rm=TRUE) * 1.1)
  }
  if (!draw_trajectories){
    xrange <- c(0,min(max(final_rel_effort, na.rm=TRUE)*1.1,5, na.rm=TRUE))
    yrange <- c(0, max(final_catch, na.rm=TRUE) * 1.1)
  }

  plot(x=xrange, y=yrange, type="n", xlab="Final relative fishing effort", ylab="Final catch", xlim=xrange, ylim=yrange)
  points(x=final_rel_effort, y=final_catch, pch=16, cex=3)
  # Draw last one in blue
  nproj <- nrow(stock$catch)
  points(x=final_rel_effort[nproj], y=final_catch[nproj], col="blue", pch=16, cex=3)

  # Add lines of full trajectories?
  # Looks too messy
  if (draw_trajectories == TRUE){
    for (proj in 1:nrow(stock$catch)){
      lines(x=rel_effort[proj,], y=stock$catch[proj,], lty=3, col="black")
      points(x=rel_effort[proj,], y=stock$catch[proj,], col="black", cex=0.5)
    }
  
    # Draw last one in blue
    nproj <- nrow(stock$catch)
    lines(x=rel_effort[nproj,], y=stock$catch[nproj,], lty=3, col="blue")
    points(x=rel_effort[nproj,], y=stock$catch[nproj,], col="blue", cex=0.5)
  }
}

# Generic timeseries plot
plot_indiv_timeseries_base <- function(data, stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, yrange, ylab, add_grid=TRUE, xlab="Year", ghost_col="grey", true_col="black", ...){

  years <- as.numeric(dimnames(stock$biomass)$year)
  # Plot empty axis
  plot(x=years, y=years, type="n", ylim=c(yrange[1], yrange[2]), ylab=ylab, xlab=xlab, xaxs="i", yaxs="i",...)
  if (add_grid){
    grid()
  }
  # Get last iteration 
  last_iter <- dim(data)[1]
  # If we have more than X iters, draw envelope of iters
  if(last_iter > max_spaghetti_iters){
    # Draw ribbon
    draw_ribbon(x=years, y=data, quantiles=quantiles)
    # Add spaghetti
    for (iter in 1:nspaghetti){
      lines(x=years, y=data[iter,], lty=spaghetti_lty, lwd=spaghetti_lwd, col=spaghetti_col)
    }
  }
  # Else plot individual iters
  else{
    # Plot all iters as ghosts
    if (last_iter > 1){
      for (i in 1:last_iter){
        lines(x=years, y=data[i,], col=ghost_col, lwd=2, lty=1)
      }
    }
  }
  # Current iteration
  if(show_last){
    lines(x=years, y=data[last_iter,], col=true_col, lwd=2, lty=1)
  }

}

plot_releffort <- function(stock, stock_params, mp_params, app_params=NULL, show_last=TRUE, max_spaghetti_iters=50, quantiles, nspaghetti=5, add_grid=TRUE, ...){
  years <- as.numeric(dimnames(stock$biomass)$year)
  rel_effort <- sweep(stock$effort, 1, stock$effort[,app_params$last_historical_timestep], "/")
  # Set Ylim - use same as HCR plot
  ymax <- max(c(rel_effort * 1.1, 1.0), na.rm=TRUE)
  ymax <- min(10, ymax, na.rm=TRUE)
  yrange <- c(0, ymax)

  # Plot it
  plot_indiv_timeseries_base(data=rel_effort, stock=stock, stock_params=stock_params, mp_params=mp_params, app_params=app_params, show_last=show_last, max_spaghetti_iters=max_spaghetti_iters, quantiles=quantiles, nspaghetti=nspaghetti, yrange=yrange, ylab="Relative effort", add_grid=add_grid, ...)

  # Add 1 line
  lines(x=years,y=rep(1,length(years)), lty=2)

}



#---------------------------------------------------------
# The actual Shiny stuff
#---------------------------------------------------------


# UI
ui <- navbarPage(
  title="Introducing Projections",
  tabPanel("Projections",
  useShinyjs(),  # For greying out buttons
    # Controls and what not
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        # HCR is either constant catch or constant effort
        mp_params_setterUI("mpparams", mp_visible=c("Constant effort", "Constant catch"), title="Select projection type: catch or effort", input_label="Projection type", init_constant_catch=50, init_constant_effort=1.0),
        br(),
        # Controls for management cycle length
        tags$span(title="Step length",
          numericInput("projlen", label="Step length", value=3, min = 1, step = 1)
        ),
        br(),
        # Buttons:
        # Advance one management cycle
        # New projection - unless this is triggered automagically when we reach the end of the current projection?
        # Reset all projections
        tags$span(title="Project one management cycle.",
          actionButton("project", "Project")
        ),
        tags$span(title="Set up next projection",
          actionButton("setnext", "Set up next projection")
        ),
        tags$span(title="Reset all projections.",
          actionButton("reset", "Reset all")
        ),
        br(),
        br(),
        # Majuro / Kobe option
        selectInput("kobemajuro", "Plot selection", c("Kobe" = "kobe", "Majuro" = "majuro", "Yield curve" = "yieldcurve")),
        br(),

        # Stochasticity options - only biological variability
        stoch_params_setterUI("stoch", init_prod_sigma=0.0, show_var=FALSE, show_biol_est_sigma = FALSE, show_biol_est_bias = FALSE)
      ),
      #
      mainPanel(width=9,
        fluidRow(
          # Top left - 3 stacked plots
          # SB/SBF=0
          # Catch
          # Effort (relative to something)
          column(6,
            plotOutput("plotall",height="500px")
          ),
          # Top right
          # Kobe / Majuro (with switch)
          column(6,
            plotOutput("MajKobeplot",height="500px"),
            
            # conditional panel - only show if yield-curve = TRUE
            conditionalPanel(condition="input.kobemajuro == 'yieldcurve'", tags$span(title="Show projection trajectories", checkboxInput(inputId="showtraj", label="Show trajectores", value=FALSE)))
            
            
          )
        ),
        # PI Table underneath
        fluidRow(
          column(12,
            textOutput("currentts"),
            tableOutput("pis")#,
          )
        )
      )
    )
  ),
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
          #stoch_params_setterUI("stoch", init_prod_sigma=0.15, init_est_sigma=0.03, init_est_bias=0.0),
          stock_params_setterUI("stock"),
          # Total number of years (including historical years)
          numericInput("nyears", "Number of years", value = 50, min=20, max=100, step=1)
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
        h1("Tutorials"),
        p("A detailed tutorial can be found at these links:"),
        a("Tutorial (pdf)",target="_blank",href= "introProjections.pdf"), 
        br(),
        a("Tutorial (html)",target="_blank",href="introProjections.html") 
      )
    )
  ),
  tabPanel("About",
    sidebarLayout(
      sidebarPanel(width=3,
        br(),
        img(src = "spc.png", height = 100),
        br(),
        br(),
        amped_maintainer_and_licence()
      ), 
      mainPanel(width=9,
        spc_about()
      )
    )
  )
)

# Do stuff
server <- function(input, output,session) {

  # Global settings - year range and quantiles
  app_params <- list(initial_year = 2009, last_historical_timestep = 10)
  app_params$historical_timesteps <- 1:app_params$last_historical_timestep
  quantiles <- c(0.20, 0.80)

  # Getting the stochasticity parameters
  # These are located in the Projections tab
  # We only want biological variability - is this still OK?
  get_stoch_params <- callModule(stoch_params_setter, "stoch") 
  # Getting the life history parameters
  # These are located in the Settings tab
  get_lh_params <- callModule(stock_params_setter, "stock") 
  # Join these together into a single object to be passed to the funcs - bit clumsy as I will have to do this in all the servers
  get_stock_params <- reactive({
    sp <- get_stoch_params()
    lh <- get_lh_params()
    out <- c(sp, lh)
    return(out)
  })

  # Get the management procedure parameters
  # These are located in the Projections tab
  get_mp_params <- callModule(mp_params_setter, "mpparams") 

  # Make the stock - empty and with no dims
  stock <- create_stock()

  # We have 1 initial iter. More are added to it but this is accounted for using the iter() function
  # iter is really a separate projection - called iter for use with existing code
  niters <- 1 

  # Initialise the stock with a appropriate history
  # Use isolate else error (function tries to be reactive to changes in get_stock_params() and stock itself
  # Do we need this here? Isn't it already handled in the reset procedure?
  isolate(reset_stock(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, initial_biomass=get_stock_params()$b0, nyears=input$nyears, niters=niters))
  
  # Set iter as a reactive value. If this changes, it triggers other stuff
  iter <- reactiveVal(1) 
  current_timestep <- reactiveVal(app_params$last_historical_timestep + 1)

  # PIs for a single projection
  pitemp <- reactiveVal(NULL)


  # Reset procedure
  # Clear out stock history for all iterations
  observe({
    req(input$nyears)
    # If HCR changes, stock LH changes or reset button is pressed the reset procedure gets triggered
    #mp_params <- get_mp_params()
    stock_params <- get_stock_params() 
    input$reset # reset above on Projections tab
    # Reset iterations and current_timestep and PIs
    iter(1)
    current_timestep(app_params$last_historical_timestep + 1)
    pitemp(NULL)
    # Need isolate here because calling reset_stock() causes stock to change which triggers this observe() resulting
    # in an infinite loop
    isolate(reset_stock(stock=stock, stock_params=stock_params, mp_params=get_mp_params(), app_params=app_params, initial_biomass=stock_params$b0, nyears=input$nyears, niters=niters))
  })

  # Turn on the setnext button only if we have run out of time
  observe({
    shinyjs::toggleState("setnext", current_timestep() >= dim(stock$biomass)[2])
  })
  
  observeEvent(input$setnext, {
    # What to do if we haven't finished the current one?
    # Button does not nothing - you have to complete the current projection
    if(current_timestep() < dim(stock$biomass)[2]){
      return()
    }
    # Reset time, add another iter and add another row to the stock
    current_timestep(app_params$last_historical_timestep + 1)
    iter(iter() + 1)
    # If nrow stock < iter, add a row biomass, biomass_obs and catch
    # This row will contain the values for the new iteration
    if (nrow(stock$biomass) < iter()){
      # rbind is dropping dimnames - really annoying - so hack
      dnames <-names(dimnames(stock$biomass)) 
      stock$biomass <- rbind(stock$biomass,stock$biomass[1,])
      stock$hcr_ip<- rbind(stock$hcr_ip,stock$hcr_ip[1,])
      stock$hcr_op<- rbind(stock$hcr_op,stock$hcr_op[1,])
      stock$catch <- rbind(stock$catch,stock$catch[1,])
      stock$effort <- rbind(stock$effort,stock$effort[1,])
      # Empty out future years
      stock$biomass[iter(),(app_params$last_historical_timestep+2):dim(stock$biomass)[2]] <- NA
      stock$hcr_ip[iter(),(app_params$last_historical_timestep+2):dim(stock$hcr_ip)[2]] <- NA
      stock$hcr_op[iter(),(app_params$last_historical_timestep+2):dim(stock$hcr_op)[2]] <- NA
      stock$catch[iter(),(app_params$last_historical_timestep+1):dim(stock$catch)[2]] <- NA
      stock$effort[iter(),(app_params$last_historical_timestep+1):dim(stock$effort)[2]] <- NA
      # Bit hacky but rbind is dropping the dimnames names - annoying
      names(dimnames(stock$biomass)) <- dnames 
      names(dimnames(stock$effort)) <- dnames 
      names(dimnames(stock$hcr_ip)) <- dnames 
      names(dimnames(stock$hcr_op)) <- dnames 
      names(dimnames(stock$catch)) <- dnames 
    }
  })

  # Turn on the project button only if we have enough timesteps left
  observe({
    shinyjs::toggleState("project", current_timestep() <= dim(stock$biomass)[2])
  })

  # Projection procedure
  # The main event
  observeEvent(input$project, {
    # If we have reached end of simulation, don't do anything
    if (current_timestep() > dim(stock$biomass)[2]){
      return()
    }
    # Pass current iter to project()
    # project() takes entire stock
    # Here we only want the current iteration
    # (subset out using fancy lapply with [ operator)
    stock_iter <- lapply(reactiveValuesToList(stock), '[', iter(),,drop=FALSE) 
    # Update hcr_op  - # But if noise added here it will result in different value?
    # Necessary in this app because each iteration and timestep (row of stock) can have a different HCR
    mp_params <- get_mp_params()
    stock_iter$hcr_op[,current_timestep()] <- get_hcr_op(stock=stock_iter, stock_params=get_stock_params(), mp_params, yr=current_timestep()-mp_params$timelag)
    max_timestep <- min(dim(stock$biomass)[2],current_timestep() + input$projlen - 1)
    out <-  project(stock_iter,
                    timesteps=c(current_timestep(), max_timestep),
                    stock_params=get_stock_params(),
                    mp_params=mp_params,
                    app_params=app_params)
    # Update whole timeseries or just new elements?
    stock$biomass[iter(),] <- out$biomass
    stock$effort[iter(),] <- out$effort
    stock$hcr_ip[iter(),] <- out$hcr_ip
    stock$hcr_op[iter(),] <- out$hcr_op
    stock$catch[iter(),] <- out$catch
    # Update the timestep
    current_timestep(max_timestep+1)
  })

  # ************************************************
  # Output stuff
  output$plotall <- renderPlot({
    plot_projection(stock=stock, stock_params=get_stock_params(), mp_params=get_mp_params(), app_params=app_params, show_last=TRUE, max_spaghetti_iters=500, quantiles=quantiles, cex.lab=1.5, cex.axis=1.5)
  })

  output$MajKobeplot  <- renderPlot({
    if(input$kobemajuro == "kobe"){
      plot_kobe_majuro_projections(stock=stock, stock_params=get_stock_params(), choice="kobe")
    }
    if(input$kobemajuro == "majuro"){
      plot_kobe_majuro_projections(stock=stock, stock_params=get_stock_params(), choice="majuro")
    }
    if(input$kobemajuro == "yieldcurve"){
      # Add an extra check box for drawing trajectories
      plot_yieldcurve_projections(stock=stock, stock_params=get_stock_params(), app_params=app_params, draw_trajectories=input$showtraj)
    }
  })

  output$pis <- renderTable({
    tabout <- get_projection_pis(stock=stock, stock_params=get_stock_params(), app_params=app_params, current_timestep=current_timestep())
    tabout[,"Notes"] <- ""
    tabout[tabout[,"Last SB/SBF=0"] <= 1e-3,"Notes"] <- '<img src="deadfish.png" height="25"></img>'
    return(tabout)
    },
    sanitize.text.function = function(x) x,  # Necessary to include else you lose the figure
    caption= "Performance indicators",
    auto=TRUE
  )

  output$currentts  <- renderText({
    paste("Start of year: ", min(current_timestep(), dim(stock$biomass[2])) + app_params$initial_year-1, sep="")
  })
}

# Run the app
shinyApp(ui, server)
