context("project method")

test_that("project works over iterations",{
  # Bits that we need - copying the input controls
  # There is a lot of set up...
            
  # MP bits
  input_mp <- list(
    blim_belbow = c(0.2, 0.5),
    cmin_cmax = c(10, 140), 
    constant_catch_level = 50,
    constant_effort_level = 1,
    emin_emax = c(0.1, 0.5),
    hcr_type = "threshold_catch")
  mp_params <- mp_params_switcheroo(input_mp)

  input_stoch <- list(
    biol_est_bias = 0,
    biol_est_sigma = 0,
    biol_prod_sigma = 0,
    show_var <- TRUE)
  stoch_params <- set_stoch_params(input_stoch)

  input_lh <- list(
    stock_history = "fully",
    stock_lh = "medium")
  lh_params <- get_lh_params(input_lh)

  stock_params <- c(stoch_params, lh_params)
  app_params <- list(initial_year = 2009, last_historical_timestep = 10)

  # Make the null stock and fill it up
  stock <- list( biomass = NULL, hcr_ip = NULL, hcr_op = NULL, effort = NULL, catch = NULL)

  nyears <- 20
  niters <- 1000

  # Make the stock
  stock <- reset_stock(stock = stock, stock_params = stock_params, mp_params = mp_params, app_params = app_params, initial_biomass = stock_params$b0, nyears = nyears, niters = niters)

  # Fuck that took a long time
  stock1 <- stock
  stock2 <- stock

  # Project 1 iter at a time
  # Taken from MeasuringPerformance
  for (iter in 1:dim(stock1$biomass)[1]){
    stock_iter <- lapply(stock1, '[', iter,,drop=FALSE) 
    out <-  project(stock_iter,
                    timesteps=c((app_params$last_historical_timestep+1),dim(stock1$biomass)[2]),
                    stock_params=stock_params,
                    mp_params=mp_params,
                    app_params=app_params)
    stock1$biomass[iter,] <- out$biomass
    stock1$effort[iter,] <- out$effort
    stock1$hcr_ip[iter,] <- out$hcr_ip
    stock1$hcr_op[iter,] <- out$hcr_op
    stock1$catch[iter,] <- out$catch
  }

  # Project all iters at the same time
  stock2 <-  project(stock2,
                  timesteps=c((app_params$last_historical_timestep+1),dim(stock2$biomass)[2]),
                  stock_params=stock_params,
                  mp_params=mp_params,
                  app_params=app_params)
  # Are they the same?
  expect_equal(stock1, stock2)
  # Much quicker

  # What about with noise? Can't tell because random!
  # But summary stats should be same?



})
