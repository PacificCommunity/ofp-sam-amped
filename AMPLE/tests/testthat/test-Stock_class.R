test_that("intialization", {
  # Initialisation
  niters <- round(runif(1, min=1, max=10))
  nyears <- round(runif(1, min=10, max=40))
  last_historical_timestep <- round(runif(1, min=5, max=nyears-4))
  # Make dummy stock_params
  stock_params = make_dummy_stock_params(nyears = nyears, last_historical_timestep=last_historical_timestep)
  mp_params = make_dummy_mp_params()
  stock <- Stock$new(stock_params=stock_params, mp_params=mp_params, niters = niters)

  # Check the dims of the array fields are right
  array_fields <-  c("biomass", "hcr_ip", "hcr_op", "catch", "effort")
  for (f in array_fields){
    expect_equal(dim(stock[[f]]),c(niters, nyears))
  }

  # Biomass filled up to the last historical year + 1
  expect_true(all(!is.na(stock$biomass[,1:(last_historical_timestep+1)])))
  expect_true(all(is.na(stock$biomass[,(last_historical_timestep+2):nyears])))
  # Catches filled up to the last historical year
  expect_true(all(!is.na(stock$catch[,1:(last_historical_timestep)])))
  expect_true(all(is.na(stock$catch[,(last_historical_timestep+1):nyears])))
  expect_true(all(!is.na(stock$hcr_ip[,last_historical_timestep+1])))
  expect_true(all(is.na(stock$hcr_ip[,(last_historical_timestep+2):nyears])))
  expect_true(all(!is.na(stock$hcr_op[,last_historical_timestep+1])))
  expect_true(all(is.na(stock$hcr_op[,(last_historical_timestep+2):nyears])))
})

test_that("fill_biomass", {
  stock_params = make_dummy_stock_params()
  last_historical_timestep <- stock_params$last_historical_timestep
  mp_params = make_dummy_mp_params()
  niters <- round(runif(1, min=1, max=10))
  stock <- Stock$new(stock_params=stock_params, mp_params=mp_params, niters = niters)

  # Just the first timestep
  for(ts in 2:(last_historical_timestep)){
    fB <- (stock$r / stock$p) * stock$biomass[,ts] * (1 - (stock$biomass[,ts] / stock$k) ^ stock$p)
    nextB <- stock$biomass[,ts] + fB - stock$catch[,ts]
    expect_equal(nextB, stock$biomass[,ts+1])
  }
})

# Project is tricky to write a test for
# Should do all iters - just checks that all iters have been filled with non-NAs
test_that("project", {
  niters <- round(runif(1, min=1, max=10))
  nyears <- round(runif(1, min=10, max=40))
  last_historical_timestep <- round(runif(1, min=5, max=nyears-4))
  # Make dummy stock_params
  stock_params = make_dummy_stock_params(nyears = nyears, last_historical_timestep=last_historical_timestep)
  mp_params = make_dummy_mp_params()
  # Fills up biomass and hcr_ip and hcr_op up timestep+1
  stock <- Stock$new(stock_params=stock_params, mp_params=mp_params, niters = niters)
  
  # Just one timestep - the one after last_historical timestep
  # Should also fill up biomass, hcr_ip and hcr_op if room
  ts <- last_historical_timestep+1
  stock$project(timesteps=ts, mp_params=mp_params)
  # Catch and effort should not be NA in the timestep we've just projected to
  expect_true(all(!is.na(stock$catch[,ts])))
  expect_true(all(!is.na(stock$effort[,ts])))
  # Biomass, hcr_ip and hcr_op should go an extra timestep
  expect_true(all(!is.na(stock$biomass[,ts+1])))
  expect_true(all(!is.na(stock$hcr_ip[,ts+1])))
  expect_true(all(!is.na(stock$hcr_op[,ts+1])))
  
  # Project over several timesteps
  timesteps <- c((last_historical_timestep+1),nyears)
  stock$project(timesteps=timesteps, mp_params=mp_params)
  
  # Everything beyond last_historical_timestep + 1, should be filled with non-NA values
  expect_true(all(!is.na(stock$biomass[,timesteps])))
  expect_true(all(!is.na(stock$hcr_ip[,timesteps])))
  expect_true(all(!is.na(stock$hcr_op[,timesteps])))
  expect_true(all(!is.na(stock$catch[,timesteps])))
  expect_true(all(!is.na(stock$effort[,timesteps])))
})