context("The Stock R6 class")
source("utility_test_funcs.R")

test_that("intialization", {
  # Initialisation
  niters <- round(runif(1, min=1, max=10))
  nyears <- round(runif(1, min=10, max=40))
  last_historical_timestep <- round(runif(1, min=5, max=nyears-4))
  # Make dummy stock_params
  stock_params = make_dummy_stock_params(nyears = nyears, last_historical_timestep=last_historical_timestep)
  stock <- Stock$new(stock_params=stock_params, niters = niters)

  # Check the dims of the array fields are right
  array_fields <-  c("biomass", "hcr_ip", "hcr_op", "catch", "effort")
  for (f in array_fields){
    expect_equal(dim(stock[[f]]),c(niters, nyears))
  }

  # Catches and biomass should have been filled up to the last historical year
  expect_true(all(!is.na(stock$biomass[,1:(last_historical_timestep+1)])))
  expect_true(all(is.na(stock$biomass[,(last_historical_timestep+2):nyears])))
  expect_true(all(!is.na(stock$catch[,1:(last_historical_timestep)])))
  expect_true(all(is.na(stock$catch[,(last_historical_timestep+1):nyears])))

  # Check fill catch history? Hard to when it's random
  # Check get_next_biomass

})

test_that(fill_next_biomass, {
  stock_params = make_dummy_stock_params()
  #mp_params = make_dummy_mp_params()
  stock <- Stock$new(stock_params=stock_params, niters = niters)

  # Just the first timestep
  for(ts in 2:(last_historical_timestep)){
    fB <- (stock$r / stock$p) * stock$biomass[,ts] * (1 - (stock$biomass[,ts] / stock$k) ^ stock$p)
    nextB <- stock$biomass[,ts] + fB - stock$catch[,ts]
    expect_equal(nextB, stock$biomass[,ts+1])
  }
})
