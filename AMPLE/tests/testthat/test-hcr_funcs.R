test_that("get_assessment_hcr_ip", {
  # Test assessment type

  # No estimation error
  mp_params <- make_dummy_mp_params(mp_analysis = "assessment")
  stock_params <- make_dummy_stock_params()
  stock <- Stock$new(stock_params=stock_params, mp_params=mp_params, niters = 1)
  for(yr in 2:(stock$last_historical_timestep+1)){
    expect_equal(get_hcr_ip(stock, mp_params, yr), stock$biomass[,yr] / stock$k)
  }

  # With bias
  est_bias <- runif(n = 1, min = -0.5, max = 0.5)
  mp_params <- make_dummy_mp_params(mp_analysis = "assessment", est_bias = est_bias)
  stock_params <- make_dummy_stock_params()
  stock <- Stock$new(stock_params=stock_params, mp_params=mp_params, niters = 1)
  for(yr in 2:(stock$last_historical_timestep+1)){
    expect_equal(get_hcr_ip(stock, mp_params, yr), pmin((stock$biomass[,yr] / stock$k) * (1 + est_bias)))
  }

})

