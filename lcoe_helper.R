# inputs <- list(
#   ty = 30, # T, Useful life for economic purposes (years)
#   sp = 1.45, # SP, System price ($/W)
#   cf = 0.292, # CF, Average capacity factor of the generation capacity
#   r = 0.075, # r, Interest rate, weighted average cost of capital WACC
#   x = 0.995, # x, System degradation factor
#   fix_op = 20.305, # F, Fixed operating cost ($/kWh-year)
#   var_op = 0.0021, # W, Variable operating cost ($/kWh)
#   fuel_cost = 0, # Fuel cost ($/kWh)
#   co2_cost = 12.95, # Carbon dioxide emissions cost ($/tCO2e)
#   co2_perf = 0, # Emissions performance (kg CO2e/kWh)
#   
#   method = 4, # Tax depreciation method
#   itc = 0.3, # i, Investment tax credit
#   a = 0.4384 # α, Effective corporate income tax rate
#   
#   # d = 0.5 # δ, Capitalization discount for depreciation purposes
# )

hours <- 24 * 365 # hours in 1 year

# adapted from https://github.com/jumpingrivers/optiRum/blob/main/R/PV.R
pv <- \(rate, nper, pmt) -pmt / rate * (1 - 1 / (1 + rate) ^ nper)

compute_lcoe <- \(params, deps) {
  # message(params)
  i <- 1:params$ty # years 1 to lifetime
  
  life_capacity <- sum(params$x ^ i / (1 + params$r) ^ i)
  life_output <- hours * params$cf * life_capacity
  
  # c = levelized capacity cost
  cap_cost <- (params$sp * 1000) / life_output
  
  # f = levelized fixed operating cost
  fix_cost <- params$fix_op / (hours * params$cf) * (-pv(params$r, params$ty, 1)) / life_capacity
  
  # w = time-averaged variable cost
  var_cost <- params$var_op + params$fuel_cost + ((params$co2_cost / 1000) * params$co2_perf)
  
  # compute depreciation PVs and sum for each method
  # deps <- depreciations |>
  #   pivot_longer(-year, names_to = "method", values_to = "d_i") |>
  dep_sums <- deps |>
    mutate(pv_i = d_i / (1 + params$r) ^ year) |>
    group_by(method) |>
    summarise(d_t = sum(pv_i)) |>
    select(method, d_t) |>
    deframe()

  # ∆ = tax factor
  tax_factor <- (1 - params$itc - params$a * (1 - 0.5 * params$itc) * dep_sums[[params$method]]) / (1 - params$a)
  
  lcoe <- fix_cost + var_cost + cap_cost * tax_factor
  
  c(cap_cost = cap_cost, tax_factor = tax_factor, fix_cost = fix_cost, var_cost = var_cost, lcoe = lcoe)
}

# ex <- compute_lcoe(inputs, dep_vals)
# round(ex, 6) == c(0.050544, 0.665328, 0.008359, 0.0021, 0.044088)
