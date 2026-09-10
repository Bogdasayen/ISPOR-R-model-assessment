# Run from the repository root: Rscript tests/regression_tests.R
# Base R only; reads the supplied PSA CSVs and runs the real model functions.
source('generate_input_parameters.R')
source('generate_hazards.R')
source('generate_transition_matrix.R')
source('generate_state_utilities.R')
source('generate_state_costs.R')
source('generate_model_outputs.R')

state_names <- c('AF Well', 'Stroke', 'ICH', 'MI', 'Bleed', 'Dead')
n_states <- length(state_names)
treatment_names <- c('Coumarin', 'Apixaban', 'Dabigatran')
n_treatments <- length(treatment_names)
event_names <- c('Stroke', 'MI', 'Bleed', 'ICH', 'Death', 'SE', 'TIA')
n_events <- length(event_names)
n_samples <- 100
failures <- character()
check <- function(ok, label) {
  cat(if (isTRUE(ok)) 'PASS' else 'FAIL', label, '\n')
  if (!isTRUE(ok)) failures <<- c(failures, label)
}
near <- function(x, y) isTRUE(all.equal(x, y, tolerance = 1e-10))

set.seed(14142234)
inputs <- generate_input_parameters(n_samples)
utility_names <- c('AF Well utility', 'Stroke utility', 'MI utility',
                   'ICH utility', 'Bleed utility')
for (name in utility_names) {
  check(length(unique(inputs[, name])) > 1, paste(name, 'retains PSA variation'))
  check(all(inputs[, name] <= 0.25), paste(name, 'is capped at one quarter'))
}
check(identical(inputs[, 'Bleed utility'], inputs[, 'Stroke utility']),
      'Bleed retains the same utility draws as Stroke')

# Supplement the real-draw checks with known boundary values. Only this copy
# of the input generator sees the instrumented samplers; production is unchanged.
probe_env <- new.env(parent = environment(generate_input_parameters))
probe_env$rnorm <- function(n, mean = 0, sd = 1) {
  if (mean %in% c(0.779, 0.69, 0.718)) return(c(-0.1, 0.6, 1, 1.2))
  stats::rnorm(n, mean, sd)
}
probe_env$rbeta <- function(n, shape1, shape2) c(0.1, 0.6, 1, 0.8)
probe_generator <- generate_input_parameters
environment(probe_generator) <- probe_env
probe <- probe_generator(4)
for (name in c('AF Well utility', 'Stroke utility', 'MI utility', 'Bleed utility')) {
  check(near(probe[, name], c(-0.025, 0.15, 0.25, 0.25)),
        paste(name, 'caps each draw before quarterly conversion'))
}
check(near(probe[, 'ICH utility'], c(0.025, 0.15, 0.25, 0.2)),
      'ICH utility preserves each beta draw and quarterly conversion')

# Independently calculate each trajectory from the actual transition matrix.
# Each reward is weighted at its original pre-cycle time; the terminal reward
# uses half the final-cycle discount. This preserves the model's convention.
trajectory_totals <- function(x, cycles) {
  transitions <- generate_transition_matrix(x)
  costs <- generate_state_costs(x)
  utilities <- generate_state_utilities(x)
  discounts <- (1 / 1.035) ^ floor(seq_len(cycles) / 4)
  weights <- c(discounts, 0.5 * discounts[cycles])
  weights[1] <- weights[1] * 0.5
  expected_costs <- expected_qalys <- matrix(0, n_samples, n_treatments,
                                            dimnames = list(NULL, treatment_names))
  for (sample in seq_len(n_samples)) {
    for (treatment in seq_len(n_treatments)) {
      trajectory <- matrix(0, cycles + 1, n_states)
      trajectory[1, 1] <- 1
      for (time in seq_len(cycles)) {
        trajectory[time + 1, ] <- trajectory[time, ] %*%
          transitions[sample, treatment, , ]
      }
      expected_costs[sample, treatment] <- sum(weights *
        as.vector(trajectory %*% costs[sample, treatment, ]))
      expected_qalys[sample, treatment] <- sum(weights *
        as.vector(trajectory %*% utilities[sample, treatment, ]))
    }
  }
  list(total_costs = expected_costs, total_qalys = expected_qalys)
}

n_samples <- 8
inputs <- inputs[seq_len(n_samples), ]
equal_inputs <- inputs
equal_inputs[, grepl('Log HR', colnames(equal_inputs))] <- 0
equal_inputs[, paste(treatment_names, 'cost')] <- 100
for (cycles in c(1, 4, 5, 120)) {
  actual <- generate_model_outputs(inputs, initial_age = 70,
                                   final_age = 70 + cycles / 4)
  expected <- trajectory_totals(inputs, cycles)
  for (quantity in names(expected)) {
    check(near(actual[[quantity]], expected[[quantity]]),
          paste(quantity, 'matches the trajectory sum for', cycles, 'cycles'))
  }
  equal_outputs <- generate_model_outputs(equal_inputs, initial_age = 70,
                                          final_age = 70 + cycles / 4)
  for (quantity in names(equal_outputs)) {
    for (arm in 2:n_treatments) {
      check(near(equal_outputs[[quantity]][, 1], equal_outputs[[quantity]][, arm]),
            paste(quantity, 'identical arms 1 and', arm, 'for', cycles, 'cycles'))
    }
  }
}
if (length(failures)) stop(length(failures), ' regression checks failed', call. = FALSE)
cat('All regression checks passed.\n')
