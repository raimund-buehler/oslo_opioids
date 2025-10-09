library(pwr)

# Parameters
sample_size <- 49
total_predictors <- 7 # Main effects + interactions
tested_predictors <- 1 # Testing three-way interaction
df <- sample_size - total_predictors - 1 # Residual degrees of freedom

# Small effect size
small <- pwr.f2.test(u = tested_predictors, v = df, f2 = 0.02, sig.level = 0.05)

# Medium effect size
medium <- pwr.f2.test(u = tested_predictors, v = df, f2 = 0.15, sig.level = 0.05)

# Large effect size
large <- pwr.f2.test(u = tested_predictors, v = df, f2 = 0.35, sig.level = 0.05)

# Display results
small
medium
large
