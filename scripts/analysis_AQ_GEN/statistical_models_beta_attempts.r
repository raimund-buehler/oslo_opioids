library(dplyr)
library(glmmTMB)
library(emmeans)
library(readr)
library(here)

df <- read_csv(here("data", "analyses", "fix_perc_ASQ_GEN.csv"))

#Working directory
#setwd("C:/Users/priva/Documents/GitHub/oslo_opioids/data/analyses")

#df <- read_csv("fix_perc_ASQ_GEN.csv")


#Only additional sample and eye AOI
#Wurde auch ohne ID/Sample-Filter probiert, ändert nichts


df_eye <- df %>%
  filter(ID >= 200, AOI == "eye_brow")

df_eye$Drug <- factor(df_eye$Drug,
  levels = c("placebo", "morphine", "naltrexone")
)

df_eye$FixProp <- df_eye$FixTimePerc / 100

#Clamp values to avoid 0 and 1 for beta regression (So:

#0 becomes 0.000001, 1 becomes 0.999999, everything else stays almost the same

eps <- 1e-6
df_eye$FixProp_beta <- pmin(pmax(df_eye$FixProp, eps), 1 - eps)



# ===============================
# Simple Beta-Model
# ===============================

model_beta_simple <- glmmTMB(
  FixProp_beta ~ ASQ + (1 | ID),
  family = beta_family(link = "logit"),
  data = df_eye
)

print(summary(model_beta_simple))



# Summary of workaround attempts within glmmTMB for beta regression using GPT-5.1

#| Attempt                        | Purpose                      | glmmTMB Support | Result           |
#| ------------------------------ | ---------------------------- | --------------- | ---------------- |
#| Basic beta regression          | preregistered model          | yes             | fails (NaN)      |
#| Convert to proportion          | required for beta            | yes             | DV still invalid |
#| Clamp values                   | avoid 0/1                    | yes             | fails            |
#| Remove 0/100                   | try open interval            | yes             | fails            |
#| Zero-inflation                 | handle zeros                 | yes             | fails            |
#| Probit link                    | alternative likelihood shape | yes             | fails            |
#| BFGS optimizer                 | stabilize                    | yes             | fails            |
#| Zero-one inflation + θ mapping | stabilize                    | yes             | fails            |

