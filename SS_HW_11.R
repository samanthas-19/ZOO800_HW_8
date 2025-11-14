##Created by Samantha Summerfield
##11/14/25
##HW 11 for ZOO800
##modified from HW 10


#------------------ Objective 1: ANCOVA Simulation: DOM Scenario ------------------#

#Variables (names & units):
###Response: A254 — UV absorbance at 254 nm (absorbance units, AU)
###Continuous predictor: DOM_conc — dissolved organic matter concentration (mg C/ L)
###Two-level factor: Watershed — agricultural vs urban (categorical)

set.seed(123)

# Sample size
n <- 100
n_group <- n/2

# Continuous predictor: DOM concentration (mg C/L) this is X!
DOM_conc <- runif(n, 0, 10)

# Categorical predictor: watershed type, these are the two categorical variables
Watershed <- factor(rep(c("agricultural", "urban"), each = n_group))

# ANCOVA structure here:

interaction <- TRUE   # TRUE = different slopes; FALSE = parallel slopes

# "True" intercepts and slopes for A254 ~ DOM_conc
# Agricultural watershed
beta0_ag <- 0.5
beta1_ag <- 0.12

# Urban watershed
beta0_urb <- 0.8
beta1_urb <- ifelse(interaction, 0.20, 0.12)

# Lognormal errors (centered)
err <- rlnorm(n, meanlog = 0, sdlog = 0.6)
err <- err - mean(err)

# Generate response variable: A254 absorbance (AU), absorbance is the Y!
A254 <- ifelse(Watershed == "agricultural",
               beta0_ag + beta1_ag * DOM_conc + err,
               beta0_urb + beta1_urb * DOM_conc + err)

# Combine into data frame
df <- data.frame(DOM_conc, A254, Watershed)

# Fit ANCOVA model
model <- lm(A254 ~ DOM_conc * Watershed, data = df)
summary(model)

# ------------------ Plot ------------------ #
library(ggplot2)

ggplot(df, aes(x = DOM_conc, y = A254, color = Watershed)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    title = ifelse(interaction,
                   "ANCOVA: A254 vs. DOM Concentration Across Watershed Types",
                   "ANCOVA with Parallel Slopes"),
    x = "DOM concentration (mg C/L)",
    y = "UV absorbance at 254 nm (AU)"
  ) +
  theme_minimal()


#save df as a .csv to send to partner
write.csv(df, "DOM_conc_abs_data.csv")

#####my scenario I made for this data, send to partner
#Water entering a drinking water treatment plant was sampled from tributaries surrounded by agricultural 
#and urban watersheds. DOM concentration(mg/L C) and the UV absorbance at 254 nm (A254) was measured to estimate
#DOM aromaticity. This looks to determine whether land-use influences DOM aromaticity (how 
#absorbance scales with concentration before treatment and if these differences will influence water quality). 

#Ecological question:
# Does DOM absorbance increase with DOM concentration in the same way for agricultural and urban 
# tributaries supplying the drinking water treatment plant?




#------------------ Objective 2: Partner(Maggie Phillips) Data Exploration ------------------#




