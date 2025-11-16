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



# my interpretation
# DOM concentration positively affects A254 in all watersheds.
# In urban watersheds, DOM has a stronger effect on A254 compared to reference watersheds.
# The baseline A254 in urban watersheds isn’t significantly different from reference watersheds when DOM = 0.
# DOM is the main driver of A254, and its effect is amplified in urban watersheds.

#####my scenario I made for this data, send to partner
#Water entering a drinking water treatment plant was sampled from tributaries surrounded by agricultural 
#and urban watersheds. DOM concentration(mg/L C) and the UV absorbance at 254 nm (A254) was measured to estimate
#DOM aromaticity. This looks to determine whether land-use influences DOM aromaticity (how 
#absorbance scales with concentration before treatment and if these differences will influence water quality). 

#Ecological question:
# Does DOM absorbance increase with DOM concentration in the same way for agricultural and urban 
# tributaries supplying the drinking water treatment plant?




#------------------ Objective 2: Partner(Maggie Phillips) Data Exploration ------------------#

# Ecological scenario
# This dataset contains simulated DOC (x) and NO3 (y) concentrations for a 
# gravel-bed and sand-bed river.
# “How well do DOC concentrations predict NO3 concentrations in rivers with 
# different bed materials?”
# DOC availability can influence denitrification, a process that removes NO3 
# from stream water, but NO3 removal is also affected by physical properties 
# of the riverbed. Gravel and sand differ in how long NO3-rich water remains 
# in zones where removal can occur, so the DOC–NO3 relationship may differ 
# between bed materials.

# Load packages
library(tidyverse)

# Read partner’s data
df <- read_csv("data_hw11.csv")

# Convert riverbed to factor (required for ANCOVA)
df$riverbed <- factor(df$riverbed)

# Full model (with interaction)
# NO3 ~ DOC * riverbed

full_mod <- lm(NO3 ~ DOC * riverbed, data = df)
summary(full_mod)


# Model selection: Remove non-significant terms based on p-values.
# Start with the interaction. If significant, keep it.

# Check interaction term, shows that these are all significant (three stars)
anova(full_mod)

# If interaction is NOT significant, we would remove it:
# reduced_mod1 <- lm(NO3 ~ DOC + riverbed, data = df)
# summary(reduced_mod1)

# BUT the interaction IS highly significant, so the model reduction stops here.
# The final model is the full model.

final_mod <- full_mod

# Final model summary

summary(final_mod)


# Plot diagnostics, not sure if this was needed but I wanted to practice it

par(mfrow=c(2,2))  
plot(final_mod)

par(mfrow=c(1,1))

# Visual the model like I did for my data

ggplot(df, aes(x = DOC, y = NO3, color = riverbed)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(title = "NO3 vs DOC by Riverbed Material",
       x = "DOC concentration (mg/L)",
       y = "NO3 concentration (mg/L)")

#####My interpretation of results
# The interaction between nutrient concentrations and riverbed material was 
# statistically significant, showing that the relationship between DOC and NO3 
# depends on whether the streambed is composed of sand or gravel. DOC predicts NO3 
# differently across bed types, suggesting that properties of the substrate 
# alter how efficiently NO3 leaves the substrate (denitrifies). 
# The effect of DOC on NO3 concentrations is different between 
# riverbed materials; answering the question that riverbed type modifies the DOC–NO3 relationship.


## Questions for part 4
#Check back with the partner who generated the data that you analyzed. 
#How does your answer compare to the truth? 

#How close are your parameter estimates to the true values?

####Answer: the graphs compared with my partner looks the same. I also found that the slope 
### for sand is 0.199 which is what Maggie had inputted and for gravel it was .195. maggie didn't
### originally input this, but when she went back to do the statistical test, it came back as 1.95
### due to the added error which changed it from her original 0.8


