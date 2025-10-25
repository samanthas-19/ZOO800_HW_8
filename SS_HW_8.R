##Modified/Created by Samantha Summerfield
##10/25/25

#####---------Objective 1---------#######

##I wanted to use the GBIF code since I couldn't find others in my field

########this is the code you gave us to modify##########

#install.packages("gbifdb")
library(gbifdb)
library(dplyr)    # for using dplyr style

gbif_conn <- gbif_remote()

gbif_conn <- gbif_remote(backend = "duckdb", bucket = "gbif-open-data-us-east-1")

# Suppose we want occurrences of a species in a country after 2000:
tbl <- gbif_conn %>%
  filter(species == "Danaus plexippus",
         countrycode == "US",
         year > 2000) %>%
  select(species, countrycode, decimallatitude, decimallongitude, eventdate) %>%
  arrange(desc(eventdate)) %>%
  head(1000)   # just pull first 1000

df <- collect(tbl)  # bring into R


#####This is what I did#######

##had to use chatgpt to help update my version and fix some bugs
##this code did not work
##there was too much data to load in, it took forever just to load to 5%
##my computer doesn't have enough power for this, it kept terminating R midway through
##so i wanted to show my attempt here

#load librarys
library(gbifdb)
library(dplyr)

# Connect to GBIF’s remote dataset (DuckDB on AWS)
gbif_tbl <- gbif_remote(backend = "duckdb")

# gbif_tbl already points to the occurrence table.
# Build your remote query (filters run on the server)
query_tbl <- gbif_tbl %>%
  filter(
    species == "Danaus plexippus",
    countrycode == "US",
    year >= 2020
  ) %>%
  select(
    species, countrycode, decimallatitude, decimallongitude, eventdate
  )

##now just bring in the ones after 2023 and only a few rows
df <- query_tbl %>%
  filter(year >= 2023) %>%   # narrow range
  collect() %>%
  head(50)

# Preview the results
df




#####----------------Objective 2-----------------#####

#since my data pulling didn't work and I couldn't find other data in my field,
#I decided to bring in the palmer penguins package data to do the visualizations and outliers

# Load libraries
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(knitr)

# 1. Histogram of Body Mass with annotation for extreme values
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Body Mass",
       x = "Body Mass (g)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  # Annotate potential outlier region (>6000 g and <3000g)
  geom_vline(xintercept = 6000, linetype = "dashed", color = "red") +
  annotate("text", x = 6100, y = 30, label = "Extreme outliers", color = "red", angle = 90, vjust = -0.5) +
geom_vline(xintercept = 3000, linetype = "dashed", color = "red") +
  annotate("text", x = 3100, y = 30, label = "Extreme outliers", color = "red", angle = 90, vjust = -0.5)



# 2. Boxplot of Body Mass by Species with outlier labels as red stars
##the red stars labelled the species with the most outliers
ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 3) +
  labs(title = "Boxplot of Body Mass by Species",
       x = "Species",
       y = "Body Mass (g)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_brewer(palette = "Paired") 
 

# 3. Scatterplot of Bill Length vs Bill Depth with species imbalance annotations
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Bill Length vs Bill Depth by Species",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  # Highlight Adelie cluster vs Gentoo cluster
  annotate("text", x = 38, y = 22, label = "Adelie penguins: shorter, deeper bills", color = "red") +
  annotate("text", x = 54, y = 13, label = "Gentoo penguins: longer, shallower bills", color = "blue")

# 4. Table of Largest Outliers in Body Mass (top 3 per species)
###I just learned how to do this and thought it was cool you could identify the outliers
outliers <- penguins %>%
  filter(!is.na(body_mass_g)) %>%
  group_by(species) %>%
  top_n(3, body_mass_g) %>%   # top 3 largest per species
  arrange(species, desc(body_mass_g)) %>%
  select(species, island, body_mass_g)

# Display the table with a note
kable(outliers, caption = "Top 3 Largest Body Mass Values by Species")

