################################ Introduction #################################
# This code is aimed at looking at biases in database occurrences of Mytilidae
# Have the plot window open so you can see the results!

################################ Getting Started ##############################

# Before we work, lets set your working directory
# Check that all .csv files are within your directory as well

# Clean work environment 
rm(list = ls())


# Set working directory to the folder where all materials are
d <- "~/BRI_Prieto_Example Work/r" 
setwd(d)

# Next, create a folder where all the figures will be saved 
#   to within your directory
if (!dir.exists("figures")) dir.create("figures")

# Install necessary packages before beginning
install.packages("rgbif") # Accesses the GBIF database directly
install.packages("stringdist") # Package that helps filtering data especially strings or words
install.packages("dplyr") # Manipulates data easier
install.packages("chronosphere") # Makes mapping easier
install.packages("sf") # Manipulates data for use in maps
install.packages("terra") # Manipulates spatial features and shape files
install.packages("icosa") # Icosahedral grid that we'll use for sub-sampling
install.packages("ggplot") # Makes snappy figures and can save plots as .png with less lines of code
install.packages("gridExtra") # Grids


################################## Visualizing biases #########################

library(ggplot2)
library(gridExtra)
library(dplyr)

# Recall the newly saved .csv made previously
occ_final <- read.csv("data/Cleaned_Mytilidae_GBIF.csv")

#Make a bar chart of occurrence by genera
# Step 1: Count occurrences per genus
genus_frequency <- occ_final %>%
  count(genus) %>%
  arrange(desc(n))

#To annotate the outlier super abundant genera
top_10_genera <- genus_frequency %>% 
  top_n(10, n)

# Step 2: Plot a bar chart of genus relative frequency (plot on log scale optional)
ggplot(genus_frequency, aes(x = reorder(genus, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip axes for better readability
  #scale_y_log10()+
  labs(title = "Relative Frequency of Genera in Dataset", x = "Genus", y = "Number of Occurrences") +
  theme_minimal()
#Dang! Settle down Mytilus!

# Lets investigate top 10 genera

# Count occurrences per species and get top 10
top_10_genera <- occ_final %>%
  count(genus) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)  # Select the top 10 species

# Step 2: Plot a bar chart of the top 10 species
ggplot(top_10_genera, aes(x = reorder(genus, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip axes for better readability
  # scale_y_log10() +  # Optional log scale
  labs(title = "Top 10 Most Frequent Species in Dataset", x = "Species", y = "Number of Occurrences") +
  theme_minimal()

# THE WORLD IS MYTILUS

#Make a bar chart of occurrence by species
# Step 1: Count occurrences per species
species_frequency <- occ_final %>%
  count(species) %>%
  arrange(desc(n))

#To annotate the outlier super abundant species
top_10_species <- species_frequency %>% 
  top_n(10, n)

# Plot a bar chart of species relative frequency (plot on log scale optional)
ggplot(species_frequency, aes(x = reorder(species, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip axes for better readability
  scale_y_log10()+
  labs(title = "Relative Frequency of species in Dataset", x = "species", y = "Number of Occurrences") +
  theme_minimal()+
  theme(axis.text.y=element_blank())

# Lets investigate top 10 most abundant species

# Count occurrences per species and get top 10
top_10_species <- occ_final %>%
  count(species) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)  # Select the top 10 species

# Step 2: Plot a bar chart of the top 10 species
ggplot(top_10_species, aes(x = reorder(species, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip axes for better readability
  #scale_y_log10() +  # Optional log scale
  labs(title = "Top 10 Most Frequent Species in Dataset", x = "Species", y = "Number of Occurrences") +
  theme_minimal()

# Makes sense seeing this - edulis is probably the most commonly ate mussel
# Mytilus seems to inflate the data as a whole

############################ More Biases #######################################

# But wait... what about the number of individuals per genera/species?

# We have this column
head(occ_final$individualCount)

# So lets run the same series of tests, multiplying each count by genera/species
library(ggplot2)
library(dplyr)
library(gridExtra)

# Load dataset
occ_final <- read.csv("data/Cleaned_Mytilidae_GBIF.csv", stringsAsFactors = FALSE)

# Make sure individualCount is numeric and replace NA with 1
# This assumes that each 'NA' or 'Null' = 1 individual 
occ_final$individualCount <- as.numeric(occ_final$individualCount)
occ_final$individualCount[is.na(occ_final$individualCount)] <- 1

# Save updated file for future reference
# occ_final <- write.csv("data/Cleaned_Mytilidae_GBIF.csv")

# GENUS-level counts (rows and individuals)
genus_frequency <- occ_final %>%
  group_by(genus) %>%
  summarise(
    occurrences = n(),
    total_individuals = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_individuals))

# Top 10 genera by individuals
top_10_genera <- genus_frequency %>%
  slice_max(total_individuals, n = 10)

# SPECIES-level counts (rows and individuals)
species_frequency <- occ_final %>%
  group_by(species) %>%
  summarise(
    occurrences = n(),
    total_individuals = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_individuals))

# Top 10 species by individuals
top_10_species <- species_frequency %>%
  slice_max(total_individuals, n = 10)

# Plots

# Genera (all, weighted by individuals)
p_genus_weighted <- ggplot(genus_frequency, aes(x = reorder(genus, -total_individuals), y = total_individuals)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Genera by Total Individuals", x = "Genus", y = "Total individuals") +
  theme_minimal()

# Genera (occurrence counts for comparison)
p_genus_counts <- ggplot(genus_frequency, aes(x = reorder(genus, -occurrences), y = occurrences)) +
  geom_col(fill = "lightgrey") +
  coord_flip() +
  labs(title = "Genera by Occurrence Count (rows)", x = "Genus", y = "Occurrences") +
  theme_minimal()

# Top 10 genera
p_top10_genera <- ggplot(top_10_genera, aes(x = reorder(genus, total_individuals), y = total_individuals)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Genera by Total Individuals", x = "Genus", y = "Total individuals") +
  theme_minimal()

# Species (all, weighted by individuals)
p_species_weighted <- ggplot(species_frequency, aes(x = reorder(species, -total_individuals), y = total_individuals)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Species by Total Individuals", x = "Species", y = "Total individuals (log10)") +
  theme_minimal() +
  theme(axis.text.y = element_blank())

# Top 10 species
p_top10_species <- ggplot(top_10_species, aes(x = reorder(species, total_individuals), y = total_individuals)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Species by Total Individuals", x = "Species", y = "Total individuals") +
  theme_minimal()

# --- Step 5: View side by side ---
grid.arrange(p_genus_weighted, p_genus_counts, ncol = 2)
grid.arrange(p_top10_genera, p_top10_species, ncol = 2)

# We found our major bias! Mytilus, specifically Mytilus edulis

# Lets move onto mapping our dataset in 2.0!
