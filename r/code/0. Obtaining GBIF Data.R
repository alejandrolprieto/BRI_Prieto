
############################### Introduction ##################################

#  This code is for specifically downloading and cleaning GBIF data
#   So it can be used for various analyses and eventually mapping
#   In this example, I use "Mytilidae" (Family within Bivalvia)

# If you are unfamiliar with GBIF, please take a look here:
# https://www.gbif.org then type in any taxa you desire to show occurrences

################# Thinking about ways to get GBIF Data ########################

# How you obtain your data is dependent on how many occurrences your desired
# taxa has on GBIF database. There are essentially 2 ways:

#     1) Using R to get the data for you (Recommended for smaller datasets)
#     2) Manual download from the GBIF website (Recommended for larger datasets)

# What makes a dataset large or small? The time it takes to download is always
# a good proxy.

############## Using R Code to obtain GBIF Occurrence Data #####################

# An easy way to test if you're dealing with a large or small dataset is to 
#   simply check GBIF directly for your desired Taxa. If the number of 
#   occurrences surpass 200,000, then you'll want to skip to the next section.

# Another thing to consider is download speed. If you're dealing with a small
#   dataset and you need to do quick analysis, then this section will be useful.


# In any case, you might want to test out a GBIF download with R code first. 
#   The time it takes to download will give you a better idea on what 
#   you're dealing with.

# GENERAL RULE-OF-THUMB: You almost always want to work offline
# This means downloading your data and storing it so you can recall at any time

# In this section, I'll walk you through downloading your data with R via
#   a GBIF API request. If your occurrence data surpasses 200,000 - 
#   then this code will NOT obtain all data for your desired taxon.

# Always establish a working directory
d <- "~/BRI_Prieto_Example Work/r" # Change to your folder path
setwd(d)

# Begin by installing this package
install.packages("rgbif") # This package obtains data from GBIF via API requests
library(rgbif)

# For this example, I'm using Mytilidae (family of Mussels in Bivalvia)
# Input taxonomic name and rank
taxonomic_name <- "Mytilidae" # Change value accordingly
taxonomic_rank <- "FAMILY" # Change value accordingly

# Get the taxon key for the specified taxon
taxon_key <- rgbif::name_backbone(name = taxonomic_name, 
                                  rank = taxonomic_rank)$usageKey

taxon_key

# Here, you'll notice a number: 3476L (taxon_key)
# This is the code of the taxa that GBIF uses to identify the taxa 

# Search for occurrence data
occs <- rgbif::occ_search(
  taxonKey = taxon_key,
  hasCoordinate = TRUE,        # Only records with coordinates
  hasGeospatialIssue = FALSE,  # Exclude geospatial issues
  limit = 200                  # Number of occurrences I want
)

# Notice I put '200' for the number of occurrences
# You'll want to look at your dataset to make sure you want all the occurrences
# The maximum number of occurrences you can grab via API is 200,000
# Notice the amount of time it takes for R to download the data
# Most likely, there are more than 200k occurrences for this group 
#   so I'm not even capturing the entire dataset with this

# View the first few records
head(occs$data)

# Save the results to a CSV file if you want to use it later
write.csv(occs$data, 
          "data/GBIF_Mytilidae_Occurrence_Example_Data.csv", row.names = FALSE)

####################### Using GBIF to Download Occurrence Data ################

# What if your dataset surpasses 200,000 occurrences?
# This means that you'll need to manually download your data directly from GBIF.

# The instructions below are if you want to download species data of your choice 
#   from GBIF.

# For the sake of this tutorial, you can follow along with the data I have 
#   already. Please skip steps 1-9 and use my example data.

################ Downloading your own GBIF Data ###############################

# This section is mostly for reading - Github doesn't allow large files to be
#   imported. I would usually have you work through importing the data from 
#   GBIF itself but - alas...

# To use the given data in the folder, proceed to the next section!

# Instructions on how to download your own data from GBIF

# 1) Go to the GBIF website: https://www.gbif.org 
# 2) Sign in or make an account (if you haven't already)
# 3) Enter your desired taxa in the search box 
# 4) Click on the taxa in search results
# 5) Click "Occurrences" on the heading of the taxa page
# 6) Click "Download" tab
# 7) Click on your desired download option ('Darwin Core Active' 
#     is usually sufficient)
# 8) An email will arrive with your download as a zipped file
# 9) The zipped file should have a .txt file called "occurrence.txt". Copy 
#       that file into your working directory.      

# 10) Convert the .txt filt into a useable dataframe


# Packages
install.packages("dplyr")
library(dplyr)

# If you manage to open the original .txt file, you'll notice it's
#   massive and not organized into columns

# This next line of code will take the .txt file and convert it into a usable 
#   dataframe that we can mannipulate and save as a .csv
#gbif_sample <- read.delim("data/occurrence.txt", stringsAsFactors = FALSE, quote = "")

# Might take a little bit to load (this dataset has over 500k occurrences!)

# Take a quick look
#glimpse(gbif_sample)

# The data is now organized into columns.

# Convert this to an .RDS file for faster analysis processing
#saveRDS(gbif_sample, "data/gbif_sample.rds")

# I always try to save the original sample in .rds since its compressed
#   and I can recall it whenever I need

# Now recall it and give it an easy name so we can play with it
#occs <- readRDS("data/gbif_sample.rds")

# We're ready to move on to the next steps of cleaning GBIF data

############################# Cleaning GBIF Data ############################

# Spoiler Alert: Raw GBIF data is heavily flawed
# In order to actually do analyses that are publication-ready, cleaning your
#   data is necessary.

# In GBIF, it says Mytilidae as 1,200-ish species.... Which is not accurate
# Presently, there are roughly 400 living Mytilidae species
# I've created a a more up-to-date species list, so the following steps will be
#   showing how I would clean GBIF data using this list

# Load necessary packages
install.packages("stringdist")
install.packages("chronosphere") # Makes mapping easier
install.packages("sf") # Manipulates data for use in maps

library(rgbif)
library(dplyr)
library(stringdist)

# Clean work environment 
rm(list = ls())

# Set your Working directory
d <- "~/BRI_Prieto_Example Work/r" # Change to your folder path
setwd(d)

# Now, lets go in and clean the occurrence data since there certain errors 
#   we don't want to include in our analysis

# Load occurrence data
occs <- readRDS("data/gbif_sample.rds")

# Load list of accepted species in Mytilidae (from previous section)
species <- read.csv("data/Mytilidae_Species_List_Cleaned.csv")

# Make a list of accepted species names
allaccepted_species <- species$x

# Filter to remove entries without coordinates,fossils, and non-accepted species
occ_cleaned <- occs %>%
  filter(hasCoordinate == "true") %>%  # Keep only rows where 'hasCoordinate' is TRUE
  filter(!grepl("FOSSIL_SPECIMEN", basisOfRecord, ignore.case = TRUE)) %>%  # Remove fossils if labeled
  filter(species %in% allaccepted_species)  # Keep only species in the accepted list

# That seemed to have gotten rid of over 150K occurrences

# Create a simplified data frame with species, genera, coordinates, etc.
occ_final <- occ_cleaned %>%
  select(species, genus, individualCount, year, habitat, 
         depth, decimalLatitude, decimalLongitude)

# Convert to sf points object
occ_sf <- sf::st_as_sf(occ_final, 
                       coords = c("decimalLongitude", "decimalLatitude"), 
                       crs = 4326)

# Check if there are any missing coordinates in the filtered dataset
any(is.na(occ_cleaned$decimalLatitude) | 
      is.na(occ_cleaned$decimalLongitude))  
# This should return FALSE if successful

# View the first few rows of the final data frame to confirm
head(occ_final)

# How many species are left?
unique(occ_final$species)

# Only 359 left species left, which is probably good enough for this example

# Finally, lets take a closer look at the 'individualCount' column
unique(occ_final$individualCount)

# oh no... I see zero's.... which doesn't really make sense since this is 
#   logged as an occurrence so it should be at least 1

# Now we're faced with the age-old dilemma - do we replace all the zero's with 
#   one's? Or do we eliminate them altogether since they might be invalid.

# Well, before we get too hasty, lets see how many zeros and NA's there are 
# Count how many occurrences have 0 in individualCount
# Count how many occurrences have "NA" in individualCount 

# First, Total rows
total_rows <- nrow(occ_final)

# Count zeros
zero_count <- sum(occ_final$individualCount == 0, na.rm = TRUE)

# Count NAs
na_count <- sum(is.na(occ_final$individualCount))

# Summarize
cat("Total rows:", total_rows, "\n")
cat("Zero values:", zero_count, "(", round(zero_count/total_rows*100, 4), "%)\n")
cat("NA values:", na_count, "(", round(na_count/total_rows*100, 4), "%)\n")

# OK... seems like removing zeros is viable since its such a small percent
# The opposite can be said for the "NA" values
# Following just the basic logic of databases, I'll assume all NA values are
#   just 1 individual that most likely had an inputter that was lazy

# Replace NA values in individualCount with 1
occ_final$individualCount[is.na(occ_final$individualCount)] <- 1

# Remove occurrences with 0 individualCount
occ_clean <- occ_final[occ_final$individualCount != 0, ]

# Check how many were removed
removed <- nrow(occ_final) - nrow(occ_clean)
cat("Removed", removed, "rows with individualCount = 0\n")

# Verify no zeros remain
sum(occ_clean$individualCount == 0, na.rm = TRUE)

# Perfect!

# Save the final cleaned matrix to a new CSV file
write.csv(occ_clean, "data/Cleaned_Mytilidae_GBIF.csv", row.names = FALSE)

# You're ready for analysis! Proceed to script 1.0!
