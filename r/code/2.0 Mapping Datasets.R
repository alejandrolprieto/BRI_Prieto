################################# Simple Mapping of GBIF Data ###################

library(sf)
library(chronosphere)
library(dplyr)

# Clean work environment 
rm(list = ls())

# Set working directory to the folder where all materials are
d <- "~/BRI_Prieto/r" 
setwd(d)

# Recall last csv. made in the previous section
occ_final <- read.csv("data/Cleaned_Mytilidae_GBIF.csv")

# Convert occurrence data to sf points object
occ_sf <- st_as_sf(occ_final, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Fetch and prepare land shapefile
ne <- chronosphere::fetch("NaturalEarth", ser="land", ver="4.1.0")

# Perform spatial join to remove points on land (from preserved specimens)
land <- st_as_sf(ne) # Ensures land shapefile is the same CRS as the occurrence data
occ_cleaned <- occ_sf[!st_intersects(occ_sf, land, sparse = FALSE), ]

# Convert back into sf points with new cleaned points for ease of use
occ_sf <- st_as_sf(occ_cleaned, coords = c("decimalLongtiude", "decimalLatitude", crs = 4326))

genera_num <- unique(occ_sf$genus)
species_num <- unique(occ_sf$species)

# Creating a vector for just sf data points
mussels <- st_coordinates(occ_sf)

# Plot with adjusted sizing (might take awhile)
# Be patient ;)
# Also save the plot as a PNG
png(filename = "figures/ALLMytilidae_Occurrences_Map.png", width = 800, height = 600)
plot(st_geometry(land), main = "All Mytilidae Occurrences (350 Species)", col = "gray", border = NA, xlim = c(-180, 180), ylim = c(-90, 90))
points(mussels, col = "red", pch = 20, cex = 0.5)
dev.off()

############################# Mapping with Additional Data Visualization ################################################

# This is a continuation of the mapping but using subsampling methods
# This will help us look at hotspots of occurrences and this can also be 
#   applied to species/genera

# First lets make a Gaussian grid
library(terra)
library(sf)
library(icosa)

# a 15- degree resolution grid
# You can play around with the resolution to get alternate values per tile
r <- terra::rast(res=5)

# count the number of points in raster cells
counts <- terra::rasterize(mussels,r, fun=length)
counts

# Save the plot
png(filename = "figures/ALLMytilidae_Occurrence_Tiles_Map.png", width = 800, height = 600)
plot(counts, main = "All Mytilidae Occurrence Tiles")
plot(ne$geometry, col=NA, border="black", add=TRUE)
dev.off()

### Create a hexagrid for Subsampling
hexa <- hexagrid(deg=5, sf=TRUE)

# Save the plot
png(filename = "figures/ALLICOSA_Mytilidae_Occurrence_Point_Cloud_Map.png", width = 800, height = 600)
plot(ne$geometry, main = "All Mytilidae Occurrence Point Cloud", col="gray", border=NA)
plot(hexa, add=TRUE)
points(mussels, col="#FF000022", pch=3)
dev.off()

### Spatial Binning
plot(hexa)
gridlabs(hexa, cex=0.5)

# visualize exact locations (might take a while to load)

# The locate() function returns the name of the cell under a point: 
#   one cell name for every coordinate pairs. 
# The function takes the grid (hexa), and the point coordinates (examples) 
#   as arguments and returns a vector of cell names
musselsCells <- locate(hexa, mussels)
musselsCells

# replot the map for clarity
plot(hexa)

# labels
gridlabs(hexa, cex=0.5)

# points again
points(mussels, col="#FF0000", pch=3, cex=1) # might take awhile to load

# This localization of the points does a lot of heavy lifting in icosa, and it can be applied thousands/millions of points at once.
cells <- locate(hexa, mussels)
str(cells)

# transform this to a data frame
rdf <- data.frame(mussels)
# assign the face names as a column
rdf$cells <- cells
# the first 6 rows
head(rdf)

### Counting the cells
tCells <- table(cells)
str(tCells)

# Plot on map
plot(hexa, tCells)

# Fix symbology and save figure 
png(filename = "figures/ALLICOSA_Mytilidae_Spatially_Binned_Global_OccMap.png", width = 800, height = 600)
plot(hexa, tCells, 
     main = "All Mytilidae Spatially Binned Global Occurrences",
     border="black",
     pal=c("#440154FF", "#2D708EFF", "#29AF7FFF", "#FDE725FF", "#FF3300"), 
     breaks=c(0, 500, 20000, 40000, 60000, 80000),
     reset= FALSE)
plot(ne$geometry, add= TRUE, col="#66666688") # Land mass
dev.off()

# Changing projection to Mollewide
neMoll <- sf::st_transform(ne, "ESRI:54009")

# Plot and save into folder
png(filename = "figures/ALLICOSA_Mollweide_Mytilidae_Spatially_Binned_Global_OccMap.png", width = 800, height = 600)
plot(hexa, tCells,
     crs="ESRI:54009",
     main = "All Mytilidae Spatially Binned Global Occurrences (Mollweide)", 
     border="black",
     pal=c("#440154FF", "#2D708EFF", "#29AF7FFF", "#FDE725FF", "#FF3300" ), 
     breaks=c(0, 500, 20000, 40000, 60000, 80000), 
     reset=FALSE)
plot(neMoll$geometry, add=TRUE, col="#66666688") # the landmasses transparent gray with black contour
dev.off()

### Same thing as above but with log transformation to make it more readable

### Counting the cells
tCells <- table(cells)
log_tCells <- log1p(tCells)  # Log-transform the counts (log1p handles zeros)

# Plot log-transformed values on the map
plot(hexa, log_tCells)

# Fix symbology and save figure with log-transformed values
png(filename = "figures/ALLICOSA_Mytilidae_Spatially_Binned_Global_OccMap_Log.png", width = 800, height = 600)
plot(hexa, log_tCells, 
     main = "All Mytilidae Spatially Binned Global Occurrences (Log-Transformed)",
     border = "black",
     pal = c("#440154FF", "#2D708EFF", "#29AF7FFF", "#FDE725FF", "#FF3300"), 
     breaks = c(0, 2, 3, 6, 9, 12),
     reset = FALSE)
plot(ne$geometry, add = TRUE, col = "#66666688") # Land mass
dev.off()

# Changing projection to Mollweide
neMoll <- sf::st_transform(ne, "ESRI:54009")

# Plot and save Mollweide figure with log-transformed values
png(filename = "figures/ALLICOSA_Mollweide_Mytilidae_Spatially_Binned_Global_OccMap_Log.png", width = 800, height = 600)
plot(hexa, log_tCells,
     crs = "ESRI:54009",
     main = "All Mytilidae Spatially Binned Global Occurrences (Mollweide, Log-Transformed)", 
     border = "black",
     pal = c("#440154FF", "#2D708EFF", "#29AF7FFF", "#FDE725FF", "#FF3300"), 
     breaks = c(0, 2, 3, 6, 9, 12),
     reset = FALSE)
plot(neMoll$geometry, add = TRUE, col = "#66666688") # Land masses as transparent gray with black contour
dev.off()

################################## Calculations with the Map ##########################################

# We can run some quick analyses with the map data to look at patterns of occurrences
# Lets start by getting a frequency of occurrences based on latitudes

library(dplyr)

# Convert centers to data frame if needed and add count values from tCells
faceInfo <- as.data.frame(centers(hexa))
faceInfo$count <- tCells[rownames(faceInfo)]

# Remove rows with NA values in 'count' or 'lat' columns
faceInfo_cleaned <- faceInfo %>% 
  filter(!is.na(lat) & !is.na(count))

# Log-transform the counts
faceInfo_cleaned$log_count <- log10(faceInfo_cleaned$count + 1)  # Adding 1 to avoid log(0)

# Add a trend line (loess smoothing for a general trend)
loess_model <- loess(log_count ~ lat, data = faceInfo_cleaned)
lat_seq <- seq(min(faceInfo_cleaned$lat), max(faceInfo_cleaned$lat), length.out = 100)
loess_pred <- predict(loess_model, newdata = data.frame(lat = lat_seq))

# Plot and save into folder
png(filename = "figures/ALLMytilidae_Latitudinal_Occurrence_Patterns_Log.png", width = 800, height = 600)
# Create the base plot
plot(faceInfo_cleaned$lat, faceInfo_cleaned$log_count, 
     xlab = "Latitude (deg)", 
     ylab = "Log(Count of Occurences + 1)", 
     main = "All Mytilidae Latitudinal Occurrence Patterns (Log Transformed)", 
     pch = 16, col = "#99000044", 
     axes = FALSE) # Suppress default axes
# Add custom axes
axis(1) # Default X-axis
axis(2, las = 1) # Y-axis with labels rotated for readability
# Add the LOESS curve
lines(lat_seq, loess_pred, col = "blue", lwd = 2)
# Add a box around the plot
box()
dev.off()

# Lets make a plot that turns the latitudes into absolute values
# Then runs an r-squared test based on frequency of species

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Ensure decimalLatitude is numeric, and remove rows where it's NA
occ_final <- occ_final %>%
  filter(!is.na(decimalLatitude)) %>%
  mutate(decimalLatitude = as.numeric(decimalLatitude))  # Convert to numeric

# Convert latitudes to absolute values, group by unique species per latitude bin
species_counts <- occ_final %>%
  mutate(abs_latitude = abs(round(decimalLatitude, 0))) %>%  # Use absolute latitudes
  group_by(abs_latitude, species) %>%  # Group by absolute latitude and species
  summarize() %>%  # Keeps unique species per latitude bin
  group_by(abs_latitude) %>%
  summarize(species_count = n())  # Count unique species per latitude bin

# Apply log transformation to species counts
species_counts <- species_counts %>%
  mutate(log_species_count = log1p(species_count))  # log1p(x) = log(1 + x), handles zero values

# Perform linear regression on the log-transformed species counts
model <- lm(log_species_count ~ abs_latitude, data = species_counts)
r_squared <- summary(model)$r.squared

# Plot the data with points and add a trendline
ggplot(species_counts, aes(x = abs_latitude, y = log_species_count)) +
  geom_point(shape = 18, size = 3) +  # Diamond-shaped points
  geom_smooth(method = "lm", color = "blue", linetype = "dashed", se = FALSE) +  # Add trendline
  labs(x = "Absolute Latitude", y = "Log-transformed Number of Species",
       title = paste("All Mytilid Species Count by Latitude (R² =", round(r_squared, 3), ")")) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Save the ggplot as a PNG
ggsave("figures/ALLMytilid_Species_Latitude_RSquared.png", width = 8, height = 6)

############################## Data manipulation and Plots ########################################

# Now we make plots on occurrences, species and genera by latitude
library(dplyr)
library(ggplot2)

# Ensure decimalLatitude is numeric, and remove rows where it's NA
occ_final <- occ_final %>%
  filter(!is.na(decimalLatitude)) %>%
  mutate(decimalLatitude = as.numeric(decimalLatitude))  # Convert to numeric

# Aggregate occurrences by latitude
occurrence_counts <- occ_final %>%
  mutate(lat_bin = round(decimalLatitude, 0)) %>%  # Group by integer latitudes
  group_by(lat_bin) %>%
  summarize(occurrences = n())

# Apply log transformation to occurrences
occurrence_counts <- occurrence_counts %>%
  mutate(log_occurrences = log1p(occurrences))  # log1p(x) = log(1 + x), handles zero values

# Plot the data
ggplot(occurrence_counts, aes(x = lat_bin, y = log_occurrences)) +
  geom_point(shape = 18, size = 3) +  # Diamond-shaped points
  geom_line() +
  scale_x_continuous(breaks = seq(-90, 90, by = 30), limits = c(-90, 90)) +
  labs(x = "Latitude", y = "Log-transformed Number of Occurrences", title = "All Mytilid Occurences by Latitude") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = 0, linetype = "solid")  # Vertical line at the equator

# Save the ggplot as a PNG
ggsave("figures/ALLMytilid_Occurrences_by_Latitude.png", width = 8, height = 6)

#### Same plot but with Species

# Ensure decimalLatitude is numeric, and remove rows where it's NA
occ_final <- occ_final %>%
  filter(!is.na(decimalLatitude)) %>%
  mutate(decimalLatitude = as.numeric(decimalLatitude))  # Convert to numeric

# Aggregate by latitude and count unique species
species_counts <- occ_final %>%
  mutate(lat_bin = round(decimalLatitude, 0)) %>%  # Group by integer latitudes
  group_by(lat_bin) %>%
  summarize(unique_species = n_distinct(species))  # Count unique species per lat_bin

# Apply log transformation to species counts
species_counts <- species_counts %>%
  mutate(log_species = log1p(unique_species))  # log1p handles zero values

# Plot the data
ggplot(species_counts, aes(x = lat_bin, y = log_species)) +
  geom_point(shape = 18, size = 3) +  # Diamond-shaped points
  geom_line() +
  scale_x_continuous(breaks = seq(-90, 90, by = 30), limits = c(-90, 90)) +
  labs(x = "Latitude", y = "Log-transformed Number of Unique Species", title = "All Unique Mytilid Species by Latitude") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = 0, linetype = "solid")  # Vertical line at the equator

# Save the ggplot as a PNG
ggsave("figures/ALLMytilid_Species_by_Latitude.png", width = 8, height = 6)


###### Same plot but with Genera
# Ensure decimalLatitude is numeric and remove rows where it's NA
occ_final <- occ_final %>%
  filter(!is.na(decimalLatitude)) %>%
  mutate(decimalLatitude = as.numeric(decimalLatitude))  # Convert to numeric if necessary

# Aggregate by latitude and count unique genera
genus_counts <- occ_final %>%
  mutate(lat_bin = round(decimalLatitude, 0)) %>%  # Group by integer latitudes
  group_by(lat_bin) %>%
  summarize(unique_genera = n_distinct(genus))  # Count unique genera per lat_bin

# Apply log transformation to genus counts
genus_counts <- genus_counts %>%
  mutate(log_genera = log1p(unique_genera))  # log1p handles zero values

# Plot the data
ggplot(genus_counts, aes(x = lat_bin, y = log_genera)) +
  geom_point(shape = 18, size = 3) +  # Diamond-shaped points
  geom_line() +
  scale_x_continuous(breaks = seq(-90, 90, by = 30), limits = c(-90, 90)) +
  labs(x = "Latitude", y = "Log-transformed Number of Unique Genera", title = "All Unique Mytilid Genera by Latitude") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = 0, linetype = "solid")  # Vertical line at the equator

ggsave("figures/ALLMytilid_Genera_by_Latitude.png", width = 8, height = 6)

##### Plot all the data together on the same plot
# Ensure decimalLatitude is numeric and remove rows where it's NA
occ_final <- occ_final %>%
  filter(!is.na(decimalLatitude)) %>%
  mutate(decimalLatitude = as.numeric(decimalLatitude))  # Convert to numeric if necessary

# Aggregate occurrences by latitude
occurrence_counts <- occ_final %>%
  mutate(lat_bin = round(decimalLatitude, 0)) %>%
  group_by(lat_bin) %>%
  summarize(counts = n())

# Aggregate by unique species
species_counts <- occ_final %>%
  mutate(lat_bin = round(decimalLatitude, 0)) %>%
  group_by(lat_bin) %>%
  summarize(counts = n_distinct(species))

# Aggregate by unique genera
genus_counts <- occ_final %>%
  mutate(lat_bin = round(decimalLatitude, 0)) %>%
  group_by(lat_bin) %>%
  summarize(counts = n_distinct(genus))

# Add a 'metric' column to each for labeling
occurrence_counts$metric <- "Occurrences"
species_counts$metric <- "Species"
genus_counts$metric <- "Genus"

# Combine into a single data frame
combined_counts <- bind_rows(occurrence_counts, species_counts, genus_counts)

# Log-transform the counts, handling zero values with log1p
combined_counts <- combined_counts %>%
  mutate(log_counts = log1p(counts))

# Plot
ggplot(combined_counts, aes(x = lat_bin, y = log_counts, color = metric)) +
  geom_line(linewidth = 1) +  # Use linewidth instead of size for lines
  geom_point(size = 2) +
  scale_color_manual(values = c("Occurrences" = "red", "Species" = "blue", "Genus" = "green")) +
  scale_x_continuous(breaks = seq(-90, 90, by = 30), limits = c(-90, 90)) +
  labs(
    x = "Latitude",
    y = "Log-transformed Counts",
    title = "All Mytilidae Occurrences, Species, and Genera Counts by Latitude",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = 0, linetype = "solid")  # Vertical line at the equator

ggsave("figures/ALLMytilid_OccsSpeciesGenera_by_Latitude.png", width = 8, height = 6)

# Done!
