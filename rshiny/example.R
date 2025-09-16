################################### Example BRI ##############################

#   0. R packages to download :) 
# 
# ******************************************************

## Install and load the following packages:

install.packages(c("tidyverse", "geoscale", 
                   "viridis", "vegan", "deeptime", "devtools", 
                   "sepkoski", "rgplates", "divDyn", "dplyr",
                   "ggplot2", "mgcv"))


library(tidyverse) # for data organisation, manipulation, and visualisation
library(sepkoski) # bespoke package to access Sepkoski's compendia
library(geoscale) # for plotting with the geological time scale on the x-axis (uses base R syntax)
library(viridis) # for colour scales
library(vegan) # for diversity metrics
library(deeptime) # for plotting geological time scale
library(rgplates) # palaeogeographic reconstructions
require(devtools) # developer tools

# ______________________________________________________
#
#   1. Accessing databases in R 
#       & cleaning imported data
# 
# ******************************************************


# 0. Packages used in this script -----------------------------------------

library(tidyverse) # for data organisation, manipulation, and visualisation
library(divDyn) # bespoke package diversity dynamics 
library(sepkoski) # bespoke package to access Sepkoski's compendia


## Clear R's environment before starting so you're working with a clean slate:
rm(list = ls())

## If you've been using a lot of different packages, some function names might be masked;
## this step ensures that the function 'select' is coming from the dplyr package (part of tidyverse)
select <- dplyr::select

# 1. Fetching data from packages -------------------------------------------


### (a) sepkoski

## This package allows easy access to Sepkoski's fossil marine animal genera 
## compendium (Sepkoski, 2002), ported from Shanan Peters' online database.
## More information here: https://github.com/LewisAJones/sepkoski

## Accessing the datasets
data("sepkoski_raw") # Sepkoski's raw fossil marine animal genera compendium (Sepkoski, 2002)
data("sepkoski") # Sepkoski's compendium with first and last appearance intervals updated to be consistent with stages from the International Geological Time Scale 2022
data("interval_table") # a table linking intervals in Sepkoski's compendium with the International Geological Time Scale 2022.

## Let's look at the data...
View(sepkoski_raw) # opens a new tab in RStudio

## What variables have we got?
glimpse(sepkoski_raw) # dplyr (tidyverse function)
str(sepkoski_raw) # base R function

## Let's plot Sepkoski's famous curve
sepkoski_curve()

## Take a look at the help file to customise the plot
?sepkoski_curve
sepkoski_curve(fill = TRUE)


###############################################################################
#
#   2. Exploring fossil record biases
# 
# ******************************************************


# 0. Packages used in this script -----------------------------------------

#library(tidyverse)
library(geoscale) # for plotting with the geological time scale on the x-axis (uses base R syntax)
library(viridis) # for colour scales
library(vegan) # for diversity metrics
library(rgplates) # palaeogeographic reconstructions
library(dplyr)
library(ggplot2)

select <- dplyr::select # ensure the select function is coming from dplyr

# 1(a). Sampling proxy counts ---------------------------------------------------

## Let's explore sampling patterns!
## First we'll calculate counts of sampling proxies and plot these alongside raw diversity

## For this example, I've gone through all the cleaning beforehand, just load 
##  the cleaned .csv file!

d <- "~/Github/BRI_Prieto/rshiny"
setwd(d)

# Load data
occ_data <- read.csv("data/NewMytilidaePBDB_Staged.csv")

# Create intervals object
intervals <- data.frame(max_ma = occ_data$max_ma, 
                        min_ma = occ_data$min_ma, 
                        stage_name = occ_data$stages, 
                        avg_ma = occ_data$avg_ma)

# Save intervals to CSV
write.csv(intervals, "data/intervals_mytilids.csv", row.names = FALSE)

# Read the intervals file
orig_intervals <- read.csv("data/intervals_mytilids.csv")

intervals <- orig_intervals

# Inspect the cleaned data
head(intervals)

# Taxa per interval 
count_taxa <- vector("numeric") # create empty vector for the loop below to populate

count_taxa <- numeric(nrow(intervals))

for (i in 1:nrow(intervals)) { # for-loop to count each taxon that appears in each interval
  out <- subset(occ_data, max_ma > intervals[i,]$min_ma & min_ma < intervals[i,]$max_ma) # uses our intervals dataframe
  count_taxa[i] <- (length(unique(out$accepted_name)))
  print(count_taxa[i])
}

# Collections per interval
count_colls <- vector("numeric")
for (i in 1:nrow(intervals)) {
  out <- subset(occ_data, max_ma > intervals[i,]$min_ma & min_ma < intervals[i,]$max_ma)
  count_colls[i] <- (length(unique(out$collection_no)))
  print(count_colls[i])
}

# Formations per interval
count_formations <- vector("numeric")
for (i in 1:nrow(intervals)) {
  out <- subset(occ_data, max_ma > intervals[i,]$min_ma & min_ma < intervals[i,]$max_ma)
  count_formations[i] <- (length(unique(out$formation)))
  print(count_formations[i])
}

## For equal-area gird cells, I recommend the package 'icosa' (Kocsis, 2017)
## For more info see: http://cran.nexr.com/web/packages/icosa/vignettes/icosaIntroShort.pdf


## Gather the proxy information together in a new dataframe for plotting:
proxy_counts <- data.frame(intervals$stage_name, intervals$avg_ma, count_taxa, count_colls, count_formations)
## Rename the columns for ease:
proxy_counts <- rename(proxy_counts, 
                       "stage_name" = "intervals.stage_name", 
                       "avg_ma" = "intervals.avg_ma")

## Finally, convert all zero's to NAs for plotting 
## This means that the plots won't register zero and instead will leave gaps 
## where there is no sampling instead - this gives a more realistic picture
proxy_counts[proxy_counts == 0] <- NA # NA is for a gap in data instead a zero for plotting



# 1(b). Sampling plots ----------------------------------------------------------

library(geoscale)
library(ggplot2)

## Let's get plotting these patterns!

## Option 1: Plotting using ggplot with log-transformation

## Set interval boundaries for the dotted lines on the plot
## We'll also use this vector again, so its handy to have :)
int_boundaries <- c(450, 425, 400, 375, 350,325, 300, 275, 250, 225, 200, 175, 150, 125, 100, 75, 50, 25, 0)

## Set up your ggplot layers (first layer goes on the bottom, etc):
proxy_plot <- ggplot() + 
  # Formations (as dots and a line):
  geom_line(data = proxy_counts, aes(avg_ma, log1p(count_formations), color = "Formations"), linewidth = 1.2, linetype = "dashed")  +
  geom_point(data = proxy_counts, aes(avg_ma, log1p(count_formations), color = "Formations"), size = 4, shape = 16) +
  # Collections (as dots and a line):
  geom_line(data = proxy_counts, aes(avg_ma, log1p(count_colls), color = "Collections"), linewidth = 1.2, linetype = "dashed")  +
  geom_point(data = proxy_counts, aes(avg_ma, log1p(count_colls), color = "Collections"), size = 5, shape = 16) +
  # Taxa (as dots and a line):
  geom_line(data = proxy_counts, aes(avg_ma, log1p(count_taxa), color = "Taxa"), linewidth = 1.2)  +
  geom_point(data = proxy_counts, aes(avg_ma, log1p(count_taxa), color = "Taxa"), size = 4, shape = 16) +
  
  # Add a minimal theme - but you can make your own custom themes too!
  theme_minimal() + 
  labs(x = "Time (Ma)", y = "Log-transformed Sampling proxy counts") +
  # Make sure to reverse the x-axis to match geological time!
  scale_x_reverse(breaks = int_boundaries) +
  # And tidy up our y-axis with even breaks that match the totals in our dataframe:
  scale_y_continuous(breaks = seq(0, 7, 0.5)) + # Adjust the breaks for log-transformed scale
  # Add legend
  scale_color_manual(values = c("Formations" = "orangered3", "Collections" = "peru", "Taxa" = "black"))

## Call the finished plot to the RStudio plots tab:
proxy_plot

## Set dimensions and save plot (as pdf) to the plots folder
#dir.create("./plots") # create new folder if one doesn't already exist
ggsave(plot = proxy_plot,
       width = 20, height = 15, dpi = 500, units = "cm", 
       filename = "plots/Mytilidaesampling_proxies.pdf", useDingbats=FALSE)

##__________________________________
## Option 2: Plotting using geoscale
##__________________________________

# Ensure proxy_counts is sorted by avg_ma in descending order
proxy_counts <- proxy_counts[order(-proxy_counts$avg_ma),]

# Recalculate log-normalized values (just to be safe)
log_count_taxa <- log(proxy_counts$count_taxa + 1)
log_count_formations <- log(proxy_counts$count_formations + 1)
log_count_colls <- log(proxy_counts$count_colls + 1)

# Save as PDF
pdf("plots/Mytilidaesampling_proxies_Taxa_geoscale.pdf", width = 9, height = 7) 

geoscalePlot(proxy_counts$avg_ma, log_count_taxa, 
             units = c("Period"),
             tick.scale = "Period",
             boxes = "Period", 
             abbrev = c("Period"), 
             lty = 1, pch = NA, 
             cex.age = 1, cex.ts = 1, cex.pt = 1, 
             age.lim = c(430, 0), 
             data.lim = c(0, 7), 
             ts.col = TRUE, 
             ts.width = 0.17, 
             label = "Log-transformed Counts", 
             direction ="horizontal", 
             erotate = 90) 

# Formations
lines(proxy_counts$avg_ma, log_count_formations, col="peru", lty = 1, lwd = 3)

# Collections
lines(proxy_counts$avg_ma, log_count_colls, col="maroon", lty = 1, lwd = 3)

# Taxa
lines(proxy_counts$avg_ma, log_count_taxa, col="black", lty = 1, lwd = 3)

# Legend
legend('topright', legend = c("Formations", "Collections", "Taxa"), 
       col = c("peru", "maroon", "black"), 
       lty = 1, lwd = 3, box.lty = 1, bg= "white")

dev.off()

##__________________________________
## Option 3: Plotting Surface area (occupied grid cells) Range on Top of Geoscale 
##__________________________________

# Load surface area data
results <- read.csv("data/icosa_occupancy_results.csv")

# Ensure convex hull data is sorted by avg_ma in descending order
results <- results[order(results$Stage),]

# Log-transform convex hull values
log_area <- log(results$Occupied_km2 + 1)

# Ensure proxy_counts is sorted by avg_ma in descending order
proxy_counts <- proxy_counts[order(-proxy_counts$avg_ma),]

# Recalculate log-normalized values (just to be safe)
log_count_taxa <- log(proxy_counts$count_taxa + 1)
log_count_formations <- log(proxy_counts$count_formations + 1)
log_count_colls <- log(proxy_counts$count_colls + 1)

# Save as PDF
pdf("plots/MytilidaeRange_sampling_proxies_geoscale.pdf", width = 9, height = 7) 

geoscalePlot(proxy_counts$avg_ma, log_count_taxa, 
             units = c("Period"),
             tick.scale = "Period",
             boxes = "Period", 
             abbrev = c("Period"), 
             lty = 1, pch = NA, 
             cex.age = 1, cex.ts = 1, cex.pt = 1, 
             age.lim = c(430, 0), 
             data.lim = c(0, 20),  # Increased limit to accommodate convex hull values
             ts.col = TRUE, 
             ts.width = 0.17, 
             label = "Log-transformed Counts", 
             direction ="horizontal", 
             erotate = 90) 

# Formations
lines(proxy_counts$avg_ma, log_count_formations, col="peru", lty = 1, lwd = 3)

# Collections
lines(proxy_counts$avg_ma, log_count_colls, col="maroon", lty = 1, lwd = 3)

# Taxa
lines(proxy_counts$avg_ma, log_count_taxa, col="black", lty = 1, lwd = 3)

# Convex Hull (range size)
lines(results$Stage, log_area, col="blue", lty = 2, lwd = 3)

# Legend
legend('topright', legend = c("Formations", "Collections", "Taxa", "Area of Occupied Grid cells (Range Size)"), 
       col = c("peru", "maroon", "black", "blue"), 
       lty = c(1, 1, 1, 2), lwd = 3, box.lty = 1, bg= "white")

dev.off()

##__________________________________
## Option 4: Creating a Panel figure to compare Mytilidae diversity and range size
##__________________________________

#*# Load surface area data
# Load & prep data
library(mgcv)

# Z-score both variables
log_area <- log(results$Occupied_km2 + 1)
z_area <- scale(log_area)[,1]

log_diversity <- log(proxy_counts$count_taxa + 1)
z_diversity <- scale(log_diversity)[,1]

# Fit GAM models
gam_div <- gam(z_diversity ~ s(avg_ma, k = 10), data = proxy_counts)
gam_area <- gam(z_area ~ s(Stage, k = 10), data = results)

# Create prediction data
ma_seq_div <- seq(min(proxy_counts$avg_ma), max(proxy_counts$avg_ma), length.out = 200)
ma_seq_area <- seq(min(results$Stage), max(results$Stage), length.out = 200)

pred_div <- predict(gam_div, newdata = data.frame(avg_ma = ma_seq_div))
pred_area <- predict(gam_area, newdata = data.frame(Stage = ma_seq_area))

# Plot setup
pdf("plots/Mytilidae_range_diversity_gam.pdf", width = 10, height = 8)
layout(matrix(1:2, nrow = 2), heights = c(1, 1))

# --- Panel A: Z-scored Diversity ---

# Define mass extinctions
mass_extinctions <- c(444, 372, 252, 201, 66)
par(mar = c(2, 5, 2, 1))
geoscalePlot(proxy_counts$avg_ma, z_diversity,
             units = c("Period"),
             tick.scale = "Period",
             boxes = "Period", 
             abbrev = c("Period"), 
             lty = 1, pch = NA, 
             cex.age = 1.2, cex.ts = 1.2, 
             age.lim = c(430, 0), 
             data.lim = c(-5, 5),  
             ts.col = TRUE, 
             ts.width = 0.15, 
             label = "Z-scored Log(Diversity)", 
             direction = "horizontal", 
             erotate = 90)

lines(proxy_counts$avg_ma, z_diversity, col = "black", lwd = 2)
lines(ma_seq_div, pred_div, col = "darkgreen", lwd = 3, lty = 2)
for (ext in mass_extinctions) abline(v = ext, col = "red", lwd = 2)
legend("topright", legend = c("Observed", "GAM Smooth"),
       col = c("black", "darkgreen"), lwd = 2, lty = c(1, 2), bty = "n")


# --- Panel B: Z-scored Range Size ---
par(mar = c(4, 5, 1, 1))
geoscalePlot(results$Stage, z_area,
             units = c("Period"),
             tick.scale = "Period",
             boxes = "Period", 
             abbrev = c("Period"), 
             lty = 1, pch = NA, 
             cex.age = 1.2, cex.ts = 1.2, 
             age.lim = c(430, 0), 
             data.lim = c(-5, 5),  
             ts.col = TRUE, 
             ts.width = 0.15, 
             label = "Z-scored Log(Range Size)", 
             direction = "horizontal", 
             erotate = 90)

lines(results$Stage, z_area, col = "blue", lwd = 2)
lines(ma_seq_area, pred_area, col = "darkorange", lwd = 3, lty = 2)
for (ext in mass_extinctions) abline(v = ext, col = "red", lwd = 2)

legend("topright", legend = c("Observed", "GAM Smooth"),
       col = c("blue", "darkorange"), lwd = 2, lty = c(1, 2), bty = "n")

dev.off()

# Done!
