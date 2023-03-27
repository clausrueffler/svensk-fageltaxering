# We will use the `rgbif` package to access data on GBIF from R
# This package has lots of functionality, you can read more on the project website:
# https://docs.ropensci.org/rgbif/
install.packages("rgbif")
library(rgbif)

# First we need to request a download of the dataset from GBIF

x <- occ_download(pred("datasetKey", "91fa1a0d-a208-40aa-8a6e-f2c0beb9b253"),
                  format = "SIMPLE_CSV",
                  user = "clausr",
                  pwd = "Loxia123",
                  email = "claus.rueffler@ebc.uu.se"
)

x <- occ_download(pred("datasetKey", "91fa1a0d-a208-40aa-8a6e-f2c0beb9b253"),
                  format = "SIMPLE_CSV",
                  user = # "your GBIF user name",
                  pwd = # "your GBIF password",
                  email = # "your email address associated with your GBIF account"
)

# It will take some time for the download to be available (usually a few minutes)
# We can check on the status of the download with this function
occ_download_wait(x)

# Once it is available, we can download and import the dataset like this
birds <- occ_download_import(occ_download_get(x))

# Let's look at the top of the full dataset
head(as.data.frame(birds))

# There are many columns with no information (all NA) and others that we don't care about for this exercise (e.g., "recordedBy").  So let's choose which columns we want to keep.
focal_columns <- c("year",
                   "species",
                   "individualCount",
                   "decimalLongitude",
                   "decimalLatitude")

# Reduce to only the focal columns
birds <- birds[,focal_columns]

# In the GBIF data, routes are not identified by a name or number, only the geographic coordintes.
pts <- unique(birds[,c("decimalLongitude",
                       "decimalLatitude")])

# It will be helpful to have an easy way to identify individual routes.
# This creates a unique location ID for each point
birds$LocationID <- as.numeric(factor(paste(birds$decimalLongitude, 
                                            birds$decimalLatitude, sep="")))

# Number of years sampled for each route
years_sampled <- tapply(birds$year, 
                        birds$LocationID, 
                        function(x) length(unique(x)))

# Plot a map of Sweden with location of the routes, colored by number of years sampled
library(maps)
map(region="Sweden")

# Create a gradient of colors to represent different number of years sampled
cols <- colorRampPalette(c("yellow", "red"))(max(years_sampled))

points(pts, pch=16, col=cols[years_sampled])

legend("right", legend=c(1, 5, 10, 15, 20, max(years_sampled)), 
       col=cols[c(1, 5, 10, 15, 20, max(years_sampled))], 
       pch=16, bty="n")

# Number of routes sampled for each year
routes_sampled <- tapply(birds$LocationID, birds$year, function(x) length(unique(x)))
plot(names(routes_sampled), routes_sampled, type='l',
     xlab="Year", ylab="No. routes sampled")

# We may want to remove the routes sampled before 1998 to correspond with the Swedish Bird Survey trends...
birds <- birds[birds$year >= 1998,]

# Now let's assign each species to our 

# We can define our focal bird species for farms and forests
farm_birds <- c("Falco tinnunculus",
                "Vanellus vanellus",
                "Alauda arvensis",
                "Saxicola rubetra",
                "Anthus pratensis",
                "Motacilla flava",
                "Emberiza citrinella",
                "Passer montanus",
                "Sturnus vulgaris",
                "Sylvia communis")

forest_birds <- c("Accipiter nisus",
                  "Garrulus glandarius",
                  "Dendrocopos major",
                  "Periparus ater",
                  "Phoenicurus phoenicurus",
                  "Phylloscopus collybita",
                  "Anthus trivialis",
                  "Spinus spinus",
                  "Coccothraustes coccothraustes",
                  "Pyrrhula pyrrhula")

# First let's remove other species
birds <- birds[birds$species %in% c(farm_birds, forest_birds),]

# Now assign habitat based on our classification above
birds$habitat <- ifelse(birds$species %in% farm_birds, "Farms", "Woods")

# We now have a simplified and clean dataset for further analyses
head(birds)




