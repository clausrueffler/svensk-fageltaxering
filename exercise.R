# Exercise
birds <- readRDS("bird_data_subset2023.RDA")

# or

birds <- read.csv("bird_data_subset2023.csv", stringsAsFactors = FALSE)

dim(birds)
head(birds, 3)

species.data <- unique(birds[ , c("species", "habitat")])
species.data

# Sum counts for each species (over all years)
total.counts <- tapply(birds$organismQuantity, birds$species, sum)
total.counts

# We add a new column to the data frame `species.data` containing the total counts in the correct order
species.data$total.count <- total.counts[match(species.data$species, names(total.counts))]
species.data

# Make a vector of colors for plotting
species.data$col <- ifelse(species.data$habitat %in% "Farms", "darkorange3", "forestgreen")

# Set figure margins for plotting
par(mar=c(7,4,1,1))

# Create a bar plot of total counts per species
barplot(species.data$total.count/1000, 
        names.arg=species.data$species,
        las=2, col=species.data$col,
        ylim = c(0,100),
        ylab="Total observations (x 1000)",
        cex.names=0.5)

# Add a legend to the plot
legend("topleft", legend=c("Farmland","Forest"), 
       pch=22, pt.bg=c("darkorange3", "forestgreen"),
       bty="n", pt.cex=2)

count.per.year <- tapply(birds$organismQuantity, list(birds$year, birds$species), sum)

# Convince yourself that this gives the desired result.
head(count.per.year, 3)

count.per.year <- count.per.year[ , c(match(species.data$species, names(total.counts)))]

# Let us inspect whether the data look as intended.
head(count.per.year, 3)

# First set the layout and margins of the figure
par(mfcol=c(5,4), mar=c(2,4,1.5,1), oma=c(0,4,0,4))

for (i in 1:ncol(count.per.year)){# this for-loop iteratively produces the plot for the different species by cycling through the columns of the data frame `count.per.year`.
  plot(1998:2021, count.per.year[ , i], 
       type="l", ylab="Total Obs.", xlab=NA,
       col=ifelse(i %in% 1:10, "darkorange3", "forestgreen"), 
       lwd=3)
  # Last, add a title at the top of the panel with the name of the species
  mtext(unique(birds$species)[i], 3, 0, cex=0.5, font=2)
}

# Compute the number of routes recorded for each year
nroutes <- colSums(table(birds$LocationID, birds$year) > 0)

# Let's redo the figure by dividing the number of observations 
# per species per year by the number of routes surveyed each year.
# First set the layout and margins of the figure.
par(mfcol=c(5,4), mar=c(2,4,1.5,1), oma=c(0,4,0,4))

for (i in 1:ncol(count.per.year)){
  plot(1998:2021, count.per.year[ , i]/nroutes, 
       type="l", ylab="Obs. per Route", xlab=NA,
       col=ifelse(i %in% 1:10, "darkorange3", "forestgreen"), 
       lwd=3)
  # Last, add a title at the top of the panel with the name of the species
  mtext(unique(birds$species)[i], 3, 0, cex=0.5, font=2)
}

# Time series for the average number of Skylarks per standard route for each year.
count.per.year[ , 1]/nroutes

(count.per.year[ , 1]/nroutes)/(count.per.year[1 , 1]/nroutes[1])


par(mfcol=c(5,4), mar=c(2,4,1.5,1), oma=c(0,4,0,4))

for (i in 1:ncol(count.per.year)){
  plot(1998:2021, (count.per.year[ , i]/nroutes)/(count.per.year[1 , i]/nroutes[1]),
       # for better comparison with the plots from Svensk Fågeltaxering we change the y-axis so that it starts at zero and is exactly as long as needed 
       ylim = c(0, max((count.per.year[ , i]/nroutes)/(count.per.year[1 , i]/nroutes[1])+0.3)), yaxs="i",
       type="l", ylab="Obs. per Route", xlab=NA,
       col=ifelse(i %in% 1:10, "darkorange3", "forestgreen"), 
       lwd=3)
  mtext(unique(birds$species)[i], 3, 0, cex=0.5, font=2)
}

# Sum over all routes for each species and each year.
count.per.year <- tapply(birds$organismQuantity, list(birds$year, birds$species), sum)
# As in the previous exercise, we have re-order the species from alphabetic back to the original order. We use the same trick, the match-function.
count.per.year <- count.per.year[ , c(match(species.data$species, names(total.counts)))]

# Index for each species for each year with counts expressed relative to the count in the first year.
for (i in 1:ncol(count.per.year)){
  count.per.year[ , i] <- (count.per.year[ , i]/nroutes)/(count.per.year[1 , i]/nroutes[1])
  index <- count.per.year
}

# Compute the arithmetic mean by summing over all columns with farmland and forest birds, respectively, and then dividing by the number of species.
farmland.arith <- rowSums(count.per.year[ , 1:10])/10
forest.arith <- rowSums(count.per.year[ , 11:20])/10

# Time series plots.
plot(1998:2021, farmland.arith,
     ylim = c(0.8, max(forest.arith) + 0.1), yaxs="i",
     type="l", ylab="arithmetic mean index", xlab=NA,
     col= "darkorange3", lwd=3)
lines(1998:2021, forest.arith,
      type="l", ylab="arithmetic mean index", xlab=NA,
      col= "forestgreen", lwd=3)
legend("top", legend = c("Farmland","Forest"), col= c("darkorange3", "forestgreen"), lty = 1, lwd = 3)

# Computing the geometric mean for farmland and forest birds.
farmland.geom <- exp(rowMeans(log(index[ , 1:10])))
forest.geom <- exp(rowMeans(log(index[ , 11:20])))

plot(1998:2021, farmland.geom,
     ylim = c(0.8, max(forest.geom) + 0.1), yaxs="i", 
     ylab="geometric mean index", xlab=NA,
     type="l", col= "darkorange3", lwd=3)
lines(1998:2021, forest.geom,
      type="l", col= "forestgreen", lwd=3)
legend("topleft", legend = c("Farmland","Forest"), col= c("darkorange3", "forestgreen"), lty = 1, lwd = 3) 
