# Computes the obliquity to precession ratio of summer energy time series 
# at specified latitudes and threshold values
library(ggplot2)
library(dplyr)
library(dtplyr)
library(data.table)

latitudes = seq(-90, 90, 1)
thresholds = seq(0, 500, 25)

# Initialise matrices
obliquity.to.precession.matrix = matrix(ncol = length(thresholds), 
                                        nrow = length(latitudes))
colnames(obliquity.to.precession.matrix) = thresholds
rownames(obliquity.to.precession.matrix) = latitudes

# Iterate over latitudes and thresholds and determine the contribution of 
# orbital components in the summer energy time series.
summer.energy = readRDS("analysis/compiled_data.RData") %>%
  as.data.table %>%
  filter(bin.size == 1) %>%
  select(Age, SummerEnergy, latitude, threshold)


for (i in 1:length(lats)) {
  for (j in 1:length(thresholds)) {
    lat = latitudes[i]
    t = thresholds[j]
    
    se = summer.energy %>% filter(latitude == lat, threshold == t)
    timeseries = stats::ts(se$SummerEnergy, deltat = 1, frequency = 1)
    multitaper.spec = multitaper::spec.mtm(timeseries, 
                                           dtUnits = "year", 
                                           deltat = 1000, 
                                           nw = 4.0, 
                                           k = 8, 
                                           plot = F)
    spec = multitaper.spec$spec
    freq = multitaper.spec$freq
    
    spectral.estimate = data.frame(freq, spec)

    total.power = sum(spectral.estimate$spec)
    obliquity.power = sum(x = spectral.estimate %>% 
                            filter(freq >= 1/41000 - 1/150000 & 
                                     freq <= 1/41000 + 1/150000) %>% 
                            select(spec))
    precession.power = sum(x = spectral.estimate %>% 
                             filter(freq >= 1/21000 - 1/150000 & 
                                      freq <= 1/21000 + 1/150000) %>% 
                             select(spec))
    obliquity.to.precession.frac = obliquity.power /
                                    (precession.power + obliquity.power)
    
    obliquity.to.precession.matrix[i, j] =  obliquity.to.precession.frac
    
    cat("latitude = ", lat, " threshold = ", t, 
        "\tobliquity/precession = ", obliquity.to.precession.frac, "\n")
  }
}

# Melt the matrices to get data on a format that is easy to plot and 
# fix the colnames of the melted matrices
obliquity.to.precession = melt(obliquity.to.precession.matrix)
colnames(obliquity.to.precession) = c("latitude", 
                                      "threshold", 
                                      "obliquity.to.precession")

# Save to file
p1 <- "analysis/obliquity_to_precession_fraction.RData"
p2 <- "analysis/obliquity_to_precession_fraction_matrix.RData"
saveRDS(obliquity.to.precession, p1)
saveRDS(obliquity.to.precession.matrix, p2)

