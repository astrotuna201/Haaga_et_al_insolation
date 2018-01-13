library(plyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(viridis)
library(tidyr)

# File names for the summaries.
path <- "results/crossmap_summerenergy_GSL_speleoice_summaries"
summary.files <- list.files(path = path, full.names = T)

# Load summaries and combine them into a single data table.
ccms <- lapply(summary.files, function(f) {readRDS(f)}) %>% 
  data.table::rbindlist() %>% 
  # Plot results at lag 0
  filter(lag == 0) %>%  
  # Size of causal test window
  filter(window.size == 12) %>%  
  # Causal test criterion
  mutate(median = ifelse(causaldifference > 0, median, 0)) %>%
  # Surrogate data criterion
  mutate(median = ifelse(median > X99._surr, median, 0)) %>% 
  select(threshold, latitude, median, X99._surr) %>%
  # Set skill and surrogate skills to zero for analyses where we have no results
  complete(threshold = seq(0, 500, 25), 
           latitude = seq(-90, 90, 1),
           fill = list(median = 0, 
                       X99._surr = 0))

# Load summer energy data
summer.energy = readRDS("analysis/compiled_data.RData") %>%
  filter(bin.size == 1) %>%
  select(Age, SummerEnergy, latitude, threshold)

orbpath <- "analysis/obliquity_to_precession_fraction.RData"
orbfracs = readRDS(orbpath) %>% as.data.frame
orbfracs$median.rho = rep(NA)
orbfracs$surr99 = rep(NA)
orbfracs$cluster = rep(NA)

for (lat in unique(ccms$latitude)) {
  for (t in unique(ccms$threshold)) {
    # Check if median.rho is > 0 at this particular combination of latitude 
    # and threshold
    
    cat(lat, " ", t, "\n")
    rho = ccms[ccms$latitude == lat & ccms$threshold == t, ]$median[1]
    surr = ccms[ccms$latitude == lat & ccms$threshold == t, ]$X99._surr[1]
    orbfracs[orbfracs$latitude == lat &orbfracs$threshold == t, ]$surr99 = surr 
    orbfracs[orbfracs$latitude == lat &orbfracs$threshold == t, ]$median.rho = rho 
    
    
    # Assign clusters
    if (lat < -20) {
      cluster = "SH"
    } else if (lat >= -10 & lat <= 60 & t <= 250 ||
               lat >= -10 & lat <= 54 & t == 275 ||
               lat >= -10 & lat <= 40 & t == 300 || 
               lat >= -10 & lat <= 35 & t == 325 ||
               lat >= -10 & lat <= 30 & t == 350 || 
               lat >= -10 & lat <= 25 & t == 375 ||
               lat >= -10 & lat <= 20 & t == 400 ||
               lat >= -10 & lat <= 17 & t == 425 ||
               lat >= -10 & lat <= 17 & t >= 450) {
      cluster = "NH2"
    } else if (t >= 500 & t <= 425 & lat >= 13 ||
               t == 400 & lat >= 20 ||
               t == 375 & lat >= 20 ||
               t == 350 & lat >= 30 ||
               t == 325 & lat >= 35 ||
               t == 300 & lat >= 40 ||
               t == 275 & lat >= 45 ||
               t == 250 & lat >= 50) {
      cluster = "NH1"
    }
    
    orbfracs[orbfracs$latitude == lat & 
               orbfracs$threshold == t, ]$cluster = cluster
  }
}


fig3 <- plot_ly(width = 1150, height = 600) %>%
  add_trace(type = "scatter", mode = "markers",
            data = orbfracs %>% filter(median.rho > 0,
                                       median.rho > surr99,
                                       cluster == "NH1"),
            name = "NH1",
            x = ~obliquity.to.precession, y = ~median.rho, 
            marker = list(size = 11,
                          symbol = "cross-dot",
                          opacity = 0.8,
                          color = "black")) %>%
  add_trace(type = "scatter", mode = "markers",
            data = orbfracs %>% filter(median.rho > 0,
                                       median.rho > surr99,
                                       cluster == "NH2"),
            name = "NH2",
            x = ~obliquity.to.precession,  y = ~median.rho,
            marker = list(size = 11,
                          symbol = "circle",
                          opacity = 1,
                          color = toRGB("blue", alpha = 0.6))) %>%

add_trace(type = "scatter", mode = "markers",
          data = orbfracs %>% filter(median.rho > 0,
                                     median.rho > surr99,
                                     cluster == "SH"),
          name = "SH",
          x = ~obliquity.to.precession,  y = ~median.rho, 
          marker = list(size = 11,
                        symbol = "triangle-left",
                        opacity = 0.8,
                        color = "red")) %>%

  config(displayModeBar = F) %>%
  layout(xaxis = list(title = "Obliquity to precession ratio in forcing time series",
                      zeroline = F,
                      showgrid = F,
                      showline = T,
                      ticks = "inside"),
         yaxis = list(title = "CCM skill",
                      zeroline = F,
                      showgrid = F,
                      showline = T,
                      ticks = "inside"),
         legend = list(xref = "x", xanchor = "left", x = 0.04,
                       yref = "y", yanchor = "top", y = 0.97, opacity = 0.5),
         font = list(size = 20),
         margin = list(b = 60, l = 100, t = 0, r = 0))

fig3