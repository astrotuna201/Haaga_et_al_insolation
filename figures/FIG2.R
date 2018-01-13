library(plotly)
library(tidyr)

# File names for the summaries.
path <- "results/crossmap_summerenergy_GSL_speleoice_summaries"
summary.files <- list.files(path = path, full.names = T)

# Load summaries and combine them into a single data table.
summary <- lapply(summary.files, function(f) {readRDS(f)}) %>% 
  data.table::rbindlist()


# Load information about orbital frequencies at each latitude for each threshold
path <- "analysis/obliquity_to_precession_fraction_matrix.RData"
orb <- readRDS(path)

orbital_ratios = list()
orbital_ratios$z = orb
orbital_ratios$x = as.character(as.numeric(colnames(orb)))
orbital_ratios$y = as.character(as.numeric(rownames(orb)))

dt <- summary %>% 
  # Plot results at lag 0
  filter(lag == 0) %>% 
  # Size of causal test window
  filter(window.size == 12) %>%  
  # Causal test criterion
  mutate(median = ifelse(causaldifference > 0, median, 0)) %>% 
  # Surrogate data criterion
  mutate(median = ifelse(median > X99._surr, median, 0)) %>% 
  select(threshold, latitude, median) %>%
  # Set cross mapping value to zero for latitude-threshold combinations 
  # where we have no results.
  complete(threshold = seq(0, 500, 25), 
           latitude = seq(-90, 90, 1),
           fill = list(median = 0))

# Plot heatmap
fig2 <- plot_ly(height = 900, width = 1150) %>% 
  add_trace(type = "heatmap",
            data = dt,
            x = ~threshold, y = ~latitude, z = ~median) %>%
  colorbar(title = "CCM skill<br>at zero lag") %>% 
  config(displayModeBar = F) %>% 
add_trace(inherit = F, type = "contour",
          x = orbital_ratios$x,
          y = orbital_ratios$y,
          z = ~orbital_ratios$z,
          autocontour = F,
          contours = list(start = 0.1,
                          end = 0.9,
                          size = 0.2,
                          coloring = "lines",
                          showlabels = T,
                          labelfont = list(size = 17, color = "white")),
          line = list(width = 1.4,
                      smoothing = 0.3,
                      dash = "dash"),
          showscale = F,
          opacity = 0.8,
          colorscale = list(c(0,1), c(toRGB("white"), toRGB("black"))),
          name = "Obliquity/precession") %>%
layout(yaxis = list(title = "Latitude"),
       xaxis = list(title = "Threshold (W/m<sup>2</sup>)"),
       margin = list(b = 70, l = 70, t = 0, r = 0),
       font = list(size = 20))

fig2