library(plyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(viridis)
library(tidyr)

summer.energy = readRDS("analysis/compiled_data.RData") %>%
  filter(bin.size == 1) %>%
  select(Age, SummerEnergy, latitude, threshold, GSLSpeleoIce)

path <- "analysis/obliquity_to_precession_fraction.RData"
obliquity.to.precession.frac = readRDS(path) %>% as.data.frame

# File names for the summaries.
path <- "results/crossmap_summerenergy_GSL_speleoice_summaries/"
summary.files <- list.files(path = path, full.names = T)

# Load summaries and combine them into a single data table.
skills <- lapply(summary.files, function(f) {readRDS(f)}) %>% 
  data.table::rbindlist() %>% 
  filter(lag == 0) %>%  # Plot results at lag 0
  filter(window.size == 12) %>%  # Size of causal test window
  mutate(median = ifelse(causaldifference > 0, median, 0)) %>% # Causal test criterion
  mutate(median = ifelse(median > X99._surr, median, 0)) %>% # Surrogate data criterion
  mutate(surr.diff = median - X99._surr) %>%
  select(threshold, latitude, median, X99._surr, surr.diff) %>%
  complete(threshold = seq(0, 500, 25), 
           latitude = seq(-90, 90, 1),
           fill = list(median = 0))

# List to store all summer energy time series that are significantly predicted 
timeseries_SH = list()
timeseries_NH1 = list() 
timeseries_NH2 = list() 
timeseries_all = list()

# Fill in the lists with significantly predicted time series.
for (lat in unique(skills$latitude)) {
  for (t in unique(skills$threshold)) {
    # Check if median.rho is > 0 at this particular combination of latitude and 
    # threshold
    rho = skills[which(skills$latitude == lat & skills$threshold == t), ]$median[[1]]
    surr.diff = skills[which(skills$latitude == lat & skills$threshold == t), ]$surr.diff[[1]]
    
    if (rho > 0 & surr.diff > 0) {
      # SH
      if (lat < -20) {
        cat(lat, " ", t, "\n")
        se_local = summer.energy %>% dplyr::filter(latitude == lat, 
                                                   threshold == t) %>% 
          select(Age, SummerEnergy)
        
        timeseries_SH[[paste(lat, "_", t, sep="")]] = list(se_local, rho)
        timeseries_all[[paste(lat, "_", t, sep="")]] = list(se_local, rho)
        
        # NH2
      } else if (lat >= -10 & lat <= 60 & t <= 250 ||
                 lat >= -10 & lat <= 54 & t == 275 ||
                 lat >= -10 & lat <= 40 & t == 300 || 
                 lat >= -10 & lat <= 35 & t == 325 ||
                 lat >= -10 & lat <= 30 & t == 350 || 
                 lat >= -10 & lat <= 25 & t == 375 ||
                 lat >= -10 & lat <= 20 & t == 400 ||
                 lat >= -10 & lat <= 17 & t == 425 ||
                 lat >= -10 & lat <= 17 & t >= 450)  {
        cat(lat, " ", t, "\n")
        se_local = summer.energy %>% dplyr::filter(latitude == lat, 
                                                   threshold == t) %>% 
          select(Age, SummerEnergy)
        
        timeseries_NH2[[paste(lat, "_", t, sep="")]] = list(se_local, rho)
        timeseries_all[[paste(lat, "_", t, sep="")]] = list(se_local, rho)
        
        # NH1
      } else if (t >= 500 & t <= 425 & lat >= 13 ||
                 t == 400 & lat >= 20 ||
                 t == 375 & lat >= 20 ||
                 t == 350 & lat >= 30 ||
                 t == 325 & lat >= 35 ||
                 t == 300 & lat >= 40 ||
                 t == 275 & lat >= 45 ||
                 t == 250 & lat >= 50) {
        cat(lat, " ", t, "\n")
        se_local = summer.energy %>% dplyr::filter(latitude == lat, 
                                                   threshold == t) %>% 
          select(Age, SummerEnergy)
        
        timeseries_NH1[[paste(lat, "_", t, sep="")]] = list(se_local, rho)
        timeseries_all[[paste(lat, "_", t, sep="")]] = list(se_local, rho)
      }
    }
  }
}

#' Compute the first principal component summer energy. Takes a list of data 
#' frames where each element is a data frame with a column named SummerEnergy.
get_pc1 <- function(timeserieslist) {
  # Get number of rows from first timeseries
  
  rows = timeserieslist[[1]][[1]] %>% nrow
  cols = length(timeserieslist)
  # Preallocate matrix to put the time series in.
  m = matrix(ncol = cols, nrow = rows)
  
  for (i in 1:cols) {
    m[1:rows, i] = timeserieslist[[i]][[1]]$SummerEnergy
  }
  
  pc = prcomp(x = m)
  pc1.scaled = - scale(pc$x[, 1])
  pc1 = data.frame(timeserieslist[[1]][[1]]$Age, pc1.scaled)
  colnames(pc1) = c("Age", "PC1")
  pc1
}

#' Gather the summer energy columns of a list of data frames containing 
#' summer energy at different latitude/threshold configurations into a 
#' a single matrix.
gather_ts <- function(timeserieslist) {
  # Average forcing ----
  rows = nrow(timeserieslist[[1]][[1]])
  cols = length(timeserieslist)
  print(rows)
  print(cols)
  m = matrix(ncol = cols, nrow = rows)
  n = names(timeserieslist)
  p <- plot_ly()
  for (i in 1:cols) {
    m[1:rows, i] = timeserieslist[[i]][[1]]$SummerEnergy
  }
  m
}

# Matrices of time series
ts_SH = gather_ts(timeseries_SH) 
ts_NH1 = gather_ts(timeseries_NH1)
ts_NH2 = gather_ts(timeseries_NH2)

# First principal components
pc1_SH = get_pc1(timeserieslist = timeseries_SH) %>% 
  mutate(cluster = "PC1 SH")
pc1_NH1 = get_pc1(timeserieslist = timeseries_NH1) %>% 
  mutate(cluster = "PC1 NH1")
pc1_NH2 = get_pc1(timeserieslist = timeseries_NH2) %>% 
  mutate(cluster = "PC1 NH2")

fig4 <- plot_ly(width = 1150, height = 600) %>%
  layout(xaxis = list(title = "Age (kyr)",
                      dtick = 50,
                      tickmode = "array",
                      tickvals = seq(-50, 800, 50),
                      autorange = "reversed",
                      zeroline = F),
         yaxis = list(zeroline = F,
                      dtick = 0,
                      title = "", #"Normalised and shifted values",
                      showticklabels = F,
                      showgrid = F),
         font = list(size = 20),
         legend = list(xref = "x", xanchor = "center", x = 0.5,
                       yref = "y", yanchor = "bottom", y = 0.95,
                       orientation = "h",
                       bgcolor = "white",
                       opacity = 1.0),
         margin = list(b = 50, t = 0, r = 20, l = 70)) %>%
  config(displayModeBar = F) %>%
  
  # NH1
  add_lines(data = pc1_NH1, name = "PC1: NH1",
            x = ~Age, y = ~ PC1 + 5, 
            line = list(color = toRGB("black", 1.5), width = 1, dash = "solid"),
            legendgroup = "PC1",
            showlegend = F) %>%
  # NH2
  add_lines(data = pc1_NH2, name = "PC1: NH2",
            x = ~Age, y = ~ PC1 - 1, 
            line = list(color = toRGB("blue", 0.8), width = 1.5, dash = "solid"),
            legendgroup = "PC1",
            showlegend = F) %>%
  
  # SH
  add_lines(data = pc1_SH, name = "PC1: SH",
            x = ~Age, y = ~ (-PC1) + 2.5, 
            line = list(color = toRGB("red", 1), width = 1.5, dash = "solid"),
            legendgroup = "PC1",
            showlegend = F) %>%
  
  ###############
  # Summer energy 
  ###############  
  add_lines(data = summer.energy %>%
            filter(latitude == 65, threshold == 350) %>%
            mutate(SummerEnergy = as.vector(scale(SummerEnergy))),
          x = ~Age, y = ~SummerEnergy + 9,
          name = "Summer energy, 65°N, 350 W/m<sup>2</sup>",
          legendgroup = "additional",
          line = list(color = "orange", width = 1.5, dash = "solid"),
          showlegend = F) %>%
  
  ###############
  # GSL
  ###############  
  add_lines(data = summer.energy %>%
            filter(latitude == 65, threshold == 350),
          x = ~Age, y = ~as.vector(scale(GSLSpeleoIce)) + 13.5,
          name = "GSL on orbitally independent age model",
          legendgroup = "additional",
          line = list(color = "green", width = 1.2, dash = "solid"),
          showlegend = F) %>%
  
  ###############
  # ANNOTATIONS
  ###############  
  add_annotations(xref = "x", yref = "y", 
                  xanchor = "center", yanchor = "center",
                  x = 880, y = -0.5, text = "PC1: NH2", showarrow = F) %>%
  add_annotations(xref = "x", yref = "y", 
                  xanchor = "center", yanchor = "center",
                  x = 880, y = 2.5, text = "PC1: SH", showarrow = F) %>%
  add_annotations(xref = "x", yref = "y", 
                  xanchor = "center", yanchor = "center",
                  x = 880, y = 5.5, text = "PC1: NH1", showarrow = F) %>%
  add_annotations(xref = "x", yref = "y", 
                  xanchor = "center", yanchor = "center",
                  x = 880, y = 8.5,  
                  text = "Summer energy<br>65°N<br>350 W/m<sup>2</sup>", 
                  showarrow = F) %>%
  add_annotations(xref = "x", yref = "y", 
                  xanchor = "center", yanchor = "center",
                  x = 880, y = 13, 
                  text = "GSL on orbitally<br>independent<br>age model", 
                  showarrow = F) %>%
  add_annotations(xref = "x", yref = "y", 
                  xanchor = "center", yanchor = "center",
                  x = 0, y = 8.5, 
                  text = "", showarrow = F)

fig4