require(R.matlab)
require(dplyr)
require(plotly)
require(viridisLite)

################################################################################
# Load daily insolation for selected latitudes, which has been pre-generated
# using the code accompanying Huybers (2006), and combine it into one data table
################################################################################
path <- "data/yearlyins_35Nand65N.mat"
yearly_ins <- R.matlab::readMat(path)$yearly.ins %>% as.data.frame
colnames(yearly_ins) <- c("Day", "Insolation.35N", "Insolation.65N")

# Combine the daily insolation data into one data table.
insolation_35N <- yearly_ins %>% 
  select(Day, Insolation.35N) %>% 
  magrittr::set_colnames(c("Day", "Insolation")) %>% 
  mutate(latitude = 35)

insolation_65N <- yearly_ins %>% 
  select(Day, Insolation.65N) %>% 
  magrittr::set_colnames(c("Day", "Insolation")) %>% 
  mutate(latitude = 65)

ins <- data.table::rbindlist(list(insolation_35N, insolation_65N)) %>%
  reshape2::melt(id.vars = c("latitude", "Day"))


########################
# DAILY INSOLATION PLOTS
########################

# Format axes
xax <- list(title = "", zeroline = F)
yax <- list(title = "Insolation intensity<br>(W/m<sup>2</sup>)", 
            zerolinecolor = toRGB("black", alpha = 0.4), zerolinewidth = 0.6,
            tickmode = "array", tickvals = seq(0, 500, 100))

# Define colors
col65N = viridisLite::viridis(n = 8, option = "C")[1]
col35N = viridisLite::viridis(n = 8, option = "C")[6]

# Daily insolation plot for 65°N at 250 W/m^2 threshold
p65_250 <- plot_ly() %>%
  # Daily insolation curve through the year
  add_trace(type = "scatter", mode = "lines",
            data = ins %>% filter(latitude == 65), 
            name = "Yearly insolation (65°N)",
            line = list(color = "black", width = 0.85),
            x = ~Day, y = ~value,
            showlegend = F,
            xaxis = "x1", yaxis = "y1") %>% 
  add_trace(type = "scatter", mode = "none", fill = "tozeroy",
            data = ins %>% filter(latitude == 65, value >= 250),
            name = "65°N",
            showlegend = F,
            fillcolor = col65N, 
            x = ~Day, y = ~value) %>% 
  add_trace(type = "scatter", mode = "lines", 
            x = c(0, 365), y = c(250, 250), showlegend = F, 
            line = list(color = "black", dash = "dashdot", width = 0.85),
            xaxis = "x1", yaxis = "y1") %>%
  layout(xaxis = xax, yaxis = yax)

# Daily insolation plot for 65°N at 450 W/m^2 threshold
p65_450 <- plot_ly() %>%
  # Daily insolation curve through the year
  add_trace(type = "scatter", mode = "lines",
            data = ins %>% filter(latitude == 65), 
            name = "Yearly insolation (65°N)",
            line = list(color = "black", dash = "solid", width = 0.85),
            x = ~Day, y = ~value, showlegend = F) %>% 
  add_trace(type = "scatter", mode = "none", fill = "tozeroy",
            data = ins %>% filter(latitude == 65, value >= 450),
            name = "Summer energy",
            fillcolor = col65N, 
            x = ~Day, y = ~value,
            showlegend = F) %>% 
  add_trace(type = "scatter", mode = "lines", 
            x = c(0, 365), y = c(450, 450), showlegend = F, 
            line = list(color = "black", dash = "dashdot", width = 0.85)) %>%
  layout(xaxis = xax, yaxis = yax)

# Daily insolation plot for 35°N at 250 W/m^2 threshold
p35_250 <- plot_ly() %>%
  add_trace(type = "scatter", mode = "lines",
            data = ins %>% filter(latitude == 35), 
            name = "Yearly insolation (35°N)",
            line = list(color = "black", width = 0.85),
            x = ~Day, y = ~value,
            showlegend = F) %>% 
  add_trace(type = "scatter", mode = "none", fill = "tozeroy",
            data = ins %>% filter(latitude == 35, value >= 250),
            name = "35°N",
            showlegend = F,
            fillcolor = col35N,
            x = ~Day, y = ~value) %>% 
  add_trace(type = "scatter", mode = "lines", 
            x = c(0, 365), y = c(250, 250), showlegend = F, 
            line = list(color = "black", dash = "dashdot", width = 0.85)) %>%
  layout(xaxis = xax, yaxis = yax)

# Daily insolation plot for 35°N at 450 W/m^2 threshold
p35_450 <- plot_ly() %>%
  add_trace(type = "scatter", mode = "lines",
            data = ins %>% filter(latitude == 35), 
            name = "Yearly insolation (35°N)",
            line = list(color = "black", dash = "solid", width = 0.85),
            x = ~Day, y = ~value,
            showlegend = F) %>% 
  add_trace(type = "scatter", mode = "none", fill = "tozeroy",
            data = ins %>% filter(latitude == 35, value >= 450),
            name = "Summer energy",
            fillcolor = col35N,
            x = ~Day, y = ~value, 
            showlegend = F) %>% 
  add_trace(type = "scatter", mode = "lines", 
            x = c(0, 365), y = c(450, 450), showlegend = F, 
            line = list(color = "black", dash = "dashdot", width = 0.85)) %>%
  layout(xaxis = xax, yaxis = yax, margin = list(b = 40))

# Combine daily insolation plots.
p_se <- subplot(p65_250, p35_250, p65_450, p35_450,
                nrows = 2,
                titleX = T, shareX = T, shareY = T,
                margin = 0.01,
                heights = c(0.5, 0.5)) 

###################
# TIME SERIES PLOTS
###################

# Load the compiled data, from which we can subset the relevant 
# summer energy time series.
summer.energy <- readRDS("analysis/compiled_data.RData")  %>% 
  filter(latitude %in% c(65, 35), threshold %in% c(250, 450)) %>%
  select(Age, SummerEnergy, threshold, latitude) %>%
  reshape2::melt(id.vars = c("Age", "threshold", "latitude"))

# Time series plot for 65°N
p_ts_65 <- plot_ly() %>% 
  add_trace(type = "scatter", mode = "lines", data = summer.energy %>% 
              filter(threshold == 250, latitude == 65), 
            x = ~Age, y = ~value, 
            name = "Summer energy at 65°N, 250 W/m<sup>2</sup> threshold",
            line = list(color = col65N, width = 0.85),
            showlegend = F) %>%
  add_trace(type = "scatter", mode = "lines", data = summer.energy %>% 
              filter(threshold == 450, latitude == 65), 
            x = ~Age, y = ~value, 
            name = "Summer energy at 65°, 450 W/m<sup>2</sup> threshold",
            line = list(color = col65N, width = 0.85),
            showlegend = F) %>%
  layout(xaxis = list(title = "Age (kyr BP)", 
                      autorange = "reversed",
                      zeroline = F),
         yaxis = list(title = "Summer energy<br>(giga-Joules/m<sup>2</sup>)",
                      range = c(0, 10),
                      zeroline = F))

# Time series plot for 35°N
p_ts_35 <- plot_ly() %>% 
  add_trace(type = "scatter", mode = "lines", data = summer.energy %>% 
              filter(threshold == 250, latitude == 35), 
            x = ~Age, y = ~value, 
            name = "Summer energy at 35°, 250 W/m<sup>2</sup> threshold",
            line = list(color = col35N, width = 0.85),
            showlegend = F) %>%
  add_trace(type = "scatter", mode = "lines", data = summer.energy %>% 
              filter(threshold == 450, latitude == 35), 
            x = ~Age, y = ~value, 
            name = "Summer energy at 35°, 450 W/m<sup>2</sup> threshold",
            line = list(color = col35N, width = 0.85),
            showlegend = F) %>%
  layout(xaxis = list(title = "Age (kyr BP)", 
                      autorange = "reversed",
                      zeroline = F),
         yaxis = list(title = "Summer energy<br>(giga-Joules/m<sup>2</sup>)",
                      range = c(0, 10),
                      zeroline = F))

# Combine time series plots
p_ts <- subplot(p_ts_65, p_ts_35, nrows = 1, titleX = T, shareY = T)

######################################
# ASSEMBLE SUBPLOTS. THIS IS FIGURE 1.
######################################
fig1 <- subplot(p_se, plotly::plotly_empty(), p_ts, nrows = 3, 
        titleX = T, titleY = T,
        heights = c(0.55, 0.025, 0.40)) %>%
  # Time series annotations for 65°N
  add_annotations(xref = "x1", xanchor = "center", x = 170,
                  yref = "y4", yanchor = "center", y = 5.5,
                  text = "250 W/m<sup>2</sup> threshold", 
                  showarrow = T, ax = 0, ay = -30,
                  font = list(size = 12)) %>%
  add_annotations(xref = "x1", xanchor = "center", x = 180,
                  yref = "y4", yanchor = "center", y = 1.35,
                  text = "450 W/m<sup>2</sup><br>threshold", 
                  showarrow = T, ax = 10, ay = 30,
                  font = list(size = 12)) %>%
  
  # Time series annotations for 35°N
  add_annotations(xref = "x2", xanchor = "center", x = 170,
                yref = "y4", yanchor = "center", y = 9,
                text = "250 W/m<sup>2</sup> threshold",
                showarrow = T, ax = 0, ay = 30,
                font = list(size = 12))  %>%
  add_annotations(xref = "x2", xanchor = "center", x = 170,
                  yref = "y4", yanchor = "center", y = 2.5,
                  text = "450 W/m<sup>2</sup><br>threshold",
                  showarrow = T, ax = 0, ay = 30,
                  font = list(size = 12)) %>%
  
  # Threshold annotations for 65N
  add_annotations(xref = "paper", xanchor = "center", x = 0.23,
                  yref = "y1", yanchor = "bottom", y = 500,
                  text = "450 W/m<sup>2</sup> threshold",
                  showarrow = F,
                  font = list(size = 12)) %>%
  add_annotations(xref = "paper", xanchor = "center", x = 1 - 0.23,
                  yref = "y1", yanchor = "bottom", y = 500,
                  text = "450 W/m<sup>2</sup> threshold",
                  showarrow = F,
                  font = list(size = 12)) %>%
  add_annotations(xref = "paper", xanchor = "center", x = 0.23,
                  yref = "y2", yanchor = "bottom", y = 500,
                  text = "250 W/m<sup>2</sup> threshold",
                  showarrow = F,
                  font = list(size = 12)) %>%
  add_annotations(xref = "paper", xanchor = "center", x = 1 - 0.23,
                  yref = "y2", yanchor = "bottom", y = 500,
                  text = "250 W/m<sup>2</sup> threshold",
                  showarrow = F,
                  font = list(size = 12)) %>%
  
  # Latitudes
  add_annotations(xref = "paper", xanchor = "center", x = 0.23,
                  yref = "y2", yanchor = "bottom", y = 545,
                  bgcolor = col65N,
                  text = "               <b>65°N</b>               ",
                  showarrow = F,
                  font = list(size = 16, color = toRGB("white"))) %>%
  add_annotations(xref = "paper", xanchor = "center", x = 1 - 0.25,
                  yref = "y2", yanchor = "bottom", y = 545,
                  bgcolor = col35N,
                  text = "               <b>35°N</b>               ",
                  showarrow = F,
                  font = list(size = 16, color = toRGB("white"))) %>%
  
  # Subplot annotations
  add_annotations(xref = "paper", xanchor = "left", x = -0.12,
                  yref = "y2", yanchor = "center", y = 500,
                  bgcolor = toRGB("white"),
                  text = "<b>A</b>",
                  showarrow = F,
                  font = list(size = 20)) %>%
  add_annotations(xref = "paper", xanchor = "left", x = -0.12,
                  yref = "y", yanchor = "center", y = 500,
                  bgcolor = toRGB("white"),
                  text = "<b>B</b>",
                  showarrow = F,
                  font = list(size = 20)) %>%
  add_annotations(xref = "paper", xanchor = "left", x = -0.12,
                  yref = "y4", yanchor = "center", y = 10,
                  bgcolor = toRGB("white"),
                  text = "<b>C</b>",
                  showarrow = F,
                  font = list(size = 20)) %>%
 
  # Upper panels x axis title (need hack here because Plotly's automatic scaling
  # introduces way too much white space between the axis and the axis title.)
  add_annotations(xref = "x1", xanchor = "center", x = 182.5,
                  yref = "paper", yanchor = "top", y = 0.44,
                  text = "Day of the year",
                  showarrow = F,
                  font = list(size = 14)) %>%
  add_annotations(xref = "x2", xanchor = "center", x = 182.5,
                  yref = "paper", yanchor = "top", y = 0.44,
                  text = "Day of the year",
                  showarrow = F,
                  font = list(size = 14)) %>%
  
  # Layout
  layout(legend = list(xref = "paper", x = 0.5, xanchor = "center",
                       yref = "paper", y = 1.03, yanchor = "center",
                       orientation = "h"),
         margin = list(r = 20, t = 0),
         autosize = F, width = 550, height = 1000) %>%
  config(displayModeBar = F)

# Display the figure.
fig1