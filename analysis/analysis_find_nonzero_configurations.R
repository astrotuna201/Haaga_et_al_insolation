require(dplyr)
require(data.table)

# Read the summer energy data (must have been precomputed by running the
# "function/preprocess.R" script.)
summer.energy.data = readRDS("analysis/compiled_data.RData")

latitudes = seq(-90, 90, 1)
bin.sizes = 1
thresholds = seq(0, 500, 25)

# List to information about all latitude-threshold configurations
configs = list()

# What fraction of zeros can be tolerated? Set to 2% to allow embedding 
# dimension routines to finish successfully (fails with time series with too 
# many identical values).
cutoff = 0.02

for (l in latitudes) {
  for (t in thresholds) {
    df = subset(x = summer.energy.data,
                subset = (threshold == t & latitude == l),
                select = c("SummerEnergy")) %>% as.data.frame
    
    n.zeros = nrow(dplyr::filter(df, SummerEnergy == 0))
    if (n.zeros > 0) {
      if (n.zeros/nrow(df) >= cutoff) {
        cat("latitude ", l, " at threshold", t, 
            " yields too many (", n.zeros, ") zeros.\n")
      }
    } 
    configs[[paste(l, t)]] = c("latitude" = l, 
                              "threshold" = t, 
                              "cutoff" = cutoff, 
                              "n.zeros" = n.zeros, 
                              "fraction.of.zeros" = n.zeros/nrow(df))
  }
}

# Bind dataframes vertically into one data frame
configs = do.call("rbind", configs) %>% as.data.frame

# Melt the data frame (convert to long format) and convert to matrix.
fraction_of_zeros = melt(configs, id.vars = c("latitude", "threshold"), 
               measure.vars = "fraction.of.zeros")[, c(1, 2, 4)] %>% as.matrix()

# Save to disk, so we don't have to re-create the data later on.
saveRDS(fraction_of_zeros, "analysis/fraction_of_zeros.RData")
