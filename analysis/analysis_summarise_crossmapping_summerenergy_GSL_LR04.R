if (!require("pacman")) install.packages("pacman")
pacman::p_install_gh("kahaaga/tstools")
pacman::p_load(dplyr, dtplyr, data.table)

combos <- expand.grid(threshold = seq(0, 500, 25), latitude = seq(-90, 90, 1))
window.sizes = c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

for (i in 1:nrow(combos)) {
  print(i)
  path <- "results/crossmap_summerenergy_GSL_LR04"
  
  # Proceed only if there are any files corresponding to this 
  # latitude-threshold configuration (there might not be, because some time 
  # series have toomany zeros too be analysed).
  threshold <- combos[i, ]$threshold
  latitude <- combos[i, ]$latitude
  key <- paste("_", threshold, "_", latitude, ".RData", sep = "")
  
  files <- list.files(path = path, 
                      full.names = T, 
                      recursive = F, 
                      include.dirs = F, 
                      pattern = key)
  
  if (length(files) > 0) {
    print(files)
    start.time <- Sys.time()
    dt <- data.table::rbindlist(lapply(files, function(f) readRDS(f)))
    
    summaries <- list()
    for (window.size in window.sizes) {
      cat("\twindow size: ", window.size, "\n")
      
      # Summarise original analyses and surrogate analyses separately.
      summary_orig <- directionalcausaltest(
        dt %>% filter(analysis.type == "original", 
                      lag %in% (-window.size):window.size)) %>% 
        mutate(analysis.type = "original")
      
      summary_surr <- directionalcausaltest(
        dt %>% 
          filter(analysis.type == "surrogate",
                 lag %in% (-window.size):window.size)) %>% 
        mutate(analysis.type = "surrogate")
      
      # Change column names for the surrogate dataset, so we can append it 
      # horisontally to the summary of the original analysis.
      rangemax <- length(colnames(summary_surr)) - 1
      cols <- paste0(colnames(summary_surr)[1:rangemax], "_surr")
      colnames(summary_surr)[1:rangemax] <- cols
      
      # Combine original and surrogate summaries
      summary <- cbind(summary_orig, summary_surr[, 2:rangemax]) %>% 
        mutate(threshold = threshold, 
               latitude = latitude,
               window.size = window.size)
      
      summaries[[toString(window.size)]]  = summary
    }
    
    # Save the summary as an .RData file
    summary_filename <- paste0("SummerEnergyDrivesGSL_LR04_", threshold,
                               "_", latitude, ".RData")
    path <- "results/crossmap_summerenergy_GSL_LR04_summaries/"
    file <- paste0(path, summary_filename)
    print(file)
    saveRDS(data.table::rbindlist(summaries), file)
    end.time <- Sys.time()
    cat("\tElapsed time: ", end.time - start.time, " seconds.\n")
    cat("\n")
  }
}
