# Source functions that load the data sets and the data structure which the 
# data sets are loaded into.
source("preprocessing/preprocess_load_datasets.R")
source("preprocessing/preprocess_data_structures.R")

#' Compiles the global climate datasets used in this study.
#' 
#' @param latitude The latitude for which to get the summer energy.
#' @param threshold Summer energy is calculated by summing over daily insolation
#' @param bin.size The temporal resolution of the data. Defaults to 1 kyr.
#'   during days of the year exceeding this threshold.
#' 
#' @return A dataframe containing the compiled and gridded dataset.
CompileGlobalClimateData <- function(latitude,
                                     threshold,
                                     bin.size = 1,
                                     time.sampled.at = "mid",
                                     start = -0.5,
                                     end = 800.5) {
  GSL_speleoice = LoadGSL_speleoice()
  GSL_speleothem = LoadGSL_speleothem()
  GSL_LR04 = LoadGSL_LR04()
  summer.energy = LoadSummerEnergy(latitude = latitude, threshold = threshold)
  
  # Extract age and data from each dataset into separate data frames and 
  # merge them (do a full join where missing values are assigned NAs)
  ds = extract_dataframes(datasets = list(summer.energy, 
                                          GSL_LR04, 
                                          GSL_speleothem, 
                                          GSL_speleoice),
                          age.range = c(start, end)) 
  
  # Arrange in descending order according to the Age column.
  ds = arrange(ds, desc(Age))
  
  # Bin data on 1 kyr grid.
  binned = tstools::bin(dt = ds, bin.size = 1, by = "Age", 
                        interpolate = T, remove.na = T, 
                        start = -0.5, end = 799.5, 
                        time.sampled.at = time.sampled.at)
  binned$latitude = rep(latitude)
  binned$threshold = rep(threshold)
  
  return(binned)
}

#' Extract data frames from Dataset objects within a given age range.
#' @param datasets A list of Dataset objects created by functions
#' in the 'preprocess_load_datasets.R' file. 
#' @param age.range A two-element vector giving the age range.
extract_dataframes <- function(datasets, age.range) {
  dataset.list = lapply(X = datasets,
                        FUN = function(dataset) {
                          dt = cbind(dataset$age, dataset$data)
                          dt = as.data.frame(dt)
                          colnames(dt) = c("Age", dataset$shortname)
                          if (!is.null(age.range)) {
                            dt = dt[dt$Age >= min(age.range) &
                                      dt$Age <= max(age.range), ]
                          }
                          
                          return(dt)
                        }
  )
  compilation = Reduce(function(x, y) full_join(x, y), dataset.list)
  return(compilation)
}
