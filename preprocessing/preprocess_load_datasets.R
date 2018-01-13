require(dplyr)

#' Load summer energy data for a given latitude and threshold.
#' 
#' @param latitude The latitude for which to sample summer energy
#' @param threshold The threshold over which to sample summer days.
#' @param return_df Return a data frame? If not, returns a Dataset object.
#' @return A Dataset object or a data frame containing the summer energy data 
#'   (in Giga-Joules/m^2)
#'   
LoadSummerEnergy <- function(latitude, 
                             threshold, 
                             return_df = F) {
  path = "data/Integrated_summer_insolation/"
  hemisphere = ifelse(test = latitude < 0, yes = "S", no = "N")
  if (latitude == 0) {
    file = "J_0North.txt"
  } else {
    if (tolower(hemisphere) == "s" || tolower(hemisphere) == "south") {
      file = paste("J_", abs(latitude), "South", ".txt", sep="")
    } else if (tolower(hemisphere) == "n" || tolower(hemisphere) == "north") {
      file = paste("J_", latitude, "North", ".txt", sep="")
    }
  }
  dt = read.table(paste(path, file, sep=""), skip = 8, header = T)
  
  dt = as.data.frame(dt)
  colnames(dt) = c("Age", seq(0, 600, 25))
  dt = dt[rev(dt$Age),]
  
  if (return_df)  return(dt[, c("Age", toString(threshold))])
  
  SummerEnergy = Dataset$new(name = paste("Integrated summer insolation at ", 
                                          latitude, hemisphere, sep=""),
                           shortname = paste("SummerEnergy"),
                           citation = "Generated using Matlab script from Huybers' (2006) Science paper.",
                           age = dt[, "Age"], 
                           age_unit = "kyr BP",
                           age_label = "Age",
                           age_sigma = rep(0, nrow(dt)), 
                           age_sigma_unit = "",
                           data = dt[, toString(threshold)],
                           data_sigma = rep(0, nrow(dt)),
                           data_sigma_unit = NA,
                           data_label= paste("Threshold at ", 
                                             threshold, " W/m^2", sep=""),
                           data_unit = "Giga-Joules/m^2")
  return(SummerEnergy)
}

#' Sea surface temperature proxies.
LoadGSL_LR04 <- function(username = Sys.info()["effective_user"]) {
  file.path = "data/GSL_LR04.txt"
  dt = read.table(file.path, header = T, sep = "\t", 
                  dec = ".", na.strings = "NaN")
  
  dt = select(dt, age_calkaBP, SeaLev_longPC1, SeaLev_longPC1_err_sig)
  colnames(dt) = c("age_mean", "GSL", "GSL_sigma")
  dt = dt[complete.cases(dt), ]
  GSL = Dataset$new(name = "Glocal sea level",
                    shortname = "GSL",
                    citation = "Spratt & Lisiecki, 2016",
                    age = dt$age, 
                    age_unit = "kyr BP",
                    age_label = "Age uncertainties are based on the estimate of 4 ka given in Spratt & Lisiecki (2016)",
                    age_sigma = rep(4, nrow(dt)), 
                    age_sigma_unit = "kyr BP",
                    data = dt$GSL, 
                    data_sigma = dt$GSL_sigma,
                    data_sigma_unit = "1 standard deviation from bootstrap",
                    data_label= "Scaled first principal component of five sea level reconstructions (0-798 ka)",
                    data_unit = "Sea level (m above present day)")
  return(GSL)
}


#' Sea surface temperature proxies.
LoadGSL_speleothem <- function(username = Sys.info()["effective_user"]) {
  file.path = "data/GSL_speleothem.txt"
  dt = data.table::fread(file.path, skip = 1)
  colnames(dt) = c("Age", "GSLspel")
  dt = dt[complete.cases(dt), ]
  GSL = Dataset$new(name = "Glocal sea level stack, speleothem agemodel",
                    shortname = "GSLspel",
                    citation = "Spratt & Lisiecki, 2016 with new agemodel",
                    age = dt$Age, 
                    age_unit = "kyr BP",
                    age_label = "Age uncertainties are based on the estimate of 4 ka given in Spratt & Lisiecki (2016)",
                    age_sigma = rep(0, nrow(dt)), 
                    age_sigma_unit = "kyr BP",
                    data = dt$GSLspel, 
                    data_sigma = rep(0, nrow(dt)),
                    data_sigma_unit = "1 standard deviation from bootstrap",
                    data_label= "Scaled first principal component of five sea level reconstructions (0-798 ka)",
                    data_unit = "Sea level (m above present day)")
  return(GSL)
}

#' Sea surface temperature proxies.
LoadGSL_speleoice <- function(username = Sys.info()["effective_user"]) {
  dt = data.table::fread("data/GSL_speleoice.txt", 
                         sep = "\t", dec = ".")[, 2:3]
  colnames(dt) = c("GSLSpeleoIce", "Age")
  dt = dt[, c("Age", "GSLSpeleoIce")]
  dt = dt[complete.cases(dt), ]
  GSL = Dataset$new(name = "Global sea level on speleoice agemodel",
                    shortname = "GSLSpeleoIce",
                    citation = "Spratt & Lisiecki, 2016",
                    age = dt$Age, 
                    age_unit = "kyr BP",
                    age_label = "",
                    age_sigma = rep(0, nrow(dt)), 
                    age_sigma_unit = "kyr BP",
                    data = dt$GSLSpeleoIce, 
                    data_sigma = rep(0, nrow(dt)),
                    data_sigma_unit = "",
                    data_label= "",
                    data_unit = "")
  return(GSL)
}