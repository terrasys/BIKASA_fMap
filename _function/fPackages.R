#-----------------------------------------------------------------------------------------------------
print("Load all required packages and dowload/install non-existent packages")
#-----------------------------------------------------------------------------------------------------
loadandinstall <- function(mypkg=c("automap",
                                   "gtools",
                                   "chron",
                                   "ggplot2",
                                   "data.table",
                                   "doSNOW",
                                   "foreign",
                                   "geosphere",
                                   "grid",
                                   "gstat",
                                   "lattice",
                                   "parallel",
                                   "raster",
                                   "RCurl",
                                   "rgdal",
                                   "pheno",
                                   "RSAGA",
                                   "stringr",
                                   "utils",
                                   "sf",
                                   "lubridate",
                                   "RColorBrewer",
                                   "classInt")) {
  print('Loading required R-Packages or installing them, if not already installed')
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
loadandinstall()


featureScale <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
