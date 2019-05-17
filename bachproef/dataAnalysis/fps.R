# Script to check if both have stable fps

# Functions

# Merge all Vuforia files into a single dataframe
# Credit: https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
multmerge = function(mypath){
  filenames <-list.files(path=mypath, full.names=TRUE)
  browser()
  # Only read the first 300 rows of each csv
  datalist <- lapply(filenames, function(x){read.csv(file=x,header=TRUE, sep = ";", nrows = 300)})
  test <-  plyr::rbind.fill(datalist)
  # Merge all dataframes into a single dataframe
  plyr::rbind.fill(datalist)
}


# Sets the working directory to the location of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
datalocation = getwd()

vuforiaMerged <- multmerge(paste(datalocation, "vuforiaData", sep = "/", collapse = NULL))
vuforiaFpsMean <- mean(vuforiaMerged$Fps)
vuforiaFpsStandardDeviation <- sd(vuforiaMerged$Fps)

arcoreMerged <- multmerge(paste(datalocation, "vuforiaData", sep = "/", collapse = NULL))
arcoreFpsMean <- mean(vuforiaMerged$Fps)
arcoreFpsStandardDeviation <- sd(vuforiaMerged$Fps)