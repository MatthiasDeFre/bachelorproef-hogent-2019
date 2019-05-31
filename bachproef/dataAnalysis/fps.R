library(tidyr)
library(dplyr)

library(ggplot2)
library(plyr)
# Script to check if both have stable fps

# Functions

# Merge all Vuforia files into a single dataframe
# Credit: https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
multmerge = function(mypath){
  filenames <-list.files(path=mypath, full.names=TRUE)
  # Only read the first 300 rows of each csv
  datalist <- lapply(filenames, function(x){read.csv(file=x,header=TRUE, sep = ";", nrows = 300)})
  test <-  plyr::rbind.fill(datalist)
  # Merge all dataframes into a single dataframe
  plyr::rbind.fill(datalist)
}


# Sets the working directory to the location of the script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
datalocation = getwd()

# Vuforia
vuforiaMerged <- multmerge(paste(datalocation, "vuforiaData", sep = "/", collapse = NULL))

# Add the row number so we can check which experiment a certain file belongs to
vuforiaMerged$RowNumber <- seq.int(nrow(vuforiaMerged)) 
vuforiaMerged$ExperimentNumber <- paste("Experiment", ceiling(vuforiaMerged$RowNumber / 300), sep = " ")

vuforiaFps <- vuforiaMerged$Fps
vuforiaFpsMean <- mean(vuforiaFps)
vuforiaFpsStandardDeviation <- sd(vuforiaFps)
vuforiaMax <- max(vuforiaFps)
vuforiaMin <- min(vuforiaFps)
boxplot(
  vuforiaFps, 
  main="FPS verdeling voor alle experimenten",
  xlab="FPS",
  horizontal = TRUE
)

# Check for big fps drops
vuforiaIsBigFpsDrop = function(data) {
  if(vuforiaFpsMean - data > 10) return (TRUE) else return(FALSE)
}
vuforiaBigDrops <- Filter(vuforiaIsBigFpsDrop, vuforiaFps);

# Further analysis to find out when the fps drops occur
vuforiaMerged[vuforiaMerged$Fps < vuforiaFpsMean - 10, ]
vuforiaMerged[vuforiaMerged$Fps < vuforiaFpsMean - 5 & vuforiaMerged$Fps >= vuforiaFpsMean - 10, ]

# Plot all experiment to visualize the fps stability
ggplot(vuforiaMerged[vuforiaMerged$ExperimentNumber == "Experiment 5", ], aes(x = Time, y = Fps)) + 
  geom_line(aes(color = ExperimentNumber), size = 0.75) +
  scale_color_manual(values = c("#d30c20")) +
  theme_minimal()

# ARCore

arcoreMerged <- multmerge(paste(datalocation, "arcoreData", sep = "/", collapse = NULL))

# Add the row number so we can check which experiment a certain file belongs to
arcoreMerged$RowNumber <- seq.int(nrow(arcoreMerged)) 
arcoreMerged$ExperimentNumber <- paste("Experiment", ceiling(arcoreMerged$RowNumber / 300), sep = " ")
arcoreFps <- arcoreMerged$Fps
arcoreFpsMean <- mean(arcoreFps)
arcoreFpsStandardDeviation <- sd(arcoreFps)
arcoreaMax <- max(arcoreFps)
arcoreMin <- min(arcoreFps)
boxplot(
  arcoreFps, 
  main="FPS verdeling voor alle experimenten",
  xlab="FPS",
  horizontal = TRUE
)

# Check for big fps drops
arcoreIsBigFpsDrop = function(data) {
  if(arcoreFpsMean - data > 10) return (TRUE) else return(FALSE)
}
arcoreBigDrops <- Filter(arcoreIsBigFpsDrop, arcoreFps);

# Further analysis to find out when the fps drops occur
arcoreMerged[arcoreMerged$Fps < arcoreFpsMean - 10, ]
arcoreMerged[arcoreMerged$Fps < arcoreFpsMean - 5 & arcoreMerged$Fps >= arcoreFpsMean - 10, ]

# Plot all experiment to visualize the fps stability
ggplot(arcoreMerged[arcoreMerged$ExperimentNumber == "Experiment 4", ], aes(x = Time, y = Fps)) + 
  geom_line(aes(color = ExperimentNumber), size = 0.75) +
  scale_color_manual(values = c("#42f465", "#d30c20")) +
  theme_minimal()

