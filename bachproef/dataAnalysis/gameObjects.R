# library(tidyr)
library(dplyr)
library(ggplot2)
library(plyr)

# Script to check the influence and evolution of Gameobjects

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

sd(vuforiaMerged$GameObjectCount)
sd(vuforiaMerged$activeGameObjectCount)
sd(vuforiaMerged$visibleGameObjectCount)

cor(vuforiaMerged$visibleGameObjectCount, vuforiaMerged$Fps)
cor(vuforiaMerged$visibleGameObjectCount, vuforiaMerged$AllocatedMemory)
cor(vuforiaMerged$visibleGameObjectCount, vuforiaMerged$ReservedMemory)

# ARCore
arcoreMerged <- multmerge(paste(datalocation, "arcoreData", sep = "/", collapse = NULL))

# Add the row number so we can check which experiment a certain file belongs to
arcoreMerged$RowNumber <- seq.int(nrow(arcoreMerged)) 
arcoreMerged$ExperimentNumber <- paste("Experiment", ceiling(arcoreMerged$RowNumber / 300), sep = " ")

sd(arcoreMerged$GameObjectCount)
sd(arcoreMerged$activeGameObjectCount)
sd(arcoreMerged$visibleGameObjectCount)

# Logical that increasing the game object count adds new active objects, else you would be adding objects without a purpose!
cor(arcoreMerged$GameObjectCount, arcoreMerged$activeGameObjectCount)
cor(arcoreMerged$GameObjectCount, arcoreMerged$visibleGameObjectCount)

cor(arcoreMerged$visibleGameObjectCount, arcoreMerged$Fps)
cor(arcoreMerged$visibleGameObjectCount, arcoreMerged$AllocatedMemory)
cor(arcoreMerged$visibleGameObjectCount, arcoreMerged$ReservedMemory)

ggplot(arcoreMerged[arcoreMerged$ExperimentNumber == "Experiment 4", ], aes(x = Time, y = GameObjectCount)) + 
  geom_line(color = "#00AFBB", size = 2)

