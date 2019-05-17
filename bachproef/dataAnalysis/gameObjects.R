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

vuforiaMerged <- mutate(vuforiaMerged, AllocatedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(AllocatedMemory)), AllocatedMemory - lag(AllocatedMemory),AllocatedMemory))
vuforiaMerged <- mutate(vuforiaMerged, ReservedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(ReservedMemory)), ReservedMemory - lag(ReservedMemory),ReservedMemory))
vuforiaMerged <- mutate(vuforiaMerged, visibleObjectIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(visibleGameObjectCount)), lag(visibleGameObjectCount) - visibleGameObjectCount,visibleGameObjectCount))


sd(vuforiaMerged$GameObjectCount)
sd(vuforiaMerged$activeGameObjectCount)
sd(vuforiaMerged$visibleGameObjectCount)

cor(vuforiaMerged$visibleGameObjectCount, vuforiaMerged$Fps)
cor(vuforiaMerged$visibleGameObjectCount, vuforiaMerged$AllocatedMemory)
cor(vuforiaMerged$visibleGameObjectCount, vuforiaMerged$ReservedMemory)

cor(vuforiaMerged$visibleGameObjectCount, vuforiaMerged$AllocatedMemory)

# ARCore
arcoreMerged <- multmerge(paste(datalocation, "arcoreData", sep = "/", collapse = NULL))

arcoreMerged <- mutate(arcoreMerged, AllocatedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(AllocatedMemory)), AllocatedMemory - lag(AllocatedMemory),AllocatedMemory))
arcoreMerged <- mutate(arcoreMerged, ReservedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(ReservedMemory)), ReservedMemory - lag(ReservedMemory),ReservedMemory))
arcoreMerged <- mutate(arcoreMerged, ObjectIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(GameObjectCount)), lag(GameObjectCount) - GameObjectCount,GameObjectCount))
arcoreMerged <- mutate(arcoreMerged, visibleObjectIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(visibleGameObjectCount)), lag(visibleGameObjectCount) - visibleGameObjectCount,visibleGameObjectCount))


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

cor(arcoreMerged$ObjectIncrease, arcoreMerged$AllocatedMemoryIncrease)
cor(arcoreMerged$ObjectIncrease, arcoreMerged$AllocatedMemoryIncrease)


ggplot(arcoreMerged[arcoreMerged$ExperimentNumber == "Experiment 4", ], aes(x = Time, y = GameObjectCount)) + 
  geom_line(color = "#00AFBB", size = 0.75)

