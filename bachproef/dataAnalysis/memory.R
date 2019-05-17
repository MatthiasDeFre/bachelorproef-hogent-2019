library(tidyr)
library(dplyr)
library(ggplot2)
library(plyr)

# Script to check the influence and evolution of RAM 


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

# Check how much the memory has increased or decreased when value is negative 
# First second will always have a big value because it went from no memory to max memory 
vuforiaMerged <- mutate(vuforiaMerged, AllocatedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(AllocatedMemory)), AllocatedMemory - lag(AllocatedMemory),AllocatedMemory))
vuforiaMerged <- mutate(vuforiaMerged, ReservedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(ReservedMemory)), ReservedMemory - lag(ReservedMemory),ReservedMemory))

# Add fps drops
vuforiaMerged <- mutate(vuforiaMerged, FpsDecrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(Fps)), lag(Fps) - Fps,Fps))

# Reserved => Allocated correlation
cor(x = vuforiaMerged$ReservedMemory, vuforiaMerged$AllocatedMemory)
cor(x = vuforiaMerged$ReservedMemory, vuforiaMerged$AllocatedMemoryIncrease)
cor(x = vuforiaMerged$ReservedMemoryIncrease, vuforiaMerged$AllocatedMemory)
cor(x = vuforiaMerged$ReservedMemoryIncrease, vuforiaMerged$AllocatedMemoryIncrease)

# Correlation Memory => Fps
# Reserved Memory
cor(x = arcoreMerged$ReservedMemory, arcoreMerged$Fps)
cor(x = arcoreMerged$ReservedMemory, arcoreMerged$FpsDecrease)
cor(x = arcoreMerged$ReservedMemoryIncrease, arcoreMerged$Fps)
cor(x = arcoreMerged$ReservedMemoryIncrease, arcoreMerged$FpsDecrease)

# Allocated Memory
cor(x = vuforiaMerged$AllocatedMemory, vuforiaMerged$Fps)
cor(x = vuforiaMerged$AllocatedMemory, vuforiaMerged$FpsDecrease)
cor(x= vuforiaMerged$AllocatedMemoryIncrease, vuforiaMerged$Fps)
cor(x= vuforiaMerged$AllocatedMemoryIncrease, vuforiaMerged$FpsDecrease)
cor(x = vuforiaMerged$ReservedMemory, vuforiaMerged$Fps)

# ARCore
arcoreMerged <- multmerge(paste(datalocation, "arcoreData", sep = "/", collapse = NULL))

# Add the row number so we can check which experiment a certain file belongs to
arcoreMerged$RowNumber <- seq.int(nrow(arcoreMerged)) 
arcoreMerged$ExperimentNumber <- paste("Experiment", ceiling(arcoreMerged$RowNumber / 300), sep = " ")

# Check how much the memory has increased or decreased when value is negative 
# First second will always have a big value because it went from no memory to max memory 
arcoreMerged <- mutate(arcoreMerged, AllocatedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(AllocatedMemory)), AllocatedMemory - lag(AllocatedMemory),AllocatedMemory))
arcoreMerged <- mutate(arcoreMerged, ReservedMemoryIncrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(ReservedMemory)), ReservedMemory - lag(ReservedMemory),ReservedMemory))

# Add fps drops
arcoreMerged <- mutate(arcoreMerged, FpsDecrease = ifelse(ExperimentNumber == lag(ExperimentNumber) & !is.na(lag(Fps)), lag(Fps) - Fps,Fps))

# Reserved => Allocated correlation
cor(x = arcoreMerged$ReservedMemory, arcoreMerged$AllocatedMemory)
cor(x = arcoreMerged$ReservedMemory, arcoreMerged$AllocatedMemoryIncrease)
cor(x = arcoreMerged$ReservedMemoryIncrease, arcoreMerged$AllocatedMemory)
cor(x = arcoreMerged$ReservedMemoryIncrease, arcoreMerged$AllocatedMemoryIncrease)

# Correlation Memory => Fps
# Reserved Memory
cor(x = arcoreMerged$ReservedMemory, arcoreMerged$Fps)
cor(x = arcoreMerged$ReservedMemory, arcoreMerged$FpsDecrease)
cor(x = arcoreMerged$ReservedMemoryIncrease, arcoreMerged$Fps)
cor(x = arcoreMerged$ReservedMemoryIncrease, arcoreMerged$FpsDecrease)

# Allocated Memory
cor(x = arcoreMerged$AllocatedMemory, arcoreMerged$Fps)
cor(x = arcoreMerged$AllocatedMemory, arcoreMerged$FpsDecrease)
cor(x= arcoreMerged$AllocatedMemoryIncrease, arcoreMerged$Fps)
cor(x= arcoreMerged$AllocatedMemoryIncrease, arcoreMerged$FpsDecrease)
cor(x = arcoreMerged$ReservedMemory, arcoreMerged$Fps)

# Vuforia Vs ARCore memory

sd(vuforiaMerged$ReservedMemory)
sd(arcoreMerged$ReservedMemory)
mean(vuforiaMerged$ReservedMemory)
mean(arcoreMerged$ReservedMemory)

sd(vuforiaMerged$AllocatedMemory)
sd(arcoreMerged$AllocatedMemory)
mean(vuforiaMerged$AllocatedMemory)
mean(arcoreMerged$AllocatedMemory)

# We can see that Vuforia uses more total RAM, we can also prove this by doing the t-test
vuforiaMerged[, "Framework"] <- 0
arcoreMerged[, "Framework"] <- 1
frameworkMerged <- plyr::rbind.fill(vuforiaMerged, arcoreMerged)
t.test(AllocatedMemory ~Framework, data=frameworkMerged, alternative="less")
