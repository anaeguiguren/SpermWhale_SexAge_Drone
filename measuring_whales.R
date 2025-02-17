# Measure and graph whales:
source("R/functions.R")

#1. read in morphometrix data -----
morpho.output <- getMorphoMetrix(ROOTfolderpath = "D:/Gal2023_Drone/Galapagos2023_Drone_Snapshots/SpermWhale_AgeSex_Snapshots/")



#2. compute whale sizes -----

#add launch height to barometer altitude:

morpho.output<- morpho.output %>% mutate(
  altitude.fix = altitudeASL(altitude.raw = altitude.raw), #add launch height to barometer altitude:
  TL.m = measureWhales(image.width = 1920, altitude = altitudeASL(altitude.raw = altitude.raw),length.pixels = TL.px), #estimate length in meters
  HD.m = measureWhales(image.width = 1920, altitude = altitudeASL(altitude.raw = altitude.raw),length.pixels = HD.px), #estimate length in meters
  HF.m = measureWhales(image.width = 1920, altitude = altitudeASL(altitude.raw = altitude.raw),length.pixels = HF.px),
  ratio.HD = HD.px/TL.px, # ratio using nose-dorsal fin measure
  ratio.HF = HF.px/TL.px, # ratio using nose- flipper measure
  ratio.DF = HF.px/HD.px # ratio nose-flipper to nose DF
)

# 3. assign whales ID's based on flight and ID number ----
morpho.output<-morpho.output%>% mutate(
  video.file = substr(imageName, 1, 32)
)

morpho.output <- morpho.output %>% mutate(
  video.ID <- paste(video.file, ind, sep = "_")
)









#estimate whale size-----
library(ggplot2)

ggplot(data = morpho.output, aes(x = TL.m, y = ratio.HD))+
  geom_point()

ggplot(data = morpho.output, aes(x = TL.m, y = ratio.HF))+
  geom_point()





ggplot(data = morpho.output, aes(x = TL.m, y = ratio.DF))+
  geom_point()












#----mixture model example:----
library(mclust)

# Generate a synthetic dataset with three clusters
set.seed(123)
data <- rbind(matrix(rnorm(100, mean = 0, sd = 1), ncol = 2),
              matrix(rnorm(100, mean = 5, sd = 1), ncol = 2),
              matrix(rnorm(100, mean = 10, sd = 1), ncol = 2))



data <- cbind(morpho.output$TL.m, morpho.output$ratio.HD)

data <- data[complete.cases(data),]

# Perform GMM clustering
# G represents the number of clusters
gmm_model <- Mclust(data =data, G = 3);summary(gmm_model)  

# Get the cluster assignments
cluster_assignments <- predict(gmm_model)$classification

# Visualize the results
plot(data, col = cluster_assignments, main = "GMM Clustering Results")
points(gmm_model$parameters$mean, col = 1:3, pch = 8, cex = 2)

