# Measure and graph whales:
source("R/functions.R")

#1. read in morphometrix data -----
ROOTfolderpath <- "D:/Gal2023_Drone/Galapagos2023_Drone_Snapshots/SpermWhale_AgeSex_Snapshots/"
morpho.output <- getMorphoMetrix(ROOTfolderpath)


#2. get image widths ----

morpho.output <- getImageWidth(ROOTfolderpath, data= morpho.output)

write.csv(morpho.output, "Output_Data/morpho.output.csv")


#3. get srt altitudes -----
# It is easy to forget to input the altitude each measurement
# extracting altitudes directly from srt files (read using exif tool)
# minimizes this source of error. 



#2. compute raw whale sizes and ratios -----

#add launch height to barometer altitude:

morpho.output<- morpho.output %>% mutate(
  altitude.fix = altitudeASL(altitude.raw = altitude.raw), #add launch height to barometer altitude:
  TL.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = altitude.raw),length.pixels = TL.px), #estimate length in meters
  HD.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = altitude.raw),length.pixels = HD.px), #estimate length in meters
  HF.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = altitude.raw),length.pixels = HF.px),
  ratio.HD = HD.px/TL.px, # ratio using nose-dorsal fin measure
  ratio.HF = HF.px/TL.px, # ratio using nose- flipper measure
  ratio.DF = HF.px/HD.px # ratio nose-flipper to nose DF
)




# 3. assign whales ID's based on flight and ID number ----



morpho.output<-morpho.output%>% mutate(
  video.file = substr(file_name, 1, 32)
)

morpho.output <- morpho.output %>% mutate(
  video.whale.ID =paste(video.file, ind, sep = "_")
)





#Plot raw whale sizes-----
library(ggplot2)

ggplot(data = morpho.output, aes(x = TL.m, y = ratio.HD))+
  geom_point()
  
  
  #geom_text(aes(label = rownames(morpho.output)), hjust = -0.2, vjust = 0.5)

ggplot(data = morpho.output, aes(x = TL.m, y = ratio.HF))+
  geom_point()+
  geom_text(aes(label = rownames(morpho.output)), hjust = -0.2, vjust = 0.5)


ggplot(data = morpho.output, aes(x = TL.m, y = HF.px/HD.px))+
  geom_point()+
  geom_text(aes(label = rownames(morpho.output)), hjust = -0.2, vjust = 0.5)





#Plot whale sizes by video.whale ID ----


whale_df <- morpho.output %>%
  group_by(video.whale.ID) %>%
  summarize(mean_TL = mean(TL.m), cv_TL = (sd(TL.m)/mean_TL)*100, 
            mean_HD = mean(HD.m), cv_HD = (sd(HD.m)/mean_HD)*100, 
            mean_HF = mean(HF.m), cv_HF = (sd(HF.m)/mean_HF)*100 )


ggplot(morpho.output, aes(video.whale.ID, TL.m)) +
  geom_jitter(position = position_jitter(0.2), color = "darkgray") + 
  geom_pointrange(inherit.aes = F, 
                  size = 0.1,aes(x = video.whale.ID, y = mean_TL, ymin = mean_TL - cv_TL, ymax = mean_TL+cv_TL),
                  data = whale_df)+
  theme_classic()




ggplot(morpho.output, aes(video.whale.ID, ratio.HD)) +
  geom_jitter(position = position_jitter(0.2), color = "darkgray")
  geom_pointrange(inherit.aes = F, 
                  size = 0.1,aes(x = video.whale.ID, y = mean_ratio.HD, ymin = mean_ratio.HD - cv_ratio.HD, ymax = mean_ratio.HD+cv_ratio.HD),
                  data = whale_df)+
  theme_classic()



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

