# Measure and graph whales:
source("functions.R")


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
  ratio.HF = HF.px/TL.px # ratio using nose- flipper measure
)


#estimate whale size-----
library(ggplot2)

ggplot(data = morpho.output, aes(x = TL.m, y = ratio.HD))+
  geom_point()

ggplot(data = morpho.output, aes(x = TL.m, y = ratio.HF))+
  geom_point()
