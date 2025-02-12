


#extracts raw data from Morphometrix generated csvs. 
getMorphoMetrix  <- function(ROOTfolderpath){
  require(dbplyr); require(dplyr)
  #' @title compMorphometrix
  #' @description compile the csv outputs from Morphometrix folder 
  #' @param ROOTfolderpath 	a character vector of full path names to the folder where the csv outputs are located  
  
  
  #remove / from ROOTFfolderpath
  if (substring(ROOTfolderpath, nchar(ROOTfolderpath), nchar(ROOTfolderpath)) == 
      "/") {
    ROOTfolderpath = substring(ROOTfolderpath, 1, nchar(ROOTfolderpath) - 
                                 1)
  }
  
  #folder not found
  if(isFALSE(dir.exists(ROOTfolderpath)))
    stop(paste(ROOTfolderpath, "doesn't exist", sep = ""))
  
  #get list of files
  tmp.files <- list.files(path = ROOTfolderpath, pattern = "*.csv", 
                          full.names = TRUE, recursive = TRUE)
  
  #make empty data table to populate
  #empty flightlog
  morpho.output <- data.frame(imagePath = character(), 
                              videoFile = character(),
                              timeStamp = numeric(),
                              altitude.raw = numeric(),
                              ind = numeric(),
                              TL.px = numeric(),
                              HF.px = numeric(),
                              HD.px = numeric(),
                              notes = character()
  )
  
  # go through all files
  for (i in seq_along(tmp.files)){
    
    #prints progress bar
    cat(paste("\r", round(100 * (i/length(tmp.files)), 1), 
              "% processing, file", i, ":", (tmp.files[i])))
    
    #read in each file
    
    a <- read.csv(tmp.files[i], header = T)
    
    
    #check if HF and fref fields exist:
    HF.exists <- any(a$Object == "HF")
    HD.exists <- any(a$Object == "HD")
    fref.exists <- any(a$Object == "fref")
    
    
    tmp.table <- data.frame(imageName = basename(a$Value[which(a$Object=="Image Path")]), 
                            imagePath = a$Value[which(a$Object=="Image Path")], 
                            altitude.raw = as.numeric(a$Value[which(a$Object=="Altitude")]),
                            ind = as.numeric(a$Value[which(a$Object=="Image ID")]),
                            TL.px = as.numeric(a$Value[which(a$Object=="TL" & a$Value_unit == "Pixels")]),
                            HF.px =  ifelse(HF.exists, as.numeric(a$Value[which(a$Object == "HF" & a$Value_unit == "Pixels")]), NA),
                            HD.px = ifelse(HD.exists, as.numeric(a$Value[which(a$Object == "HD" & a$Value_unit == "Pixels")]), NA),
                            notes = a$Value[which(a$Object=="Notes")])
    
    morpho.output <- rbind(morpho.output, tmp.table)
    
  }
  return(morpho.output)
  
} 





#corrects subtitle altitude to true altitude above sea-level (ASL) based on launch height
altitudeASL <- function(boat_height =  1.03 - 0.24, 
                        launch.chest = 1.4, 
                        camera.height = 0.045, 
                        altitude.raw){
  altitude.raw+launch.chest+camera.height+boat_height
  
}
  
  
#estimates whale length in meters

measureWhales<- function(image.width, altitude, length.pixels){
  alpha = ifelse(image.width == 3840, yes = 0.000328, no = 
                   ifelse(image.width == 1920, yes = 0.000656, no = NA))
  length = alpha * altitude * length.pixels
  return(length)
}


#estimate ratios 


#then apply correction factor
