
# 1. reading in data ------
#extracts raw data from Morphometrix generated csvs. 
getMorphoMetrix  <- function(ROOTfolderpath){
  require(dbplyr); require(dplyr)
  #' @title getMorphoMetrix
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
  morpho.output <- data.frame(textfile = character(), 
                              imagePath = character(), 
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
    
    
    object_counts <- table(a$Object, a$Value_unit)
    
    if (any(object_counts>1)){
      stop(paste("A variable has been entered twice in file",
                 basename(tmp.files[i])))
    }
    

    
    tmp.table <- data.frame(textfile = basename(tmp.files[i]),
                            imageName = basename(a$Value[which(a$Object=="Image Path")]), 
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

#2. obtaining image width from original images ----
getImageWidth <- function(ROOTfolderpath, data){
  require(exiftoolr)
  #' @title getImageWidth
  #' @description extract image width from original snapshots
  #' @param ROOTfolderpath 	a character vector of full path names to the folder where the csv outputs are located  
  #' @param data 	a data.frame output from getMorphoMetrix 
  
  
  #ROOTfolderpath <- "D:/Gal2023_Drone/Galapagos2023_Drone_Snapshots/SpermWhale_AgeSex_Snapshots/"
  #data = morpho.output
  
  FromFiles <- list.files(path = ROOTfolderpath, full.names = TRUE, recursive 
                          = T, pattern = ".png")
  FromFiles<-FromFiles[-grep("measurements", FromFiles)] #keep only original pictures
  FromFiles<-FromFiles[-grep("csv", FromFiles)] #keep only original pictures
  
  #read image sizes: 
  out <- exif_read(FromFiles, 'ImageWidth')
  
  # Identify any missing data
  if (any(is.na(out$ImageWidth))) {
    missing_files <- FromFiles[is.na(out$ImageWidth)]
    cat("Files with missing EXIF data:\n")
    print(missing_files)
  }
  
  # Create the data frame with the available data
  img_widths <- data.frame(
    image_path  = out$SourceFile,
    file_name   = basename(out$SourceFile),
    image_width = out$ImageWidth
  )
  # merge with output from morphometrix:
  
  data$file_name <- basename(data$imageName)
  
  data <-left_join(data, img_widths, by = "file_name", relationship = 'many-to-many')
  
  #cleanup: 
  data <- data %>% select(
    file_name, ind, altitude.raw, TL.px, HF.px, HD.px, image_width, notes, image_path
  )
  
  
  return(data)
  
}


#3. gets altitude directly from srt files ----

getSrtAltitude <- function(data){
  #read in all flight srt data
  require(stringr)
  drone_srt_files <- read.csv("Input_Data/Gal2023_Drone_Flight_Logs_srt.csv", header = T)
  
  
  
  
  #obtain snapshot file type: 
  data <- data %>% 
    mutate(  # identify vlc and other file snapshot types
      type = ifelse(str_detect(file_name, "vlc"), "vlc", "boris")
    )
  #figure out how to estimate these separately!
    
    
    mutate(
    # Extract date and time from the video file name
    date_part = lapply(file_name, function(x){unlist(strsplit(x, "_"))[3]}), 
    time_part = lapply(file_name, function(x){unlist(strsplit(x, "_"))[4]}), 
    ss_part = lapply(file_name, function(x){unlist(strsplit(x, "_"))[5]})
    )
   
  
}



#2. corrects subtitle altitude to true altitude above sea-level (ASL) based on launch height----
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



#apply correction 

#estimate ratios 


#then apply correction factor
