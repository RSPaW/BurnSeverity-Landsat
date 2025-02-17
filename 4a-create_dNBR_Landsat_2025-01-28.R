library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(here)
library(doParallel)

lshp <- list.files(here::here("inputs", "shpByBurn"), pattern = "shp$")
#shp <- st_read(lshp[1], stringsAsFactors = FALSE)

burns.f <- list.dirs(here("all_rgbs"), recursive = FALSE, full.names = FALSE)
burns.f <- burns.f[str_detect(burns.f, "rgb_")]
burns <- str_split_fixed(burns.f, "_", 2)[,2]

dates <- read_csv(here("inputs", "clean_dates_edited.csv"))%>%
  rename(start = date, end = date_end)

# burns <- "PHL-2000-14611385"

i <- 1
#Define how many cores (memory is limiting factor here)
UseCores <- 10
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
foreach(i = 1:length(burns)) %dopar% {
  library(raster)
  library(tidyverse)
  library(sf)
  library(lubridate)
  library(here)
  
  ply <- st_read(here::here("inputs", "shpByBurn", paste0(burns[i], "_boundry.shp")), 
                 quiet = TRUE)
  ply <- st_buffer(ply, dist = 240)
  
  date <- filter(dates, BURNID == burns[i])
  
  fstart <- parse_date_time(date$start[1], c("ymd", "dmy"))
  fend <- parse_date_time(date$end[1], c("ymd", "dmy"))

  plist <- as.data.frame(list.files(here("all_rgbs", paste0("rgb_",burns[i])), "png"))
  colnames(plist)[1] <- "file" 
  
  if(nrow(plist)!=0){
    
    write_csv(plist, here("all_rgbs", paste0("rgb_",burns[i]), "cleanDates.csv"))
    plist <- mutate(plist, date = ymd(str_split_fixed(file, "_", 3)[,2]))
    date.list <- plist$date
  
    tlist <- as.data.frame(list.files(here("tifs", burns[i])))
    colnames(tlist)[1] <- "file" 
    tlist <- tlist %>% mutate(date = ymd(str_split_fixed(file, "_", 3)[,2])) %>%
      filter(date %in% date.list)
  
  #pre image nbr
  preDates <- filter(tlist, date < fstart)
  if (nrow(preDates)==0){
    preIm <- raster(here("tifs", burns[i], tlist$file[1])) 
    cat(burns[i], "has no pre burn image\n")
  }else{
    preDate <- preDates[nrow(preDates),]
    preIm <- raster(here("tifs", burns[i], preDate$file[1]))

    #clean up in case of Landsat 7
      preIm[preIm > 1] <- NA
      preIm[preIm == 0] <- NA
      preIm[preIm < -1] <- NA
      
      if (nrow(preDates)>1){
      #clean up in case of Landsat 7
      preDate2 <- preDates[nrow(preDates)-1,]
      preIm2 <- raster(here("tifs", burns[i], preDate2$file[1]))
      preIm2[preIm2 > 1] <- NA
      preIm2[preIm2 == 0] <- NA
      preIm2[preIm2 < -1] <- NA
      
      preStk <- stack(preIm, preIm2)
      preIm <- calc(preStk, min)
      }
      preDate$type <- "pre"
      imUsed <- preDate
  }
 plot(preIm)
  
  preIm10 <- preIm
  plot(preIm10)
  
  #post image nbr
  postDates <- filter(tlist, date >= fstart & date <= fend)
  postDates.1 <- filter(tlist, date > fend)
  if (nrow(postDates) <= 1){
    postDates <-  bind_rows(postDates, postDates.1[1,]) %>%
      na.omit()
  }

  if (nrow(postDates) != 0){
    postDate <- postDates[1,]
    im <- paste0(here("tifs", burns[i], postDate$file[1]))
    
    postIm20 <- brick(im)
 
    #clean up in case of Landsat 7
      postIm20[postIm20 > 1] <- NA
      postIm20[postIm20 < -1] <- NA
      postIm20[postIm20 == 0] <- NA
      plot(postIm20)
    
    j <- 2
    if (nrow(postDates) > 1){
      for(j in 2:nrow(postDates)){
        im <- paste0(here("tifs", burns[i], postDates$file[j]))
        
        postIm20.j <- brick(im)
        
        #clean up in case of Landsat 7
          postIm20.j[postIm20.j > 1] <- NA
          postIm20.j[postIm20.j < -1] <- NA
          postIm20.j[postIm20.j == 0] <- NA
          plot(postIm20.j)

        postIm20 <- stack(postIm20.j, postIm20)
      }
    }
    #plot(postIm20)
    postNBRmin <- calc(postIm20, min)
    
    plot(postNBRmin)
    
    dNBRmax <- preIm - postNBRmin
    plot(dNBRmax, main = burns[i])
    
    names(dNBRmax) <- "Index"
    plot(dNBRmax)
    dir.create(here::here("dNBR"), showWarnings = FALSE)
    writeRaster(dNBRmax, here::here("dNBR", paste0(burns[i],"_dNBR.tiff")), overwrite=TRUE)  
    postDates$type <- "post"
    imUsed <- bind_rows(imUsed, postDates) 
    write.csv(imUsed, here( "tifs", burns[i], "imgUsed.csv"))
    #plot(stk)
  }else{
    cat(burns[i], "has no post burn image\n")
  }
  
}
}

stopCluster(cl)
