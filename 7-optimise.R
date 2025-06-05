library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(here)
library(doParallel)

dir.create(here("bufferStats", "allPre"), showWarnings = FALSE)

burns.f <- list.dirs(here("all_rgbs"), recursive = FALSE, full.names = FALSE)
burns.f <- burns.f[str_detect(burns.f, "rgb_")]
burns <- str_split_fixed(burns.f, "_", 2)[,2]

dates <- read_csv(here("inputs", "clean_dates_edited.csv"))

w.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\sw_woodyVeg", pattern = "tif$")

shpx <- st_read("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Historical\\xIndex\\DBCA_FireHistory_1987to2017_Id.shp", 
                quiet = TRUE) %>%
  st_transform("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

#burns <- "WEL-2002-89608224"
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
  library(fasterize)
  library(lwgeom)
  library(here)
  
#for(i in 1:length(burns)){
  df.all <- data.frame()
  ply <- st_read(here::here("inputs", "shpByBurn", paste0(burns[i], "_boundry.shp")), 
                 quiet = TRUE)

  ply <- st_buffer(ply, dist = 150)
  
  date <- filter(dates, BURNID == burns[i])
  
  fstart <- parse_date_time(date$date[1], c("ymd", "dmy"))
  fend <- parse_date_time(date$date_end[1], c("ymd", "dmy"))
  
  plist <- as.data.frame(list.files(here("all_rgbs", paste0("rgb_",burns[i])), "png"))
  colnames(plist)[1] <- "file" 
  
  if(nrow(plist)!=0){
    
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
    }#else{
    d <- 1
    for(d in 1:nrow(preDates)){
      preDate <- preDates[d,]
      preIm <- raster(here("tifs", burns[i], preDate$file[1]))
    #}
    plot(preIm)
    
    preIm10 <- mask(crop(preIm, ply), mask = ply)
    preIm10[preIm10 > 1] <- NA
    preIm10[preIm10 == 0] <- NA
    preIm10[preIm10 < -1] <- NA
    plot(preIm10)
    
    #post image nbr
    postDates <- filter(tlist, date >= fstart & date <= fend)
    postDates.1 <- filter(tlist, date > fend)
    if (nrow(postDates) <= 1){
      postDates <-  bind_rows(postDates, postDates.1[1,]) %>%
        na.omit()
    }
    
    
    if (nrow(postDates) != 0){
      
      #im <- paste0(here("tifs", burns[i], postDate$file[1]))
      ims <- stack(here("tifs", burns[i], postDates$file))
      #plot(ims)
      postIm20 <- mask(crop(ims, ply), mask = ply)
      
      postIm20[postIm20 > 1] <- NA
      postIm20[postIm20 < -1] <- NA
      postIm20[postIm20 == 0] <- NA
      
      postNBRmin <- calc(postIm20, min)
      
      plot(postNBRmin)
      
      dNBRmax <- preIm - postNBRmin
      plot(dNBRmax, main = burns[i])
      
      names(dNBRmax) <- "Index"
      
      ########################################
      # 4b-unburnt code
      burn.shp <- st_read(here::here("inputs", "shpByBurn", paste0(burns[i], "_boundry.shp")), 
                          quiet = TRUE)%>%
        st_make_valid()
      yr <- year(burn.shp$date)
      burn.shpx <- filter(shpx, FIH_YEAR1 == yr | FIH_YEAR1 == yr-1 | FIH_YEAR1 == yr+1)
      
      ## updated code (14/09/2022) to merge the shpx file with all burn polys with the current burn poly
      ## also change the input to shpx 
      burn.shpx <- filter(burn.shpx, BURNID != burns[i]) %>%
        dplyr::select(BURNID)
      burn.shpx <- rbind(burn.shpx, st_transform(dplyr::select(burn.shp, BURNID), crs(burn.shpx) )) %>%
        st_cast("MULTIPOLYGON")
      
      if (dir.exists(here("notVeg"))){
        noVeg.shps.list <- list.files(here("notVeg"), pattern = "shp$", full.names = TRUE)
        noVeg.shps <- st_read(noVeg.shps.list[1], quiet = TRUE)
        
        if(("BURNID" %in% colnames(noVeg.shps))==FALSE){
          noVeg.shps$BURNID <- burns[i]
        }
        
        if(burns[i] %in% noVeg.shps$BURNID){
          noVeg.shps <- filter(noVeg.shps, BURNID == burns[i])
        }else{
          noVeg.shps <- noVeg.shps[0,] 
          
        }
        
        burn.shpx <- rbind(burn.shpx, st_transform(dplyr::select(noVeg.shps, BURNID), crs(burn.shpx) )) %>%
          st_cast("MULTIPOLYGON")
      }
      
      
      
      burn.shpx$n <- 2
      dnbr <- dNBRmax#raster(here("dNBR", t.burn$tif[1]))
      plot(dnbr)
      burn.rst <- fasterize(burn.shpx, dnbr, field = "n")
      
      dnbr.mask <- dnbr
      dnbr.mask[is.na(dnbr.mask) == FALSE] <- 1
      plot(dnbr.mask)
  
      burn.rst[is.na(burn.rst)] <- 1
      burn.rst[burn.rst == 2] <- NA
      plot(burn.rst)
      
      dnbr.ub <- dnbr * burn.rst
      
      w.i <- w.lst[str_detect(w.lst, as.character(yr))]
      y <- 1
      while (length(w.i) == 0){
        w.i <- w.lst[str_detect(w.lst, as.character(yr+y))]
        y <- y+1
      }
      rst.per <- raster(paste0("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\sw_woodyVeg\\", w.i[1]))
      per.i <- crop(rst.per, st_transform(st_buffer(burn.shp, 150), crs(rst.per)))
      
      plot(per.i)
      
      per.i <- projectRaster(per.i, dnbr, method = "ngb")
      per.i[per.i >= 1] <- 1
      
      per.i <- per.i * dnbr.mask
      
      burn.rst <- burn.rst * per.i
      
      dnbr.ub <- dnbr * burn.rst

      plot(dnbr.ub)
        
      q <- quantile(dnbr.ub, probs = seq(0, 1, 0.10))
        df <- as.data.frame(as.numeric(q[10]))
        colnames(df)[1] <- "threshold"
        df$date <- str_split_fixed(preDate$file[1], "_", 3)[,2]
        df$BURNID <- burns[i]
        df.all <- bind_rows(df, df.all)
        cat(as.numeric(q[10]), "\n") 
      }
  }
  }
 df.all$date <- ymd(df.all$date) 
 ggplot(df.all, aes(date, threshold))+
  geom_point()+
  geom_line()+
   labs(title = paste0(burns[i], ": min = ", round(min(df.all$threshold), 3), " at ",df.all$date[which(df.all$threshold == min(df.all$threshold))]))+
   coord_cartesian(ylim = c(0, 0.3))+
  theme_bw() 
 ggsave(here("bufferStats", "allPre", paste0("allPre_", burns[i], ".jpg")), width = 5, height = 3.5)
}
stopCluster(cl)



