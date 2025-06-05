library(raster)
library(tidyverse)
library(sf)
library(lubridate)
library(doParallel)
library(here)

# code includes heath mapping and unburnt correction, rv 21-03-2022

v <- paste0("v", Sys.Date())
#v <- "v2024-11-01"

#mdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\models"
model.date <- "2021-10-14"

mtx <- c(-Inf, 0.3, 1,  
         0.3, 1.3, 2,  
         1.3, 2.1, 3, 
         2.1, 2.7, 4, 
         2.7, Inf, 5)
rclmat <- matrix(mtx, ncol=3, byrow=TRUE)

maxVals <- readRDS(paste0("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Historical\\xModels\\maxVals.sentinel"))

mdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Historical\\xModels\\"
mlist <- as.data.frame(list.files(mdir), stringsAsFactors = FALSE)
colnames(mlist) <- "models"
mlist <- mlist %>% mutate(type = str_split_fixed(models, "\\.", 7)[,1],
                          zone = str_split_fixed(models, "\\.", 7)[,2], 
                          name = str_split_fixed(models, "\\.", 7)[,5],
                          date = str_split_fixed(models, "\\.", 7)[,7], 
                          index = str_split_fixed(models, "\\.", 7)[,6], 
                          sat = str_split_fixed(models, "\\.", 7)[,3]) %>%
  filter(type == "qd" & date == "2021-10-14" & sat == "s2a")

ibra <- st_read(paste0(mdir, "IBRA_wa.shp"), quiet = TRUE) 

#####HEATH######

heath.dir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\heath\\predict\\heath_shp"

heath.list <- list.files(heath.dir, pattern = "shp$")
heath.list <- sort(heath.list, decreasing = TRUE)

heath <- st_read(paste0(heath.dir, "\\", heath.list[1]), quiet = TRUE) %>%
  st_transform("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
heath <- heath[,0]

heath.dir2 <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\heath\\heathFromDistricts"
heath.list2 <- list.files(heath.dir2, pattern = "shp$", full.names = TRUE)

i <- 1
for (i in 1:length(heath.list2)){
  heath.i <- st_read(heath.list2[i], quiet = TRUE) %>%
    st_transform("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
  heath.i <- heath.i[,0]
  heath <- rbind(heath, heath.i)
}
#############################

lshp <- list.files(here::here("inputs", "shpByBurn"), pattern = "shp$")

tlist <- as.data.frame(list.files(here::here("dNBR"), pattern = ".tiff$"))
colnames(tlist) <- "tif"
tlist <- mutate(tlist, Burn.Id = str_split_fixed(tif, "_", 2)[,1])

in.rgb <- str_split_fixed(list.dirs(here("all_rgbs"), 
                                   full.names = FALSE, recursive = FALSE), "_", 2)[,2] 

burns <- tlist$Burn.Id[tlist$Burn.Id %in% in.rgb]
#burns <- "BWD-2010-09548980"

dir.create(here::here(v), showWarnings = FALSE)
dir.create(here::here(v, "severity_geoTifs"), showWarnings = FALSE)
dir.create(here::here(v, "ozcbi_geoTifs"), showWarnings = FALSE)
dir.create(here::here(v, "severity_maps"), showWarnings = FALSE)
dir.create(here::here(v, "severity_stats"), showWarnings = FALSE)
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
  library(RColorBrewer)
  library(colorspace)
  library(segmented)
  library(here)
  library(fasterize)
  #for (i in 1:length(burns)){
  
  bdr.name <- burns[i]
  ply <- st_read(here::here("inputs", "shpByBurn", paste0(bdr.name, "_boundry.shp")), 
                 quiet = TRUE)
  
  dir.create(here::here("models", "ibras"), showWarnings = FALSE, recursive = TRUE)
  if(file.exists(here::here("models", "ibras", ply$BURNID[1])) == FALSE){
    ply.i <- st_intersection(st_transform(ply, crs = st_crs(ibra)), ibra)
    ply.i <- dplyr::select(ply.i, IWA_SUB_NA)
    ply.i$area <- as.numeric(st_area(ply.i))
    ply.i <- arrange(ply.i, desc(area))
    ibra.df <- as.data.frame(ply.i$IWA_SUB_NA[1])
    colnames(ibra.df)[1] <- "ibra"
    ibra.df$BURNID <- ply$BURNID[1]
    
    saveRDS(ibra.df, here::here("models", "ibras", ply$BURNID[1]))
  }else{
    ibra.df <- readRDS(here::here("models", "ibras", ply$BURNID[1]))
  }
  burn.ibra <- ibra.df$ibra[1]
  
  heath <- st_transform(heath, st_crs(ply)) #I just added this line
  heath.i <- heath[unique(unlist(st_intersects( ply, heath))),]
  
  if (burn.ibra == "Dandarragan Plateau"){burn.ibra <- "Dandaragan Plateau"}
  
  b <- filter(tlist, Burn.Id == burns[i])
  rst <- raster(here::here("dNBR", b$tif[1]))
plot(rst)
  df.dist <- data.frame(d1 = c("ALB","BWD", "DON", "FRK", "PHS", "PHS","SWC", "WTN", "WTN"), 
                        d2 = c("ALB","BWD", "DON", "FRK", "PHS", "PHL","SWC", "WTN", "WEL"))
  
  burn.dist <- str_sub(bdr.name, end = 3)
  
  mod <- readRDS(paste0("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\models\\", 
                        "landsat.dNBR.to.sentinel.", df.dist$d1[which(df.dist$d2 ==   burn.dist)]))
  names(rst) <- "landsat"
  rst <- predict(rst, mod)
  
  mx.dNBR <- maxVals$dNBR[which(maxVals$ibra == burn.ibra)]
  rst[rst >=   mx.dNBR] <-   mx.dNBR
  sev <- mask(crop(rst, ply), ply) 
  plot(sev)

  ibra.model <- filter(mlist, name == burn.ibra)
  mod <- readRDS(paste0(mdir, "\\", ibra.model$models[1]))
  mod.index <- str_split_fixed(ibra.model$models[1], "\\.", 7)[6]

    names(rst)[1] <- "index"
    
    rst.p <- predict(rst, mod)
    rst.ozcbi <- raster::mask(crop(rst.p, ply), mask = ply)
    
    plot(rst.ozcbi)
    writeRaster(rst.ozcbi, here::here(v, "ozcbi_geoTifs", paste0("OzCBI_", burns[i], "_", str_replace(v, "v", ""), ".tif")), 
                overwrite=TRUE)
    
    rst.p <- reclassify(rst.p, rclmat)

    plot(rst.p)
    ############# add heath
    if (nrow(heath.i)!=0){
      heath.i$n <- 1
      rst.h <- fasterize(heath.i, rst.p, field = "n")
      rst.nh <- rst.h 
      rst.nh[is.na(rst.nh)] <- 2
      rst.nh[rst.nh==1] <- NA
      rst.nh[rst.nh==2] <- 1
      plot(rst.nh)
      
      rst.hp <- rst.h * rst.p
      rst.hp[rst.hp>1] <- 6
      #rst.hp[rst.hp==1] <- NA
      plot(rst.hp)
      
      rst.nhp <- rst.nh * rst.p
      rst.p <- cover(rst.nhp, rst.hp)
      plot(rst.p)
    }

    unburnt.rst <- raster(here(v, "actual_burnt", paste0(bdr.name, "_unburnt.tif"))) %>%
      crop(ply)

      rst.p <- raster::cover(unburnt.rst, crop(rst.p, ply))
      rst.p <- raster::mask(rst.p, mask = ply)
      plot(rst.p)
      
      ### stats
      df.freq <- as.data.frame(freq(rst.p)) %>% na.omit()
      df.freq <- mutate(df.freq, perc1 = round((count/sum(count))*100, 2)) %>%
        dplyr::select(-count)
      
      df.freq <- left_join(data.frame(value = seq(1, 6), perc = 0), df.freq, by = "value") %>%
        mutate(perc = perc + perc1) %>%
        dplyr::select(-perc1)
      df.freq$perc[is.na(df.freq$perc)] <- 0
      df.freq$BURNID <- burns[i]
      df.freq <- spread(df.freq, value, perc)
      
      df.freq$model <- ibra.model$type[1]
      df.freq$index <- mod.index
      df.freq$date <- str_replace(v, "v", "")
      write_csv(df.freq, here(v,  "severity_stats", paste0("\\FireSevStat_",  
                                burn.ibra,"_",  burns[i], "_", 
                                ibra.model$type[1], "_s2a_",
                                mod.index, "_", str_replace(v, "v", ""), ".csv")))
      ###
      
      
      writeRaster(rst.p, here::here(v, "severity_geoTifs", paste0("BurnSeverity_", burns[i], "_", str_replace(v, "v", ""), ".tif")), overwrite=TRUE)
    
      
      all.dates <- unlist(str_split_fixed(list.files(here(paste0("all_rgbs/rgb_", burns[i])), pattern = "png" ), "_", n=3)[,2])
      Image.dates <- paste0("from ", all.dates[1], " to ", all.dates[length(all.dates)])
      
      ub <- readRDS(here("tmp", paste0("ub_", bdr.name)))
      #dir.create(here::here("CBI_maps\\jpgs"), showWarnings = FALSE)
      png(here::here(v, "severity_maps", paste0("BurnSeverityMap_", burns[i], "_", str_replace(v, "v", ""), ".png")), width = 550, height = 550)
      #my.palette <- brewer.pal(n = 5, name = "YlOrRd")
      #plot(rst.p, col = diverging_hcl(5, palette = "Blue-Red 2"), main=paste0(burns[i], ": ", burn.ibra, ", model: ", model.date))
      plot(rst.p, breaks = c(0,1,2,3,4,5, 6), col = c("lightblue", "darkolivegreen2", "yellow", "orange", "red",  "grey70"), 
           main=paste0("Burn severity map for: ", burns[i],"\nCreated: ", str_replace(v, "v", ""), 
                       ", Imagery: ", all.dates[1], " to ", all.dates[length(all.dates)]), 
           sub = paste0("Buffer difference = ", round(ub$threshold, 3),  "\nDeveloped by Densmore and van Dongen (2021)"))
      dev.off()
      
      freq.df <- as.data.frame(freq(rst.p)) %>% na.omit()
      freq.df <- mutate(freq.df, Percent = (count/sum(freq.df$count))*100)
      
      ggplot(freq.df, aes(value, Percent))+
        geom_col()+
        geom_text(aes(label=round(Percent, digits = 1)), vjust = -0.2)+
        labs(title = paste0("Percent area per severity class: ",burns[i],  "\nCreated: ", str_replace(v, "v", "")) 
             , x = "Severity class", y = "Percent of burnt area (%)"
        )+
        theme_bw()
      ggsave(here::here(v, "severity_stats", paste0("BurnSeverityGraph_", burns[i], "_", str_replace(v, "v", ""), ".jpg")), width = 4, height = 5)
      ### metadata

      library(MESS)
      BurnID = burns[i]
      Date = str_replace(v, "v", "")
      Index = "NBR"
      Satellite = "Landsat"
      Heath = if(nrow(heath.i)!=0){"Heath mapping included"}else{"Heath mapping not included"}
      Severity.Classes <- "####### CBI ########"
      Unburnt = paste0("0 to ", rclmat[1,2])
      Low = paste0(rclmat[2,1], " to ", rclmat[2,2])
      Medium = paste0(rclmat[3,1], " to ", rclmat[3,2])
      High = paste0(rclmat[4,1], " to ", rclmat[4,2])
      Very.High = paste0(rclmat[5,1], " to 3")
      Ibra.model = ibra.model[1,1]
      notes = "Developed by Valerie Densmore and Ricky van Dongen, 2021"
      
      all.dates <- as.data.frame(str_split_fixed(list.files(here(paste0("all_rgbs/rgb_", burns[i])), "png" ), "_", n=3)[,2])
      colnames(all.dates)[1] <- "im.dates"
      all.dates$n <- paste0("image-", row.names(all.dates))
      all.dates <- spread(all.dates, n, im.dates)
      
      Image.dates <- paste0("from ", all.dates[1,1], " to ", all.dates[, ncol(all.dates)])
      
      df <- data.frame(BurnID, Date, Index, Satellite, Image.dates, Heath, 
                       Severity.Classes, Unburnt, Low, Medium, High, Very.High,
                       Ibra.model, notes)
      df <- bind_cols(df, all.dates)
      
      write.xml(df, here(v, "severity_geoTifs", paste0("BurnSeverity_", burns[i], "_", str_replace(v, "v", ""), "_metadata.xml")))
      write.xml(df, here(v, "severity_maps", paste0("BurnSeverityMap_", burns[i], "_", str_replace(v, "v", ""), "_metadata.xml")))
      write.xml(df, here(v, "severity_stats", paste0("BurnSeverityGraph_", burns[i], "_", str_replace(v, "v", ""), "_metadata.xml")))
      
      }


stopCluster(cl)

df <- lapply(list.files(here(v, "severity_stats"), "FireSevStat", full.names = TRUE), read_csv) %>%
  bind_rows()
write_csv(df, here(v, "severity_stats", "summaryStats.csv"))
dfg <- gather(df, class, perc, 2:6)
ggplot(dfg, aes(class, perc)) +
  geom_col()+
  labs(y = "Percent of area (%)", x = "Severity class")+
  facet_wrap(.~BURNID)
ggsave(here(v, "severity_stats", "summaryStats.jpg"), width = 5, height = 5)


########################################################
#make treatment area
idir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Historical\\xIndex\\"
shpl <- sort(list.files(idir, pattern = "DBCA_FireHistory_1987to2017_Id.*shp$"), decreasing = TRUE)
shp.id <- st_read(paste0(idir, shpl[1]), quiet = TRUE) %>%
  st_drop_geometry() %>%
  dplyr::select(-x, -y)

dates <- read.csv(here("inputs", "clean_dates_edited.csv"))

lshp <- list.files(here("inputs\\shpByBurn"), pattern = "shp$")
shp <- st_read(here("inputs\\shpByBurn\\", lshp[1]), quiet = TRUE)
shp <- shp[0,]

for(j in 1:length(lshp)){
  shp.j <- st_read(here("inputs\\shpByBurn\\", lshp[j]), quiet = TRUE) %>%
    dplyr::select(BURNID) %>% 
    st_transform(crs = st_crs(shp))
  shp <- rbind(shp, shp.j)
}

shp.att <- left_join(shp, shp.id, by = "BURNID")
shp.att <- filter(shp.att, BURNID %in% in.rgb)

block.name <- str_split_fixed(here(), "Historical/", 2)[,2]
dir.create(here(v, "treatment_area"), showWarnings = FALSE)
st_write(shp.att, here(v,"treatment_area",  paste0(block.name, "_treatmentArea.shp")), 
         quiet = TRUE, append=FALSE)

