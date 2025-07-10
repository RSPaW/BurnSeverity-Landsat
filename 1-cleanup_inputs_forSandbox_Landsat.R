# Load libraries
library(sf) # to handle shp files
library(tidyverse) # data manipulation
library(lubridate) # date manipulation
library(here) # reference data locations
library(lwgeom)
library(raster)
 
mga50 <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"

# set the number of days to be included in the analysis. Fires along the south coast
# may need more days to capture pre and post fire images.
# including more days just means more time ding the cloud qa
pre.days <- 400 # number of days prior to start date included
post.days <- 400  # number of days following to end date included

histpath <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\Historical"

# inputs 
shp.name <- "Amarillup_Fires.shp" # shp name

# read in shp
shp <- st_read(here::here("fireSelection", shp.name), stringsAsFactors = FALSE) %>% 
  st_make_valid() # check and correct geometry

 if (length(unique(shp$BURNID)) - nrow(shp) != 0){
   cat("the selection has duplicate ids!!!")
}

####################################
# check plys done
foldsdir <- list.dirs(histpath, recursive = FALSE, full.names = FALSE)
folds <- list.dirs(histpath, recursive = FALSE)[stringr::str_starts(foldsdir, "[[:digit:]]")]

ifold <- unlist(str_split(here(), "/"))
ifold <- ifold[length(ifold)]
folds <- folds[str_detect(folds, ifold)==FALSE]
i <- 6
idsDone <- NA

for(i in seq_along(folds)){
  shpn <- list.files(paste0(folds[i], "/inputs"), pattern = ".shp$",
                     full.names = TRUE)[1]
  if(length(shpn != 0)){
    burn_ids <- sf::st_read(shpn, quiet = TRUE) %>%
      dplyr::pull(BURNID)
    idsDone <- c(idsDone, burn_ids)
  }
}

shp.done <- filter(shp, BURNID %in% idsDone)
shp <- filter(shp, (BURNID %in% idsDone)==FALSE)

# check and correct date format
shp$date <- as.Date(parse_date_time(shp$FIH_DATE1, c("ymd", "dmy")))
# calculate start and end dates
shp <- shp %>% mutate(date_end = date + 90, im_start = date - pre.days, im_end = date + post.days)

shp.n <- dplyr::select(shp, BURNID, date, date_end, im_start, im_end) %>%
  na.omit()

# create directory for inputs
dir.create(here::here("inputs"), showWarnings = FALSE)
# reproject to albers equal
shp.alb <- st_transform(shp.n, crs = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
# check if polys look sensible
plot(shp.alb[,1])

#save out shp
block.name <- str_split_fixed(str_split_fixed(here(), "Historical/", 2)[,2], "_", 2)[,2]

st_write(shp.alb, here::here(paste0("inputs\\clean_", 
                                    block.name,"_", Sys.Date(), "_alb.shp")), delete_dsn=TRUE)


write_csv(st_drop_geometry(shp.n), here("inputs", "clean_dates.csv"))
write_csv(st_drop_geometry(shp.n), here("inputs", "clean_dates_edited.csv"))


# create directory for inputs
dir.create(here::here("inputs", "shpByBurn"), showWarnings = FALSE)
i <- 1
for (i in 1:nrow(shp.alb)){
  ply <- shp.alb[i,]
  st_write(ply, here::here("inputs", "shpByBurn", paste0(ply$BURNID[1], "_boundry.shp")), 
           delete_dsn=TRUE, showWarnings = FALSE, quiet = TRUE)
}
#################################################################
# get ibras
mdir <- "Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Historical/xModels"

ibra <- st_read(here(mdir,"IBRA_wa.shp"), quiet = TRUE) 
dir.create("models/ibras", recursive = TRUE)
library(doParallel)
i <- 1
UseCores <- 10
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
foreach(i = 1:nrow(shp.alb)) %dopar% {
  library(sf) # to handle shp files
  library(tidyverse) # data manipulation
  library(lubridate) # date manipulation
  library(here) # reference data locations
  #for(i in 1:nrow(shp.alb)){
  if(file.exists(here::here("models", "ibras", shp.alb$BURNID[i])) == FALSE){
    ply <- shp.alb[i,]
    ply.i <- st_intersection(st_transform(ply, crs = st_crs(ibra)), ibra)
    ply.i <- dplyr::select(ply.i, IWA_SUB_NA)
    ply.i$area <- as.numeric(st_area(ply.i))
    ply.i <- arrange(ply.i, desc(area))
    burn.ibra <- as.data.frame(ply.i$IWA_SUB_NA[1])
    colnames(burn.ibra)[1] <- "ibra"
    burn.ibra$BURNID <- ply$BURNID[1]
    saveRDS(burn.ibra, here::here("models", "ibras", ply$BURNID[1]))
  }
}
stopCluster(cl)

