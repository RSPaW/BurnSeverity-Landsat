# the aim of this code produce a standard input file which will then be   
# copied to the sandbox. 
# The code takes the fire boundary shp and a list of fire dates and combines
# the into one shp with standard columns. if this is done correctly all 
# remaing codes should run wihtout error.

# Load libraries
library(sf) # to handle shp files
library(tidyverse) # data manipulation
library(lubridate) # date manipulation
library(here) # reference data locations
library(lwgeom)
library(raster)
library(filesstrings)
library(parsedate)

mga50 <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"

# set the number of days to be included in the analysis. Fires along the south coast
# may need more days to capture pre and post fire images.
# including more days just means more time ding the cloud qa
pre.days <- 400 # number of days prior to start date included
post.days <- 400  # number of days following to end date included

dates <- read_csv(here("inputs", "clean_dates_edited.csv"))

burns <- str_split_fixed(list.dirs(here("all_rgbs", "_Redo"), full.names = FALSE, recursive = FALSE), "_", 2)[,2]

shps <- list.files(here("inputs", "shpByBurn"), "shp$")

shp <- st_read(here("inputs", "shpByBurn", shps[1]))[0,]
i <- 1
for (i in 1:length(burns)){
  shp.name <- shps[str_detect(shps, burns[i])]
  shp.i <- st_read(here("inputs", "shpByBurn", shp.name)) %>%
    dplyr::select(BURNID)
  shp.i <- left_join(shp.i, dates, by = "BURNID") %>%
    dplyr::select(BURNID, date, date_end, im_start, im_end)
  shp <- rbind(shp, shp.i)
}

shp.alb <- st_transform(shp, crs = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
# check if polys look sensible
plot(shp.alb[,1])

block.name <- str_split_fixed(str_split_fixed(here(), "Historical/", 2)[,2], "_", 2)[,2]
#save out shp
st_write(shp.alb, here::here(paste0("inputs\\clean_", 
                                    block.name,"_", Sys.Date(), "_redo.shp")), delete_dsn=TRUE)
############################################################################

library(parsedate)

unlink(here("tmp"), recursive = TRUE)

tarfile <- here("lst_rgb_nbr_redo.tar.gz")
untar(tarfile, exdir = here("tmp"))

plst <- list.files(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr"), pattern = ".png$" )
tlst <- list.files(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr"), pattern = ".tif$" )

burns <- unique(str_split_fixed(plst, "_", 3)[,1])

i <- 1
pi.all <- data.frame()
for (i in 1:length(burns)){
  burn.i <- burns[i]
  date.i <- filter(dates, BURNID == burns[i])
  pi <- as.data.frame(plst[str_detect(plst, paste0(burn.i, "_"))])
  colnames(pi) <- "file"
  pi <- pi %>% mutate(date = ymd(str_split_fixed(file, "_", 3)[,2])) %>%
    mutate(type = case_when(date < parse_date_time(date.i$date, c("ymd", "dmy")) ~ "pre", 
                            TRUE ~ "post")) %>%
    mutate(new.name = paste0(str_sub(file, end = -5), "_", type, ".png"), 
           BURNID = burns[i])
  pi.all <- bind_rows(pi, pi.all)
  file.rename(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", pi$file), 
              here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", pi$new.name)) 
  
  fname <- here("all_rgbs", paste0("rgb_", burn.i))
  dir.create(fname, showWarnings = FALSE)
  dir.create(paste0(fname,"\\remove"), showWarnings = FALSE)
  file.move(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", pi$new.name), paste0(fname), overwrite = TRUE)
  
  unlink(here("tifs", burn.i), recursive = TRUE)
  
  ti <- tlst[str_detect(tlst, paste0(burn.i, "_"))]
  fname <- here("tifs", burn.i)
  dir.create(fname, showWarnings = FALSE)
  file.move(here("tmp\\home\\jovyan\\fireSeverity\\rgb_nbr", ti), paste0(fname), overwrite = TRUE)
} 

dir.create(here("tarDone"))
file.move(tarfile, here("tarDone"))
