#' ---
#' title: "Turtle-EOV Animation"
#' author: "Dana K Briscoe"
#' date: "`r Sys.Date()`"
#' output: html_document
#' ---
#' 

## ----load-libraries-----------------------------------------------------------------------------------------------------
# Load Libraries ---
library(tidyverse)
library(data.table)
library(sp)
library(sf)
library(gganimate)
library(ggmap)
library(dplyr)
library(magrittr)
library(magick)
library(raster)
library(rnaturalearth)
library(gifski)


#' 
## ----temp-funcs-cell----------------------------------------------------------------------------------------------------
## Funtions ----
make180 <- function(lon){
    isnot360<-min(lon)<0
    if (!isnot360) {
        ind<-which(lon>180)
        lon[ind]<-lon[ind]-360
    }
    return(lon)
}

make360 <- function(lon){
    isnot360<-min(lon)<0
    if(isnot360){
        ind<-which(lon<0)
        lon[ind]<-lon[ind]+360
    }  
    return(lon)
}

# parseDT
parseDT <- function(x, idx, start, stop, format){
    ret <- fs::path_file(x) %>%
        substr(., start=start, stop=stop) %>%
        as.character(strptime(.,format=format,tz='UTC'))
    return(ret)
}



#' 
## ----load-turtle-data---------------------------------------------------------------------------------------------------
## Pull Raw Tracking Data -----
files <- list.files('~/Downloads/batch/', pattern = "All.csv", recursive=T, full.names=T)

raw_data <- rbindlist(lapply(files, fread)) %>%
   dplyr::select('Platform ID No.', 'Latitude', 'Longitude', 'Loc. quality', 'Loc. date') %>%
    rename(id = 1,
           lat = 2, 
           lon = 3, 
           loc_quality = 4, 
           date = 5
    ) %>%
    mutate(date = as.POSIXct(date, format= "%m/%d/%Y %H:%M:%S", tz="UTC")) %>%
    filter(date >= '2023-07-11 04:30:00')

head(raw_data)

daily_avg_data <- raw_data %>%
    mutate(date = as.Date(date)) %>%
    group_by(id, date) %>%
    summarise(lat = mean(lat), 
              lon = mean(lon))

# head(daily_avg_data)


#' 
## ----load-eov-ncs-------------------------------------------------------------------------------------------------------
params <-list()
params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/deploy_reports"
params$eov = 'sst'

ncs <- list.files(params$nc_path, pattern = params$eov, full.names=T)

fdates <- ncs %>% 
    str_split(., "_") %>%
    purrr::map_chr(~ pluck(., 5)) %>%
    substr(., start=1, stop=10)

#' 
## ----prep-turtle-sp-----------------------------------------------------------------------------------------------------
## turtles geo
# 1) order points by date, rm na's
turtles <- daily_avg_data %>%
    arrange(id, date) %>%
    filter(!is.na(lon), !is.na(lat)) 

# 2) make spatial
turtles_sp <- turtles
coordinates(turtles_sp) <- c("lon", "lat")

# 3) set coord system
proj4string(turtles_sp) <- CRS("+init=EPSG:4326")

# 4) transform to decimal degrees (from UTM)
# turtlesspgeo <- spTransform(turtles_sp, CRS("+init=epsg:3857"))
turtlesspgeo <- turtles_sp

# 5) make back into dataframe (but include date for our animation)
# ggmap and gganimate use dataframes for plotting
turtlesgeo <- as.data.frame(turtlesspgeo@coords)
turtlesgeo$id <- turtlesspgeo@data$id # add individual identifier
turtlesgeo$date <- as.Date(turtlesspgeo@data$date) # Important! the variable for revealing in the animation must be
# either integer, numberic, POSIXct, Date, difftime, or orhms. Here I made sure it is a date.

head(turtlesgeo)

#' 
## ----set-sp-extents-----------------------------------------------------------------------------------------------------
## Set up Extents -- move to params later
xrange = c(120, 260)   # fyi, long has to be in 0 to 360 for the animation to work :/
yrange = c(25, 50)

e <- extent(xrange[1], xrange[2], yrange[1], yrange[2])

e_subset <- extent(make360(-160), make360(-140), 35, 50)
e_subset_180 <- extent(-160, -140, 35, 50)


#' 
## ----prep-ras-data------------------------------------------------------------------------------------------------------
# get dates that match track dates
tracking_dates <- unique(daily_avg_data$date)
deploy_date <- as.Date("2023-07-10")

daily_dates <- c(deploy_date, tracking_dates)
daily_dates <-  unique(daily_avg_data$date)
# # remove last date from trackign data to match most recently available eov layer
daily_dates <- daily_dates[-length(daily_dates)]

ncIn <- sapply(1:length(daily_dates), function(x) ncs[grepl(daily_dates[x],ncs)])

# convert ncs to raster stack
ras <- raster::stack(ncIn)

nc_dates <- ncIn %>% 
    map(~parseDT(., idx = 6, start=13, stop = 22, format="%Y-%m-%d")) %>%
    unlist() %>% as.Date() #%>%

names(ras) <- nc_dates


#' 
## ----crop-ras-extent----------------------------------------------------------------------------------------------------
ras_subset <- crop(ras, e_subset_180)

# rm(ras)

#' 
## ----convert-ras-to-df--------------------------------------------------------------------------------------------------
# convert gridded raster data dataframe
g <- ras_subset
g_df_subset <- g %>%
    rasterToPoints %>%
    as.data.frame() %>%
    `colnames<-`(c("x", "y", names(g))) %>%
    pivot_longer(cols = starts_with("X20"),
                 names_to = "layer",
                 values_to = "val") %>%
    mutate(layer = substr(layer, 2, 14)) %>%
    mutate(date = as.POSIXct(layer, "%Y.%m.%d", tz = "UTC")
    ) %>% 
    mutate(x = make360(x)) %>%
    mutate(date = as.Date(date))

head(g_df_subset)   
tail(g_df_subset)
#' 
## ----set-map-params-----------------------------------------------------------------------------------------------------
# formulate graphing element
mapdata <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) %>%
    filter(long >= 120 & long <= 270 & lat >= 15 & lat <=80) 
# cpal
# limits = c(min(g_df$value, na.rm=T), max(g_df$value, na.rm=T))
limits = c(5,35)
smooth_rainbow <- khroma::colour("smooth rainbow")

cpal <- smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9))


#' 
#' ## ----ggplot-static-with-smoother----------------------------------------------------------------------------------------
#' 
#' ## data sets ----
#' sst_df = g_df_subset #%>% mutate(val = ifelse(val >= 17.0 & val <= 17.1, NA, val))
#' turtles_df = turtlesgeo %>% filter(date %in% nc_dates)
#' 
#' release_loc = data.frame(lat=39.315, lon=213.9333)
#' 
#' params$barheight = 28 #38
#' params$plot_text_size = 14
#' ## ggplot 
#' gg_smooth <- 
#'     ggplot() +
#'     geom_tile(
#'         data = sst_df %>% mutate(date = as.Date(date)),
#'         aes(x = x, y = y, fill = val, group = date)) +
#'     # # geom_point(data=sst_df %>% mutate(date = as.Date(date)) %>% filter(val == 17),aes(x=x,y=y, group=1), color="black", alpha = 0.40, size=2.25)+
#'     # geom_line(data=sst_df %>% mutate(date = as.Date(date)) %>% filter(val == 17),aes(x=x,y=y, group=1), color="snow", alpha = 0.40, size=1.25)+
#'     geom_smooth(data=sst_df %>% mutate(date = as.Date(date)) %>% filter(val == 17),aes(x=x,y=y),
#'                 # method="loess",
#'                 span=0.1,
#'                 se=F,
#'                 color="snow", alpha = 0.40, size=1.25) +
#'     
#'     scale_fill_gradientn(colours = cpal[2:length(cpal)], 
#'                          breaks=seq(10,25,2),
#'                          limits = c(9,25),na.value = 'snow') +
#'     scale_colour_gradientn(colours = cpal[2:length(cpal)]) +
#'     
#'     geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
#'     
#'     # turtle daily movements
#'     geom_point(data=turtles_df %>% mutate(lon = make360(lon)),aes(x=lon,y=lat, group=id), color="black", alpha = 0.40, size=4.25)+
#'     geom_path(data=turtles_df %>% mutate(lon = make360(lon)),aes(x=lon,y=lat, group=id), color="black", alpha = 0.40, size=2.25)+
#' 
#'     # release location
#'     geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
#'                color = "black", shape = 4, size = 7.5)+
#'     # 
#'     labs(x = "Longitude \n", y = "Latitude \n") +
#'     # theme(legend.position = "none") +
#'     
#'     theme_minimal() + theme(text=element_text(size=params$plot_text_size)) +
#'     # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 50), expand = FALSE, crs = st_crs(4326))    labs(x = "Longitude", y = "Latitude") +
#'     coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
#'     guides(fill = guide_colourbar(title = "SST (°C)", barheight = params$barheight,
#'                                   ticks = TRUE)) + 
#'     # facet_wrap(~date)
#'     NULL
#' 
#' 
#' #' 
#' ## ----gganimate-plot-----------------------------------------------------------------------------------------------------
#' 
#' ## Animate Tracks by Date ----- ----------------
#' anim_gg_smooth = gg_smooth + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
#'     labs(title = "STRETCH average daily turtle movements (n=25) and sea surface temperature (SST)",
#'          subtitle = "Date: {frame_time}", 
#'          caption = "Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
#'     # shadow_mark(alpha = 0.3, size = 0.25) +
#'     shadow_wake(wake_length = 0.75, alpha = TRUE, exclude_layer = c(1,2, length(daily_dates))) +
#'     enter_fade() +
#'     exit_disappear() +  
#'     # ease_aes('sine-in-out') +
#'     # view_follow(fixed_y = FALSE) +
#'     # transition_states(states=date, transition_length = 1, state_length = 1, wrap = F) +
#'     # # shadow_wake(wake_length = 1)
#'     NULL
#' 
#'     
#' 
#' #' 
#' ## ----render-animation-gif-----------------------------------------------------------------------------------------------
#' gganimate::animate(anim_gg_smooth, nframes = length(daily_dates), fps =1, 
#'                    detail = 5,
#'                    width = 1400, height = 865,
#'                    renderer = gifski_renderer(loop = TRUE))
#'  
#' 
#' #' 
#' ## ----render-animation-mp4-----------------------------------------------------------------------------------------------
#' # gganimate::animate(anim_gg_smooth, nframes = length(daily_dates), fps =3, 
#' #                    # detail = 5,
#' #                    # width = 1400, 
#' #                    # height = 865,
#' #         width = 1460, height = 720, res = 104,
#' #                    renderer = av_renderer('~/Downloads/animation.mp4'))
#'  
#' 
#' #' 
#' ## ----save-animation-----------------------------------------------------------------------------------------------------
#' # anim_save(animation = last_animation(),
#' #           fps =1,
#' #           nframes =  length(daily_dates),'~/Downloads/stretch_turtle_sst_daily_animation_23June2023_white_tzcf_v4_smooth.gif')



#' 
#' ## trial
#' 
## ----gg-smooth-plus-trial-----------------------------------------------------------------------------------------------
## data sets ----
sst_df = g_df_subset %>% mutate(date = as.Date(date)) #%>% mutate(val = ifelse(val >= 17.0 & val <= 17.1, NA, val))
turtles_df = turtlesgeo %>% filter(date %in% nc_dates) #%>% mutate(col_id = 1)

release_loc = data.frame(lat=39.315, lon=213.9333)

params$barheight = 24 #38
params$plot_text_size = 14
## ggplot 
gg_smooth_trial <- 
    ggplot() +
    geom_tile(
        data = sst_df,
        aes(x = x, y = y, fill = val, group = date)) +

    geom_smooth(data=sst_df %>% mutate(date = as.Date(date)) %>% filter(val == 17),aes(x=x,y=y),
                # method="loess",
                span=0.1,
                se=F,
                color="snow", alpha = 0.40, size=1.25) +
    
    scale_fill_gradientn(colours = cpal[2:length(cpal)], 
                         breaks=seq(10,25,2),
                         limits = c(9,25),na.value = 'snow') +
    scale_colour_gradientn(colours = cpal[2:length(cpal)]) +
    
    geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
    
    # turtle daily movements
    geom_point(data=turtles_df %>% 
                   mutate(lon = make360(lon)),
               # aes(x=lon,y=lat, group=id), color="black", alpha = 0.90, size=3.25) +
    
    aes(x=lon,y=lat), color = "azure2",
             fill = "#2a9d8f", shape = 21,
             stroke = 1, alpha = 0.90, size=3.25) +
    # geom_path(data=turtles_df %>% mutate(lon = make360(lon)),aes(x=lon,y=lat, group=id), color="black", alpha = 0.40, size=2.25)+

    # # segment
    # geom_segment(data=turtles_df %>% 
    #                  mutate(lon = make360(lon)), 
    #              aes(x = lon, y = lat,
    #                  xend = next_Lon,
    #                  yend = next_Lat), size = 2) + 
    
    # release location
    geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
               color = "black", shape = 4, size = 5.5)+
    # 
    
    # geom_text(data=sst_df, aes(x = min(x), y = min(y), label = as.factor(date)), 
    #           hjust=-2, vjust = -0.2, alpha = 0.2,  col = "gray", size = 20) +
    
    labs(x = "\n Longitude \n", y = "\n Latitude \n") +
    # theme(legend.position = "none") +
    
    theme_minimal() + theme(text=element_text(size=params$plot_text_size)) +
    # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 50), expand = FALSE, crs = st_crs(4326))    labs(x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
    guides(fill = guide_colourbar(title = "SST (°C)", barheight = params$barheight,
                                  ticks = TRUE)) + 
    # facet_wrap(~date)
    NULL

#' 
## ----trial-animate-plot-------------------------------------------------------------------------------------------------
## Animate Tracks by Date ----- ----------------
anim_trial = gg_smooth_trial + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
    labs(title = "",
         subtitle = "STRETCH Daily turtle movements (n=25) and sea surface temperature (SST) \nDate: {frame_time}", 
         caption = "Raw tracking data from ARGOS averaged to 1 daily location per turtle \n The white line represents the 17°C isotherm. Ship release location (X) \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
                  # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
     theme(
           plot.title = element_text(size=18),
           plot.subtitle = element_text(size = 12, face="bold"),
           plot.caption = element_text(size=10),
           plot.margin = unit(c(0.75, 0, 0, 0), "cm")) +
    # shadow_mark(alpha = 0.3, size = 0.25) +
    shadow_wake(wake_length=0.5, alpha = 0.2, wrap=FALSE,
                falloff = 'sine-in', exclude_phase = 'enter', size=0.75,
                exclude_layer = c(1,2, length(daily_dates))
                ) +
    enter_fade() +
    exit_disappear() +  
    # exit_recolour(color = "gray") +
    # ease_aes('sine-in-out') +
    # view_follow(fixed_y = FALSE) +
    # transition_states(states=date, transition_length = 1, state_length = 1, wrap = F) +
    # # shadow_wake(wake_length = 1)
    NULL


#' 
## ----trial-save-mp------------------------------------------------------------------------------------------------------
gganimate::animate(anim_trial, nframes = length(daily_dates), fps =2, 
                   # detail = 5,
                   # width = 1400, 
                   # height = 865,
        width = 1460, height = 720, res = 104,
                   renderer = av_renderer('~/Downloads/animation_trial.mp4'))

