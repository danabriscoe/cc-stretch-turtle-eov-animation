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
library(oce)
library(RColorBrewer)
library(rnaturalearth)
library(gifski)


## ---- source functions-----------------------------------------------------------------------------------------------------
source('code/00_automate_EOV_helper_functions.R')


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

# Set param to run
# params$eov = 'sst'
# params$eov = 'ssta'
params$eov = 'chla'

if(params$eov == 'sst'){
    params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/deploy_reports"
    params$varname <- "sst_dhw_5km"
} else if (params$eov == "ssta"){
    params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"
    params$varname <- "ssta_dhw_5km"
} else if (params$eov == "chla"){
    params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"
    params$varname <- params$eov  # special case. filename uses 'chla' and not 'chlor_a', which is the varname
}

# get ncdf list
ncs <- list.files(params$nc_path, pattern =  params$varname, full.names=T)
print(str_c('most recent ncdf - ', ncs[length(ncs)]))

if(params$eov == 'sst'){
    fdates <- ncs %>% 
        str_split(., "_") %>%
        purrr::map_chr(~ pluck(., 5)) %>%
        substr(., start=1, stop=10)
    
    limits = c(5,35)
    cbar_breaks = seq(6, 34, 2)
    cbar_limits = c(5, 34)
    tzcf_contour = 17
    
    smooth_rainbow <- khroma::colour("smooth rainbow")
    
    cpal <- c(smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9)), "#9e2a2b", "firebrick4", "#540b0e")
    

} else if(params$eov == 'ssta'){
    fdates <- ncs %>% 
        str_split(., "_") %>%
        purrr::map_chr(~ pluck(., 5)) %>%
        substr(., start=1, stop=10)
    
    # limits = c(5,35)
    
    tzcf_contour <- NA
    
    brewer.pal(n = 8, name = "RdBu")
    
} else if(params$eov == 'chla'){
    ncs <- list.files(params$nc_path, pattern = "NRTchla", full.names=T)
    print(str_c('most recent ncdf - ', ncs[length(ncs)]))
    
    fdates <- ncs %>% 
        str_split(., "/") %>%
        purrr::map_chr(~ pluck(., 8)) %>%
        substr(., start=29, stop=38)
    
    cbar_breaks <- c(0.2, 2, 10, 20)
    cbar_limits <- limits <- c(0,20)
    
    tzcf_contour = 0.2
    
    cpal <- oce::oceColorsChlorophyll(20)
    
    # cpal <- smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9))
    # cpal <- colorRampPalette(chl_colors)(25)
}


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
# xrange = c(120, 260)   # fyi, long has to be in 0 to 360 for the animation to work :/
# yrange = c(25, 50)

xrange = c(180, 250)   # fyi, long has to be in 0 to 360 for the animation to work :/
yrange = c(25, 50)

e <- extent(xrange[1], xrange[2], yrange[1], yrange[2])


e_subset <- extent(make360(e[1]), make360(e[2]), e[3], e[4])                            # make 360 (just in case)
e_subset_180 <- extent(make180(e[1]+1), make180(e[2]), make180(e[3]), make180(e[4]))    # make 180 (just in case)

#' 
## ----prep-ras-data------------------------------------------------------------------------------------------------------
# get dates that match track dates
tracking_dates <- unique(daily_avg_data$date)
eov_dates <- as.Date(unique(fdates))
deploy_date <- as.Date("2023-07-10")

# daily_dates <- c(deploy_date, tracking_dates)
# daily_dates <- c(deploy_date, eov_dates)
daily_dates <- intersect(as.character(tracking_dates), as.character(eov_dates))
# daily_dates <-  unique(daily_avg_data$date)
# # remove last date from trackign data to match most recently available eov layer
# daily_dates <- daily_dates[-length(daily_dates)]

ncIn <- sapply(1:length(daily_dates), function(x) ncs[grepl(daily_dates[x],ncs)])

# convert ncs to raster stack
ras <- {suppressWarnings(raster::stack(ncIn))}

if(params$eov == 'sst'){
nc_dates <- ncIn %>% 
    map(~parseDT(., idx = 6, start=13, stop = 22, format="%Y-%m-%d")) %>%
    unlist() %>% as.Date() #%>%
} else if (params$eov == 'chla'){
    nc_dates <- ncIn %>% 
        map(~parseDT(., idx = 6, start=29, stop = 38, format="%Y-%m-%d")) %>%
        unlist() %>% as.Date() #%>%
}
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


if(params$eov == 'sst'){
    eov_df <- g_df_subset %>% mutate(date = as.Date(date)) #%>% filter(date == '2023-07-11')
} else if(params$eov == 'chla'){
    eov_df <- g_df_subset %>% mutate(date = as.Date(date)) #%>%
       # mutate(val = log(val)) #%>% filter(date == '2023-07-11')
}

turtles_df = turtlesgeo %>% filter(date %in% nc_dates)

release_loc = data.frame(lat=39.315, lon=213.9333)

p_barheight = 28.5 #38
p_plot_text_size = 14

## Functionalized version ---------------------------
# test_cpal = c(smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9)), "#9e2a2b", "firebrick4", "#540b0e", "#540b0e")

gg_static <- get_static_plot(
  eov = params$eov,
  eov_df = eov_df,
  turtles_df = turtles_df,
  e,
  release_loc,
  cpal = cpal,
  cbar_breaks,
  cbar_limits
)


## Animate Tracks by Date ----- ----------------
if(params$eov == 'sst'){
    title_eov <- 'sea surface temperature (SST)'
    subtitle_text_col <- 'snow'
        caption_iso <- 'The white line represents the 17°C isotherm. '
        eov_source <- 'NOAA Coral Reef Watch 5km Daily SST'
} else if(params$eov == 'chla'){
    title_eov <- 'chlorophyll-a (Chl)'
    subtitle_text_col <- 'gray8'
        caption_iso <- 'The 0.2 mg/m^3 isopleth represents the approximate TZCF position in the eastern North Pacific. '
        eov_source <- 'VIIRS Near Real-Time, DINEOF Gap-Filled 9km Daily Chl Concentration'
}

anim_trial = gg_static + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
    labs(title = str_c("STRETCH Daily turtle movements (n=25) with ", title_eov),
         subtitle = "Date: {frame_time}", 
         caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location per turtle \n ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
    # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
    theme(
        # element_text(size=p_plot_text_size),
        plot.title = element_text(size=16, face="bold", margin=margin(t=20,b=0), hjust=0.03),
        plot.subtitle = element_text(size = 16, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
        plot.caption = element_text(size=12),
        plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
    # guides(fill = guide_colourbar(
    #     barheight = p_barheight
    # )) +
    
    shadow_wake(wake_length=0.5, alpha = 0.2, wrap=FALSE,
                falloff = 'sine-in', exclude_phase = 'enter', size=0.75,
                exclude_layer = c(1,2, length(daily_dates))
    ) +
    enter_fade() +
    exit_disappear() +  
    
    NULL


#' 
## ----trial-save-mp------------------------------------------------------------------------------------------------------
## Save as MP4 (Faster render)
gganimate::animate(anim_trial, nframes = length(daily_dates), fps =2, 
                   # width = 1460, height = 720, res = 104,
                   width = 1640, height = 900, res = 104,
                   renderer = av_renderer(str_c('~/Downloads/dbriscoe_animation_trial_', params$eov,'_full_contours_v3.mp4')))




### MANUAL PLOTTING BELOW ----------------------------

#' ## GGPLOT STATIC  --------------------------------------------------------------------------------------------------------------------
#' gg_smooth_trial <- 
#'     ggplot() +
#'     geom_tile(
#'         # data = sst_df,
#'         data = eov_df,
#'         aes(x = x, y = y, fill = val, group = date)) +
#' 
#' #    geom_line(data=eov_df %>% mutate(date = as.Date(date)) %>% filter(val == 17),aes(x=x,y=y, group=1), color="snow", alpha = 0.40, size=1.25)+
#'     
#'     ## add smoother for tzcf -- Hold off on smoother for now...
#'     {
#'         if (params$eov=='sst') {
#'             geom_smooth(data=eov_df %>% mutate(date = as.Date(date)) %>% filter(val >= contour_val_min & val <= contour_val_max),aes(x=x,y=y),
#'                         method="loess",
#'                         span=0.3,
#'                         se=F,
#'                         color="snow", alpha = 0.40, size=1.25)
#'         }
#'     } +
#' 
#'     # add coast 
#'     geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
#'     
#' 
#'     {
#'         if ( params$eov=='chla'){
#'             scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
#'                               breaks = seq(0,1.8,0.1),
#'                               # limits = c(0, 2.0),
#'                               na.value = 'snow',
#'                               name = "Chl (mg/m^3)")
#'         } else if ( params$eov=='sst') {
#'             scale_fill_gradientn(colours = 
#'                                      # cpal[12:length(cpal)],
#'                                      cpal[9:length(cpal)],
#'                                  # breaks=seq(10,25,2),
#'                                  # limits = c(9.25,25),
#'                                  breaks=seq(6,32,2),
#'                                  limits = c(4,33),
#'                                  na.value = 'snow',
#'                                  name = "SST (°C)")
#'         }
#'     } +
#'     
#'     
#'     # turtle daily movements
#'     geom_point(data=turtles_df %>% 
#'                         mutate(lon = make360(lon)),
#'                     aes(x=lon,y=lat), color = "azure2",
#'                      # fill = "#2a9d8f", shape = 21,
#'                     fill = "#3c096c", shape = 21,
#'                     stroke = 1, alpha = 0.90, size=3.25) +
#' 
#'     # release location
#'     geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
#'                color = "black", shape = 4, size = 5.5) +
#' 
#'     labs(x = "\n \n Longitude \n", y = "\n Latitude \n \n ") +
#'     # theme(legend.position = "none") +
#'     
#'     theme_minimal() + theme(text=element_text(size=params$plot_text_size)) +
#'     # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
#'     coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) +
#'     guides(fill = guide_colourbar(#title = "SST (°C)", 
#'                                   barheight = params$barheight,
#'                                   ticks = TRUE)) + 
#'     # # facet_wrap(~date)
#'     NULL
#' 
#' 
#'     
#' #' 
#' ## ----trial-animate-plot-------------------------------------------------------------------------------------------------
#' ## Animate Tracks by Date ----- ----------------
#' if(params$eov == 'sst'){
#'     title_eov <- 'sea surface temperature (SST)'
#'     subtitle_text_col <- 'snow'
#'     caption_iso <- '' #'The white line represents the 17°C isotherm. '
#'     eov_source <- 'NOAA Coral Reef Watch 5km Daily SST'
#' } else if(params$eov == 'chla'){
#'     title_eov <- 'chlorophyll-a (Chl)'
#'     subtitle_text_col <- 'gray8'
#'     caption_iso <- 'The 0.2 mg/m^3 isopleth represents the approximate TZCF position'
#'     eov_source <- 'VIIRS Near Real-Time, DINEOF Gap-Filled 9km Daily Chl Concentration'
#' }
#' 
#' anim_trial = gg_smooth_trial + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
#'     labs(title = str_c("STRETCH Daily turtle movements (n=25) with ", title_eov),
#'          subtitle = "Date: {frame_time}", 
#'          caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location per turtle \n ", caption_iso,"Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
#'                   # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
#'      theme(
#'            plot.title = element_text(size=16, face="bold", margin=margin(t=20,b=0), hjust=0.03),
#'            plot.subtitle = element_text(size = 16, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
#'            plot.caption = element_text(size=12),
#'            plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
#' 
#'     shadow_wake(wake_length=0.5, alpha = 0.2, wrap=FALSE,
#'                 falloff = 'sine-in', exclude_phase = 'enter', size=0.75,
#'                 exclude_layer = c(1,2, length(daily_dates))
#'                 ) +
#'     enter_fade() +
#'     exit_disappear() +  
#' 
#'     NULL
#' 
#' 
#' #' 
#' ## ----trial-save-mp------------------------------------------------------------------------------------------------------
#' ## Save as MP4 (Faster render)
#' gganimate::animate(anim_trial, nframes = length(daily_dates), fps =2, 
#'         # width = 1460, height = 720, res = 104,
#'         width = 1640, height = 900, res = 104,
#'                    renderer = av_renderer(str_c('~/Downloads/dbriscoe_animation_trial_', params$eov,'_full_v5.mp4')))
#' 
#' 
#' # ## Save as GIF
#' # gganimate::animate(anim_trial, nframes = length(daily_dates), fps =2, 
#' #                    # detail = 5,
#' #                    # height = 4, #width = 3000,
#' #                    #  # height = 700, #width = 2000, 
#' #                    # units = "in", res=150,
#' #                    width = 1400, height = 865,
#' #                    renderer = gifski_renderer(loop = TRUE))
#' # 
#' # anim_save(animation = last_animation(),
#' #           fps =2,
#' #           nframes =  length(daily_dates),str_c('~/Downloads/dbriscoe_animation_trial_', params$eov,'_v1.gif'))
#' # 
