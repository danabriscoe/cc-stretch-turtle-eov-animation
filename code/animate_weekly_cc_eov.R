#' ---
#' title: "Turtle-EOV Animation - Weekly"
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
    filter(lat > 0 & lat < 60) %>%
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
params$eov = 'ssta'
# params$eov = 'chla'
# params$eov = 'sla_uv'

if(params$eov == 'sst'){
    params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/deploy_reports"
    params$varname <- "sst_dhw_5km"
} else if (params$eov == "ssta"){
    params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"
    params$varname <- "ssta_dhw_5km"
} else if (params$eov == "chla"){
    params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"
    params$varname <- params$eov  # special case. filename uses 'chla' and not 'chlor_a', which is the varname
} #else if (params$eov == "sla_uv"){
# params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"
# params$varname <- c('sla', 'ugos', 'vgos')  # special case. multiple vars inside ncdf
# }

# get ncdf list
ncs <- list.files(params$nc_path, pattern = params$varname, full.names=T)
print(str_c('most recent ncdf - ', ncs[length(ncs)]))

if(params$eov == 'sst'){
    fdates <- ncs %>% 
        str_split(., "_") %>%
        purrr::map_chr(~ pluck(., 5)) %>%
        substr(., start=1, stop=10)
    
    limits = c(5,35)
    # cbar_breaks = seq(4, 34, 1)
    # cbar_limits = c(4, 32)
    # # cbar_limits <- c(6,31)
    
    # goc masked
    cbar_breaks = seq(4, 30, 1)
    cbar_limits = c(4, 31)
    # cbar_limits <- c(6,31)
    cbar_int <- 1
    
    tzcf_contour = 18
    tzcf_color = 'white'
    
    smooth_rainbow <- khroma::colour("smooth rainbow")
    
    # cpal <- c(smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9)), "#9e2a2b", "firebrick4", "#540b0e")
    cpal <- c(smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9)), "#9e2a2b", "firebrick4")
    
    zCuts <- seq(4,34,1)
    
} else if(params$eov == 'ssta'){
    fdates <- ncs %>% 
        str_split(., "_") %>%
        purrr::map_chr(~ pluck(., 4)) %>%
        substr(., start=1, stop=10)
    
    # limits = c(5,35)
    
    tzcf_contour <- NA
    tzcf_color = NA
    
    cbar_breaks <- seq(-5.5,6,0.5)
    cbar_limits <- c(-5.5,6)
    cbar_int <- 0.5
    
    # brewer.pal(n = 8, name = "RdBu")
    cpal <- rev(c("#48090B", "#540b0e", rev(brewer.pal(9, "YlOrRd")),"white", brewer.pal(9, "Blues"), "#06224C", "#031126", "#020813"))
    
    zCuts <- seq(-5.5,6,0.5)
    
} else if(params$eov == 'chla'){
    ncs <- list.files(params$nc_path, pattern = "NRTchla", full.names=T)
    print(str_c('most recent ncdf - ', ncs[length(ncs)]))
    
    fdates <- ncs %>% 
        str_split(., "/") %>%
        purrr::map_chr(~ pluck(., 8)) %>%
        substr(., start=29, stop=38)
    
    cbar_breaks <- c(0.2, 2, 10, 20)
    cbar_limits <- limits <- c(0,20)
    cbar_int <- 0.1
    
    tzcf_contour = 0.2
    tzcf_color = 'gray10'
    
    
    cpal <- oce::oceColorsChlorophyll(20)
    
    # cpal <- smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9))
    # cpal <- colorRampPalette(chl_colors)(25)
    zCuts <- seq(0,20,1)
}


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


## ----set-sp-extents-----------------------------------------------------------------------------------------------------
## Set up Extents -- move to params later
# xrange = c(120, 260)   # fyi, long has to be in 0 to 360 for the animation to work :/
# yrange = c(25, 50)

xrange = c(make360(-165), 250)   # fyi, long has to be in 0 to 360 for the animation to work :/
# yrange = c(35, 50)
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
} else if(params$eov == 'ssta'){
    nc_dates <- ncIn %>% 
        map(~parseDT(., idx = 6, start=14, stop = 23, format="%Y-%m-%d")) %>%
        unlist() %>% as.Date() #%>%
}
names(ras) <- nc_dates


#' 
## ----crop-ras-extent----------------------------------------------------------------------------------------------------
ras_subset <- crop(ras, e_subset_180)

# rm(ras)

# Mask out GoC -----------------
goc_mask <- rbind(c(-110, 23.75), c(-122, 40), c(-110, 40), c(-110, 23.75))
goc_mask <- SpatialPolygons(list(Polygons(list(Polygon(goc_mask)), 1)))

## THIS WORKS!!!
ras_masked <- mask(ras_subset, goc_mask, inverse=T)


#' 
## ----convert-ras-to-df--------------------------------------------------------------------------------------------------
# convert gridded raster data dataframe
# g <- ras_subset
g <- ras_masked

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

## get cclme shapefile
cclme_df <- get_cclme_df()


if(params$eov == 'sst'){
    eov_df <- g_df_subset %>% mutate(date = as.Date(date)) #%>% filter(date == '2023-07-11')
} else if(params$eov == 'chla'){
    eov_df <- g_df_subset %>% mutate(date = as.Date(date)) #%>%
    # mutate(val = log(val)) #%>% filter(date == '2023-07-11')
} else if(params$eov == 'ssta'){
    eov_df <- g_df_subset %>% mutate(date = as.Date(date))
}

turtles_df = turtlesgeo %>% filter(date %in% nc_dates) #%>% filter(id == "243197")

release_loc = data.frame(lat=39.315, lon=213.9333)

p_barheight = 28.5 #38
p_plot_text_size = 14


# >>>>>>>> TO UPDATE FUNCT WITH THIS INFO
save_ext <- 'gif'
# save_ext <- 'mp4'

# if(save_ext == 'mp4'){
#     plot_params <- list(
#         turtle_pt_size = 3.25,
#         barheight = 26.5, #38
#         plot_text_size = 14,
#         title_size = 18,
#         subtitle_size = 16,
#         caption_size = 14
#     )  
# }
if(save_ext == 'gif'){
    plot_params <- list(
        turtle_pt_size = 2,
        barheight = 24,
        plot_text_size =12,
        title_size = 14,
        subtitle_size = 14,
        caption_size = 12
    )
}

# 
# 
# library(gganimate)
# library(gifski)
# 
# #     # )

# eov='sst'
eov = params$eov
cclme = TRUE

# if(params$eov == 'sst'){
#     zCuts <- seq(4,34,1)
# } else if(params$eov == 'chl')


# ## fun innerds
# mapdata <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) %>%
#     filter(long >= 120 & long <= 270 & lat >= 15 & lat <=80) 

library(sf)
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa <- st_as_sf(usa, wkt = "geom", crs = 4326)
st_set_crs(usa, 4326)
usa_360 = st_shift_longitude(usa)

# p_barheight = 28.5 #38
# p_plot_text_size = 14

# if(eov == 'sst'){
#     tzcf_contour = 18 #17
#     tzcf_color = 'white'
# } else if(eov == 'chla'){
#     tzcf_contour = 0.2
#     tzcf_color = 'gray10'
# } else if(eov == 'ssta'){
#     tzcf_contour = NULL
# }




# -----------------------------------------------------------------------------------------------
release_loc = data.frame(lat=39.315, lon=213.9333)

# p_barheight = 28.5 #38
# p_plot_text_size = 14


## Functionalized version ---------------------------
# test_cpal = c(smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9)), "#9e2a2b", "firebrick4", "#540b0e", "#540b0e")

# convert to weekly dates
eov_df_weekly <- eov_df %>%
    mutate(end_of_week = ceiling_date(date, "week") %>% as.Date(.)) %>%
    group_by(end_of_week, x, y) %>%
    summarize(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
    rename('date' = 'end_of_week')

turtles_df_weekly <- turtles_df %>%
    mutate(end_of_week = ceiling_date(date, "week") %>% as.Date(.)) %>%
    group_by(end_of_week, id) %>%
    summarize(lat = mean(lat, na.rm = TRUE),
              lon = mean(lon, na.rm = TRUE), .groups = "drop") %>%
    rename('date' = 'end_of_week')


## Create static plot
gg_static <- get_static_plot(
    eov = params$eov,
    # eov_df = eov_df, #%>% filter(date >= '2023-07-15'),
    # turtles_df = turtles_df, #%>% filter(date >= '2023-07-15'),
    eov_df = eov_df_weekly %>% filter(date >= '2023-07-15'),
    turtles_df = turtles_df_weekly%>% filter(date >= '2023-07-15'),
    e,
    release_loc,
    cpal = cpal,
    cbar_breaks,
    cbar_limits,
    plot_params,
    cclme = TRUE
)


## Animate Tracks by Date ----- ----------------
if(params$eov == 'sst'){
    title_eov <- 'sea surface temperature (SST)'
    subtitle_text_col <- 'snow'
        caption_iso <- 'The white line represents the 18°C isotherm. '
        eov_source <- 'NOAA Coral Reef Watch 5km Daily SST'
} else if(params$eov == 'chla'){
    title_eov <- 'chlorophyll-a (Chl)'
    subtitle_text_col <- 'gray8'
        caption_iso <- 'The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
        eov_source <- 'VIIRS Near Real-Time, DINEOF Gap-Filled 9km Daily Chl Concentration'
} else if(params$eov == 'current'){
    title_eov <- 'surface current flow'
    subtitle_text_col <- 'gray8'
        caption_iso <- 'The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
        eov_source <- 'Calculated from Near Real-Time Geostrohic Current (u, v) 0.2 degrees spatial res, Daily'
} else if(params$eov == 'ssta'){
    title_eov <- 'sea surface temperature anomaly (SSTA)'
    subtitle_text_col <- 'gray8'
    caption_iso <- ''
    eov_source <- 'NOAA Coral Reef Watch 5km Daily SSTA'
}


## use image magick
library(lubridate)
dates <- seq(as.Date("2023-07-16"), as.Date("2023-12-24"), by = "weeks")
dates <- seq(as.Date(min(turtles_df_weekly$date)), as.Date(max(turtles_df_weekly$date)-1), by = "weeks") # assumes week -1
# dates <- seq(as.Date(min(turtles_df_weekly$date)), as.Date(max(turtles_df_weekly$date)), by = "weeks") # assumes end of week

n = length(dates)

weekly_tracks_plot_list <- 
    lapply(seq(1,n), function(i) {

        # dt = c('2023-08-30')
        eov_test = eov_df_weekly %>% filter(date == dates[i]) 
        turtles_test = turtles_df_weekly %>% 
            # filter(id == '243178') %>% 
            filter(date <= dates[i]) 
        
        
        
        # gg <- 
            ggplot() +

        ## for continuous color pal---                    
            # # geom_tile(data = eov_test, 
            # geom_raster(data = eov_test,
            #             # aes(x = x, y = y, fill = val, group = date), interpolate = TRUE) +
            #             aes(x = x, y = y, fill = factor(cut(val, zCuts)), group = date), interpolate = TRUE) +
            
        ## for discrete color pal ---        
            geom_contour_filled(data = eov_test %>% subset(!is.na(val)) ,
                                    aes(x = x, y = y, z = val, group = date), breaks = seq(cbar_limits[1],cbar_limits[2],cbar_int)) + 
                
            
            {
                if (!is.null(tzcf_contour)) {
                    # add tzcf contour
                    geom_contour(data=eov_test, aes(x=x, y=y, z = val), colour = tzcf_color, linewidth = 1.25,
                                 breaks = c(tzcf_contour)) 
                }
            } +
            
            # # add tzcf contour
            # geom_contour(data=eov_df, aes(x=x, y=y, z = val), colour = "white", linewidth = 1.25,
            #              breaks = c(tzcf_contour)) +
            
            # add coast 
            geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
            
            {
                if (cclme){
                    # geom_polygon(data = cclme_df, aes(x = make360(long), y = lat.x, group = id), fill = 'gray75', alpha = 0.5)  # fyi, for long360 do not use id -- use group to group
                    geom_polygon(data = cclme_df, aes(x = make360(long), y = lat.x, group = group), fill = 'gray75', alpha = 0.5)  
                }
            } +
            
            # add state borders
            geom_sf(data = usa_360, color = "snow", fill = "black", size=0.5) +
            
            
            {
                if (eov =='chla'){
                    # scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
                    scale_fill_gradientn(colours = c( "gray99", cpal[6:length(cpal)]),
                                         breaks = cbar_breaks,
                                         limits = cbar_limits,
                                         na.value = 'snow',
                                         name = "Chl \n(mg/m^3) \n ")
                } else if (eov =='sst') {

                    # scale_fill_manual("SST (°C) \n", na.translate = F,
                    #                   values = cpal[7:length(cpal)], 
                    #                   # limits = c(6,34),  drop = FALSE,
                    #                   na.value="transparent",
                    #                   labels = seq(6,34,1)
                    # )
                    
                    ## discrete cpal (manual range set)
                    # scale_fill_manual(values = cpal[5:length(cpal)], name = "SST (°C) \n", 
                    #                   labels = c("≤ 6",seq(7,31, 1),"≥ 32"), drop = F, na.translate = F,
                    #                   guide = guide_legend(label.vjust=+1.2, barwidth = 1, #barheight = 32,
                    #                                        frame.colour = "black", ticks.colour = "black", ncol =1, reverse=T)) 
                    scale_fill_manual(values = cpal[7:length(cpal)], name = "SST (°C) \n", 
                                      labels = seq(4,30, 1), drop = F, na.translate = F,
                                      guide = guide_legend(label.vjust=+1.2, barwidth = 1, #barheight = 32,
                                                           frame.colour = "black", ticks.colour = "black", ncol =1, reverse=T)) 
                    
                    
                } else if (eov =='ssta') {
                    
                    scale_fill_manual(values = cpal[2:length(cpal)], name = "SSTA (°C) \n", 
                                      labels = seq(-5.5,6, 0.5), drop = F, na.translate = F,
                                      guide = guide_legend(label.vjust=+1.2, barwidth = 1, #barheight = 32,
                                                           frame.colour = "black", ticks.colour = "black", ncol =1, reverse=F)) 
                    
                    
                }
            } +
            
            
            
                # turtle daily movements -- wc pal
            geom_point(inherit.aes = FALSE, data=turtles_test %>% filter(date == dates[i]) %>%
                           mutate(lon = make360(lon)), #aes(x = lon, y = lat, colour =  as.factor(id)), size = size),
                       aes(x=lon,y=lat, color = as.factor(id)),#shape = 21,
                       stroke = 0.75, alpha = 0.90, size=plot_params$turtle_pt_size, show.legend=FALSE) +
                scale_colour_manual(values = rainbow(25)) +

            #     ## add daily pts border
            geom_point(inherit.aes = FALSE, data=turtles_test %>% filter(date == dates[i]) %>%
                           mutate(lon = make360(lon)),
                       # aes(x=lon,y=lat), color = "azure2",shape = 21,
                       aes(x=lon,y=lat, color = as.factor(id)), shape = 21,
                       stroke = 0.75, alpha = 0.90, size=plot_params$turtle_pt_size, show.legend=FALSE) +

        
        # ## add daily pts line
        geom_path(data = turtles_test %>%
                      mutate(lon = make360(lon)),
                  aes(x=lon,y=lat, color = as.factor(id),
                      group = as.factor(id)),
                  # alpha = 0.90,
                  linewidth = 2, show.legend = FALSE) +

            
            # release location
            geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
                       color = "black", shape = 4, size = 5.5) +
            
            labs(x = "\n Longitude \n", y = "\n \n Latitude \n ") +
            labs(subtitle = str_c("Week of: ", format(dates[i], "%b-%d-%Y"))) +
            # theme(legend.position = "none") +
            
            theme_minimal() + theme(text=element_text(size=plot_params$plot_text_size)) +
            # # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
            coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) +
            # guides(fill = guide_colourbar(
            #     barheight = plot_params$barheight,
            #     ticks = TRUE)
            # ) + 
            guides(fill = guide_legend(title = 'SST (°C)', ncol=1, reverse=T, barheight = plot_params$barheight, limits = cbar_breaks,
                                       ticks = TRUE)) + 
            
            # guides(fill = guide_colorsteps(direction = "horizontal",
            #                                    barwidth = unit(par("pin")[1], "in"))) +
            # theme(legend.position = "bottom") +
                
            # facet_wrap(~date) +
            
                
          
            # anim_trial = gg_static + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
                labs(title = str_c("STRETCH Weekly turtle movements (n=25) with ", title_eov),
                     # subtitle = "Date: {frame_time}", 
                     caption = str_c("\n Raw tracking data from ARGOS averaged to 1 weekly location per turtle (circles)\n", "California Current Large Marine Ecosystem (CCLME) shaded in gray\n", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
                # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
                theme(
                    # element_text(size=p_plot_text_size),
                    plot.title = element_text(size=plot_params$title_size, face="bold", margin=margin(t=20,b=0), hjust=0.03),
                    plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
                    plot.caption = element_text(size=plot_params$caption_size),
                    legend.key = element_rect(color="snow", fill = 'white'), legend.key.size=unit(13,"point"),
                    plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
                
                NULL
            # # +
            #     theme(legend.key = element_rect(color="gray80", fill = 'white'))
        
    })

# name the elements in the list
wk_names <- str_c("plot_", unique(dates))
names(weekly_tracks_plot_list) <- wk_names

    

# save plots as pngs (to create gif)
save_figs = TRUE

if(save_figs){
    lapply(seq(1:length(wk_names)), function(i) { ggsave(weekly_tracks_plot_list[[i]], 
                                                         file = str_c('./anim_figs/', i, "_", dates[i], "_", eov, "_tracks.png"), 
                                                         width=14, 
                                                         # height=8.5,   # comment out height to get auto aspect ratio set!
                                                         bg = 'white')
    })
}



# create movie
library(magick)

# List plots files
plot_files <- list.files('./anim_figs', pattern = "_tracks",full.names=T) #%>% list.files('./anim_figs', pattern = eov, full.names=T)
pfiles <- plot_files[order(basename(plot_files))]

# arrange by month order
# lsfiles <- file.info(plot_files)
# pfiles <- lsfiles[order(lsfiles$mtime),]

library(gtools)
pfiles <- mixedsort(pfiles)


# #intermediate step to crop PNG file and reduce negative space
# library(magick)
# m_png <- lapply(seq(1:length(pfiles)), function(i) {image_border(image_trim(image_read(pfiles[i])), 
#                       "white", "30x30")
#                       })
# lapply(seq(1:length(pfiles)), function(i) {image_write(image=m_png[[i]], str_c(pfiles[i]), format='png')})


## use crop image source function ------
source('code/crop_image.R')
crop_image(pfiles)

## --------------------------------------

# Read the plots as image
plots_list <- lapply(pfiles, image_read)

# Join the list of plots
plot_joined <- image_join(plots_list)

# Create animation, defining the frames per second (fps)
plot_animated <- image_animate(plot_joined, fps = 2)

# Write animation as gif
image_write(image = plot_animated,
            path = str_c('./anim_figs/', "stretch_2023_weekly_animation_", dates[2], "_", eov, ".gif"))


## Use CONVERTIO online for mp4 generation


# library(av)
# list_of_frames <- pfiles
# av::av_encode_video(list_of_frames, framerate = 1,
#                     output = "stretch_2023_weekly_animation_24Dec2023.mp4")
# 
# 
# image_write_video(image = plot_animated,
#                   path = str_c('./anim_figs/', "stretch_2023_weekly_animation_24Dec2023.mp4",vfilter = "scale=-2:-2"))
# 
# # av::av_encode_video(list.files('./anim_figs', pattern = "_tracks.png",full.names=T), framerate = 30,
# #                     output = 'test.mp4')




