# animate_daily_cc_sla_uv.r

# alt script for animate daily cc eov, for currents (sla and uv components) only



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
    filter(lat > 0) %>%
    filter(date >= '2023-07-11 04:30:00')

head(raw_data)

daily_avg_data <- raw_data %>%
    mutate(date = as.Date(date)) %>%
    group_by(id, date) %>%
    summarise(lat = mean(lat), 
              lon = mean(lon))

# head(daily_avg_data)


## ----load-eov-ncs-------------------------------------------------------------------------------------------------------

## need a longer term resolution to download this via api. for now just use manual erddap dl: 'https://coastwatch.pfeg.noaa.gov/erddap/griddap/miamicurrents.html?u_current%5B(2023-08-01T00:00:00Z)%5D%5B(-64.7):(64.7)%5D%5B(-179.9):(179.9)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cu_current&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff'
params <- c()
params$eov = 'sla_uv'

params$nc_path <- "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"
params$varname <- c('sla', 'ugos', 'vgos')  # special case. multiple vars inside ncdf

# params$eov <- params$varname
ncs <- list.files(path= params$nc_path, pattern =  params$varname, full.names=T)
print(str_c('most recent ncdf - ', ncs[length(ncs)]))

# # ncs <- '/Users/briscoedk/Downloads/miamicurrents_2b23_77c4_7608.nc'
# # will need to fix fdates when start batch downloading with api. for now, it was a bulk download from erddap
# ncIn <- sapply(1:length(ncs), function(x) ncs[grepl(ncs[x],ncs)])
# 

fdates <- ncs %>% 
    str_split(., "_") %>%
    purrr::map_chr(~ pluck(., 4)) %>%
    substr(., start=1, stop=10)


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
eov_dates <- fdates[as.Date(unique(fdates)) > as.Date('2023-07-10')] 
# deploy_date <- as.Date("2023-07-10")

# daily_dates <- c(deploy_date, tracking_dates)
# daily_dates <- c(deploy_date, eov_dates)
daily_dates <- intersect(as.character(tracking_dates), as.character(eov_dates)) 


ncIn <- sapply(1:length(daily_dates), function(x) ncs[grepl(daily_dates[x],ncs)])

# convert ncs to raster stack
ras <- {suppressWarnings(raster::stack(ncIn, varname = params$varname[1]))}

# if(params$eov == 'sla_uv'){
#     nc_dates <- ncIn %>% 
#         map(~parseDT(., idx = 6, start=37, stop = 46, format="%Y-%m-%d")) %>%
#         unlist() %>% as.Date() #%>%
# } 
# names(ras) <- nc_dates


#' 
## ----crop-ras-extent----------------------------------------------------------------------------------------------------
ras_subset <- crop(ras, e_subset_180)

# convert gridded SLA raster data dataframe
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


if(params$eov == 'sla_uv'){
    eov_df <- g_df_subset %>%
        mutate(val = round(val, 2)) %>%
        mutate(date = as.Date(date)) #%>% filter(date == '2023-07-11')
} 
# # get uv vectors
# ret_uv.se<- prep_uv_geostrophic(x = ncs)        

wind.date <- wind.date %>% filter(date > '2023-07-10')
# 
# # rename for consistency
# uv.se <- ret_uv.se %>%
#     # uv.se <- uv.se %>% 
#     mutate(date = as.Date(date)) #%>%
# # filter(date >= '2023-07-11')


## prep plot data
# eov_df  = uv.se

turtles_df = turtlesgeo %>% filter(date %in% daily_dates) 

release_loc = data.frame(lat=39.315, lon=213.9333)

# ret_uv.se_360 <- ret_uv.se %>% mutate(lon = make360(lon))
turtles_df_hauoli <- turtles_df %>% filter(id == '243194') #%>% filter(date > '2023-08-04')

# 
# turtles_df_hauoli <- turtles_df_hauoli %>% filter(date <= max(wind.date$date))

# metR
gg_currents <- 
    ggplot() +
    # geom_raster(data = wind.date, aes(x = lon, y = lat, fill = velocity), interpolate = TRUE) +
    geom_raster(data = eov_df, aes(x = x, y = y, fill = val), interpolate = TRUE) +
    metR::geom_vector(data = wind.date ,  
                      aes(x = lon, y = lat, dx = u, dy = v), 
                      arrow.angle = 30, arrow.type = "open", arrow.length = 1.25, 
                      show_guide = TRUE, 
                      pivot = 0,preserve.dir = TRUE, direction = "cw", color = 'snow') +
    scale_mag(max = 0.25, name = "", max_size = 0.7)+
    
    scale_fill_viridis_c(name = "SSH above \nsea level (m)\n",#colours = cpal, 
                         alpha = 0.9,
                         label = function(x) sprintf("%.2f", x),
                         # limits = c(0, 0.25), # m/s
                         # breaks = seq(0, 0.2, 0.05)) +
                         limits = c(-0.1, 0.35), # km/hr
                         breaks = seq(-0.1, 0.35, 0.05)) +
    scale_alpha(guide = "none") +
    
    
    geom_point(data=
                   # turtles_df_hauoli %>% 
                   turtles_df %>%
                   mutate(date = as.Date(date)) %>%
                   filter(date <= max(wind.date$date)) %>%
                   mutate(lon = make180(lon)),
               aes(x=lon,y=lat), color = "gray90",
               # fill = "#2a9d8f", shape = 21,
               fill = "orange", shape = 21,
               # stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
               stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size-1) +
    
    # release location
    geom_point(data=release_loc, aes(x=make180(lon), y=lat), fill = "black",
               color = "black", stroke = 1, shape = 4, size = 7.5) +
    
    
    labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ") +
    # coord_sf(ylim = c(35, 50), xlim =  c(-159, -141), expand = FALSE, crs = st_crs(4326)) +
    coord_sf(ylim = c(37.75, 43.75), xlim =  c(-154, -142), expand = FALSE, crs = st_crs(4326)) +
    
    theme_bw()+
    theme(axis.text = element_text(size = 14, colour = 1),
          legend.text = element_text(size = 14, colour = 1), 
          legend.title = element_text(size = 14, colour = 1)#,
          # legend.position = c(.12,.17),
          # legend.background = element_rect(colour = 1, fill = "white")
    ) +
    guides(fill = guide_colourbar(
        barheight = plot_params$barheight,
        ticks = TRUE)
    ) + 
    # facet_wrap(~date) +
    NULL

## animate ---
title_eov <- 'sea surface height'
subtitle_text_col <- 'gray8'
    caption_iso <- '' #'The orange circle is the current date-location. The darker circles are the previous locations. ' #The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
    eov_source <- 'Copernicus Daily, 0.25 degree Global Ocean Gridded L4 Sea surface heights above sea level (m). \n Surface velocity (white arrows) are calculated from u, v geostrophic components.'
    
    
    
    anim_trial <- gg_currents +     
        # labs(title = str_c("STRETCH Daily turtle movements of Turtle Hau'oli (#17) with ", title_eov),
        labs(title = str_c("STRETCH Daily  movements of all turtles (n = 25) with ", title_eov),
             subtitle = "Date : {frame_time}",
             caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location (orange circles). ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
        # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
        theme(
            # element_text(size=p_plot_text_size),
            # plot.title = element_text(size=plot_params$title_size, face="bold", margin=margin(t=20,b=0), hjust=0.03),
            # plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
            plot.title = element_text(size=plot_params$title_size, face="bold"), #, margin=margin(t=20,b=0), hjust=0.03),
            plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", color=subtitle_text_col), #, margin=margin(t=30,b=-30), hjust=0.025),
            
            plot.caption = element_text(size=plot_params$caption_size),
            plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
        guides(fill = guide_colourbar(
            barheight = plot_params$barheight
        )) +
        transition_time(date) +
        ease_aes("linear") +
        
        shadow_wake(wake_length=0.35, alpha = 0.7, wrap=FALSE,
                    colour = "gray60", fill = "orange",
                    falloff = 'sine-in', exclude_phase = 'enter', size=0.35,
                    exclude_layer = c(1,2, length(daily_dates))
        ) +
        enter_fade() +
        exit_disappear() +  
        
        NULL
    
    gganimate::animate(anim_trial, nframes = length(daily_dates), fps =2, 
                       # width = 1460, height = 720, res = 104,
                       width = 1640, height = 900, res = 104,
                       renderer = av_renderer(str_c('~/Downloads/dbriscoe_animation_trial_', params$eov,'_full_contours_v3Aug_interp_kmh_full.mp4')))
    
    
    
    ## ZOOMEDview -------------------------------------------------------------------------
    # metR
    gg_currents_zoom <- 
        ggplot() +
        # geom_raster(data = wind.date, aes(x = lon, y = lat, fill = velocity), interpolate = TRUE) +
        geom_raster(data = eov_df %>% 
                            # filter(date > '2023-08-04') %>%
                        mutate(x = make360(x)), aes(x = x, y = y, fill = val), interpolate = TRUE) +
        metR::geom_vector(data = wind.date %>% 
                              # filter(date > '2023-08-04') %>%
                          mutate(x = make360(lon)),  
                          aes(x = lon, y = lat, dx = u, dy = v), 
                          arrow.angle = 30, arrow.type = "open", arrow.length = 1.0, 
                          show_guide = TRUE, 
                          pivot = 0,preserve.dir = TRUE, direction = "cw", color = 'gray10') +
        scale_mag(name = "Velocity (m/s)",
                  max = 0.2, 
                  # max_size = 0.7
                  )+
        
        scale_fill_viridis_c(name = "SSH above \nsea level (m)\n",#colours = cpal, 
                             alpha = 0.9,
                             label = function(x) sprintf("%.2f", x),
                             # limits = c(0, 0.25), # m/s
                             # breaks = seq(0, 0.2, 0.05)) +
                             limits = c(0, 0.25), # km/hr
                             breaks = seq(-0, 0.25, 0.05)) +
        scale_alpha(guide = "none") +
        

        geom_point(data=turtles_df_hauoli %>%
                       mutate(date = as.Date(date)) %>%
                       mutate(lon = make360(lon)
                              ),
                   aes(
                       # x=lon,
                       x=make360(lon),
                       y=lat
                       ), color = "gray90",
                   # fill = "#2a9d8f", shape = 21,
                   fill = "orange", shape = 21,
                   stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size + 5) +


        # release location
        geom_point(data=release_loc, 
                   aes(
                       # x=make180(lon),
                       x=lon,
                       y=lat
                       ), fill = "black",
                   color = "black", stroke = 1, shape = 4, size = 7.5) +


        labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ") +
        ## coord_sf(ylim = c(35, 41), xlim =  c(-159, -141), expand = FALSE, crs = st_crs(4326)) +
        # coord_sf(ylim = c(37.5, 39.5), xlim =  c(-148, -144), expand = FALSE, crs = st_crs(4326)) +
        
        coord_sf(xlim = c(make360(-148), make360(-144)), ylim = c(37.5, 39.5), expand = FALSE, crs = st_crs(4326)) +
        
        theme_bw()+
        theme(axis.text = element_text(size = 14, colour = 1),
              legend.text = element_text(size = 14, colour = 1), 
              legend.title = element_text(size = 14, colour = 1)#,
              # legend.position = c(.12,.17),
              # legend.background = element_rect(colour = 1, fill = "white")
        ) +
        guides(fill = guide_colourbar(
            barheight = plot_params$barheight,
            ticks = TRUE)
        ) + 
        # facet_wrap(~date) +
        NULL
    
    
    ## animate ---
    title_eov <- 'surface currents'
    subtitle_text_col <- 'gray8'
        caption_iso <- '' #'The orange circle is the current date-location. The darker circles are the previous locations. ' #The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
        eov_source <- 'Copernicus Daily, 0.25 degree Global Ocean Gridded L4 Sea surface heights above sea level (m). \n Surface velocity (arrows) are calculated from u, v geostrophic components.'
        
        
        
    anim_trial_zoom <- gg_currents_zoom +     
        labs(title = str_c("STRETCH Daily turtle movements of Turtle Hau'oli (#17) with ", title_eov),
             subtitle = "Date : {frame_time}",
             caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location (orange circles). ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
        # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
        theme(
            # element_text(size=p_plot_text_size),
            # plot.title = element_text(size=plot_params$title_size, face="bold", margin=margin(t=20,b=0), hjust=0.03),
            # plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
            plot.title = element_text(size=plot_params$title_size, face="bold"), #, margin=margin(t=20,b=0), hjust=0.03),
            plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", color=subtitle_text_col), #, margin=margin(t=30,b=-30), hjust=0.025),
            
            plot.caption = element_text(size=plot_params$caption_size),
            plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
        guides(fill = guide_colourbar(
            barheight = plot_params$barheight
        )) +
        transition_time(date) +
        ease_aes("linear") +
        
        shadow_wake(wake_length=0.5, alpha = 0.2, wrap=FALSE,
                    falloff = 'sine-in', exclude_phase = 'enter', size=0.75,
                    exclude_layer = c(1,2,length(daily_dates))
        ) +
        enter_fade() +
        exit_disappear() +  
        
        NULL
    
        gganimate::animate(anim_trial_zoom, nframes = length(daily_dates), fps =2, 
                           # width = 1460, height = 720, res = 104,
                           width = 1640, height = 900, res = 104,
                           renderer = av_renderer(str_c('~/Downloads/dbriscoe_stretch_animation_turtle17_hauoli_', params$eov,'_5Aug23zoom.mp4')))
        
        



### OLD =================

# # dt <- seq.Date(min(uv.se$date)+1,max(uv.se$date), by=1)
# dt <- seq.Date(min(uv.se$date),max(uv.se$date), by=1)
# dt_str <- format(`dt`)
# 
# # >>>>>>>> TO UPDATE FUNCT WITH THIS INFO
# # save_ext <- 'gif'
# save_ext <- 'mp4'
# 
# if(save_ext == 'mp4'){
#     # plot_params <- list(
#     #     turtle_pt_size = 3.25,
#     #     barheight = 28.5, #38
#     #     plot_text_size = 14,
#     #     title_size = 18,
#     #     subtitle_size = 16,
#     #     caption_size = 14
#     # )  
#     plot_params <- list(
#         turtle_pt_size = 6.25,
#         barheight = 28-1,
#         plot_text_size = 22,
#         title_size = 16,
#         subtitle_size = 16,
#         caption_size = 12
#     )
# }
# if(save_ext == 'gif'){
#     plot_params <- list(
#         turtle_pt_size = 5.25,
#         barheight = 34,
#         plot_text_size = 22,
#         title_size = 24,
#         subtitle_size = 22,
#         caption_size = 20
#     )
# }
# 
# eov_df <- uv.se
# 
# gg_static_currents <- get_static_currents_plot(
#     eov = params$eov,
#     eov_df = eov_df,
#     turtles_df = turtles_df_hauoli,
#     e,
#     release_loc,
#     cpal = cpal,
#     cbar_breaks,
#     cbar_limits,
#     plot_params
# )
# 
# ##
# title_eov <- 'surface currents'
# subtitle_text_col <- 'gray8'
#     caption_iso <- 'The orange circle is the current date-location. The darker circles are the previous locations. ' #The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
#     eov_source <- 'Velocity & vectors (white arrows) are calculated from Daily Near Real-Time Geostrophic Current (u, v), 0.2 degrees spatial res, NOAA CoastWatch'
# 
#     # gg_static_currents +
#     #     labs(title = str_c("STRETCH Daily turtle movements of Turtle Hau'oli (#17) with ", title_eov),
#     #          # subtitle = i,
#     #          caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location (circles) \n ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
#     #     # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
#     #     theme(
#     #         # element_text(size=p_plot_text_size),
#     #         # plot.title = element_text(size=plot_params$title_size, face="bold", margin=margin(t=20,b=0), hjust=0.03),
#     #         # plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
#     #         plot.title = element_text(size=plot_params$title_size, face="bold"), #, margin=margin(t=20,b=0), hjust=0.03),
#     #         plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", color=subtitle_text_col), #, margin=margin(t=30,b=-30), hjust=0.025),
#     # 
#     #         plot.caption = element_text(size=plot_params$caption_size),
#     #         plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
#     #     guides(fill = guide_colourbar(
#     #         barheight = plot_params$barheight
#     #     ))
#     
# anim_trial = gg_static_currents + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
#     labs(title = str_c("STRETCH Daily turtle movements (n=25) with ", title_eov),
#          subtitle = "Date: {frame_time}", 
#          labs(title = str_c("STRETCH Daily turtle movements of Turtle Hau'oli (#17) with ", title_eov),
#               # subtitle = i, 
#               caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location (circles) \n ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
#              # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
#              theme(
#                  # element_text(size=p_plot_text_size),
#                  # plot.title = element_text(size=plot_params$title_size, face="bold", margin=margin(t=20,b=0), hjust=0.03),
#                  # plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
#                  plot.title = element_text(size=plot_params$title_size, face="bold"), #, margin=margin(t=20,b=0), hjust=0.03),
#                  plot.subtitle = element_text(size = plot_params$subtitle_size, face="bold", color=subtitle_text_col), #, margin=margin(t=30,b=-30), hjust=0.025),
#                  
#                  plot.caption = element_text(size=plot_params$caption_size),
#                  plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
#              # guides(fill = guide_colourbar(
#              #     barheight = plot_params$barheight
#              # ))
#     
#     shadow_wake(wake_length=0.5, alpha = 0.2, wrap=FALSE,
#                 falloff = 'sine-in', exclude_phase = 'enter', size=0.75,
#                 exclude_layer = c(1,2, length(daily_dates))
#     ) +
#     enter_fade() +
#     exit_disappear() +  
#     
#     NULL
# 
#     
#     
#     
#     
# # get static plot (for gganimate) --
# get_static_currents_plot <- function(eov, eov_df, turtles_df, e, release_loc, cpal = cpal, cbar_breaks, cbar_limits, plot_params){
#     
#     mapdata <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) %>%
#         filter(long >= 120 & long <= 270 & lat >= 15 & lat <=80) 
#     
#         tzcf_contour = NULL
# 
#     
#     gg <- 
#         ggplot() +
#         metR::geom_contour_fill(data = uv.se,
#                                 aes(x = lon, y = lat, z = vel), na.fill = TRUE, bins = 70) +
#         metR::geom_vector(data = uv.se,
#                           aes(x = lon, y = lat, dx = u, dy = v), 
#                           arrow.angle = 30, arrow.type = "open", arrow.length = 1.25, 
#                           show_guide = FALSE, 
#                           pivot = 0,preserve.dir = TRUE, direction = "ccw", color = 'snow')+
#         scale_mag(max = 0.15, name = "", max_size = 0.7)+
#         # geom_sf(data = wio,fill = "lightgrey", col = "black")+
#         # coord_sf(ylim = c(-15,-6), xlim = c(39, 52))+
#         # coord_sf(ylim = c(35,42), xlim = c(-155, -140), expand = FALSE, crs = st_crs(4326)) +
#         scale_fill_viridis_c(name = "Velocity\n(m/s)",#colours = cpal, 
#                              limits = c(0, 0.25),
#                              breaks = seq(0, 0.2, 0.05)) +
#         scale_alpha(guide = "none") +
#         # theme_bw()+
#         # theme(legend.position = "right",
#         #       legend.key.height = unit(1.4, "cm"),
#         #       legend.background = element_blank(),
#         #       axis.text = element_text(size = 12, colour = 1)) +
#         labs(x = "", y = "") +
#         
#         # pts up to current
#         geom_point(data=turtles_df %>% 
#                        mutate(date = as.Date(date)) %>%
#                        # filter(date <= i) %>%
#                        mutate(lon = make180(lon)),
#                    aes(x=lon,y=lat), color = "gray90",
#                    # fill = "#2a9d8f", shape = 21,
#                    fill = "black", shape = 21,
#                    stroke = 0.5, alpha = 0.90, size=plot_params$turtle_pt_size-1.5) +
#         
#         geom_line(data=turtles_df %>%
#                       # filter(date <= i) %>%
#                       mutate(lon = make180(lon)),
#                   aes(x=lon,y=lat, group=1), color = "gray20", alpha = 0.40, linewidth=1.25)+
#         
#         # current frame
#         # pts up to current
#         geom_point(data=turtles_df %>% 
#                        mutate(date = as.Date(date)) %>%
#                        # filter(date == i) %>%
#                        mutate(lon = make180(lon)),
#                    aes(x=lon,y=lat), color = "gray90",
#                    # fill = "#2a9d8f", shape = 21,
#                    fill = "orange", shape = 21,
#                    stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
#         
#         geom_line(data=turtles_df %>%
#                       # filter(date == i) %>%
#                       mutate(lon = make180(lon)),
#                   aes(x=lon,y=lat, group=1), color = "gray20", alpha = 0.40, linewidth=1.25)+
#         
#         # release location
#         geom_point(data=release_loc, aes(x=make180(lon), y=lat), fill = "black",
#                    color = "black", shape = 4, size = 6.5) +
#         
#         labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ",
#              subtitle = paste0('Date: ', i)) +
#         
#         
#         coord_sf(ylim = c(35,42), xlim = c(-155, -140), expand = FALSE, crs = st_crs(4326)) +
#         theme_bw()+
#         theme(legend.position = "right",
#               legend.key.height = unit(1.4, "cm"),
#               legend.background = element_blank(),
#               axis.text = element_text(size = 12, colour = 1)) +
#         guides(fill = guide_colourbar(
#             barheight = plot_params$barheight,
#             ticks = TRUE)
#         ) + 
#         
#         # facet_wrap(~date) +
#         NULL
#     
#     return(gg)
# }