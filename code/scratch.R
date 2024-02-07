## scratch

# loess 
```{r gg-smooth}
gg_smooth <- 
    ggplot() +
    geom_tile(
        data = sst_df %>% mutate(date = as.Date(date)),
        aes(x = x, y = y, fill = val, group = date)) +
    # # geom_point(data=sst_df %>% mutate(date = as.Date(date)) %>% filter(val == 17),aes(x=x,y=y, group=1), color="black", alpha = 0.40, size=2.25)+
    # geom_line(data=sst_df %>% mutate(date = as.Date(date)) %>% filter(val == 17),aes(x=x,y=y, group=1), color="snow", alpha = 0.40, size=1.25)+
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
    geom_point(data=turtles_df %>% mutate(lon = make360(lon)),aes(x=lon,y=lat, group=id), color="black", alpha = 0.40, size=4.25)+
    geom_path(data=turtles_df %>% mutate(lon = make360(lon)),aes(x=lon,y=lat, group=id), color="black", alpha = 0.40, size=2.25)+
    
    # release location
    geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
               color = "black", shape = 4, size = 7.5)+
    # 
    labs(x = "Longitude \n", y = "Latitude \n") +
    # theme(legend.position = "none") +
    
    theme_minimal() + theme(text=element_text(size=18)) +
    # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 50), expand = FALSE, crs = st_crs(4326))    labs(x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
    guides(fill = guide_colourbar(title = "SST (°C)", barheight = 38,
                                  ticks = TRUE)) + 
    # facet_wrap(~date)
    NULL
```


## 
```
{r anim-smooth}

## Animate Tracks by Date ----- ----------------
anim_gg_smooth = gg_smooth + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
    labs(title = "STRETCH average daily turtle movements (n=25) and sea surface temperature (SST)",
         subtitle = "Date: {frame_time}", 
         caption = "Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
    # shadow_mark(alpha = 0.3, size = 0.25) +
    shadow_wake(wake_length = 0.75, alpha = TRUE, exclude_layer = c(1,2, length(daily_dates))) +
    enter_fade() +
    exit_disappear() +  
    # ease_aes('sine-in-out') +
    # view_follow(fixed_y = FALSE) +
    # transition_states(states=date, transition_length = 1, state_length = 1, wrap = F) +
    # # shadow_wake(wake_length = 1)
    NULL

gganimate::animate(anim_gg_smooth, nframes = length(daily_dates), fps =1, 
                   detail = 5,
                   # height = 4, #width = 3000,
                   #  # height = 700, #width = 2000, 
                   # units = "in", res=150,
                   width = 1400, height = 865,
                   renderer = gifski_renderer(loop = TRUE))
```

expression('No. of'~italic(bacteria X)~'isolates with corresponding types')

test = expression(paste(italic("Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n", 'Dana Briscoe')))
test = paste("Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n", expression(italic('Dana Briscoe')))

test = bquote('Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n' ~ italic('Dana Briscoe'))


## shadow trail
anim <- ggplot(airquality, aes(Day, Temp, colour = factor(Month))) +
    geom_point() +
    # geom_path()+
    transition_time(Day)

# Change distance between points
anim1 <- anim +
    shadow_trail(0.02)

# Style shadow differently
anim2 <- anim +
    shadow_trail(alpha = 0.3, shape = 2)

# Restrict the shadow to 10 frames
anim3 <- 
    anim +
    # shadow_trail(max_frames = 10)
    shadow_wake(wake_length=0.95, alpha = 0.8, wrap=FALSE, colour = 'grey92')


https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNnoaaSNPPnoaa20NRTchlaGapfilledDaily.nc?chlor_a[(2023-07-10T12:00:00Z):1:(2023-07-10T12:00:00Z)][(50):1:(20)][(-160):1:(-110)]

https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNnoaaSNPPnoaa20NRTchlaGapfilledDaily.nc?chlor_a  [(2023-07-10T12:00:00Z):1:(2023-07-10T12:00:00Z)][([(0):1:(0)][(50):1:(20)][(-160):1:(-110)]
https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNnoaaSNPPnoaa20NRTchlaGapfilledDaily.nc?chlor_a%5B(2023-07-10T12:00:00Z):1:(2023-07-10T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(50.04166):1:(20.04166)%5D%5B(-159.9583):1:(-110.0417)%5D



# convert rmd to r script ----
knitr::purl('./code/animate_daily_cc_eov.Rmd', './code/animate_daily_cc_eov.r', documentation = 2)



node: "swfsc" 
url: "https://coastwatch.pfeg.noaa.gov/erddap/griddap/"
eov: "chla"
varname: "chlor_a"
enddate: !r Sys.Date() - 2
startdate: !r as.Date('2023-07-10')
timestep: "day"
nc_path: "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/deploy_reports"
bbox: !r tibble::tibble(ymin=20, ymax=50,xmin=-160, xmax=-110)


https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.nc?CRW_SSTANOMALY[(2023-07-10 T12:00:00Z):1:(2023-07-10 T12:00:00Z)][(50):1:(20)][(-180):1:(-110)]'
# https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.nc?CRW_SSTANOMALY%5B(2023-07-13T12:00:00Z):1:(2023-07-13T12:00:00Z)%5D%5B(50.025):1:(25.025)%5D%5B(-179.975):1:(-109.975)%5D'




## set up ggplot funcs for animations

## GGPLOT STATIC  --------------------------------------------------------------------------------------------------------------------
# set params:
if(params$eov == 'sst'){
    test_eov_df <- g_df_subset %>% mutate(date = as.Date(date)) %>% 
        filter(date == '2023-07-11') #%>%
        # mutate(val = ifelse(val >= '17' & val <= 17.2, NA, val))
} else if(params$eov == 'chla'){
    test_eov_df <- g_df_subset %>% mutate(date = as.Date(date))  %>% filter(date == '2023-07-11') %>% mutate(val = log(val))
}

test_turtles_df = turtlesgeo %>% filter(date %in% nc_dates) %>% filter(date == '2023-07-11')

release_loc = data.frame(lat=39.315, lon=213.9333)

params$barheight = 28.5 #38
params$plot_text_size = 14

test_cpal = c(smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9)), "#9e2a2b", "firebrick4", "#540b0e", "#540b0e")
cbar_breaks = seq(6, 34, 2)
cbar_limits = c(4, 34)

test_cpal = oce::oceColorsChlorophyll(20); cpal = test_cpal
cbar_breaks = c(0,0.1, 0.4, 2, 10, 20)
cbar_limits = c(0,20)
limits = c(0,20)
tzcf_contour = 0.2

test_eov_df <- test_eov_df %>% mutate(val = ifelse(val < 0, NA, val))


gg_static <- get_static_plot(eov = params$eov, eov_df = test_eov_df, turtles_df = test_turtles_df, e, cpal = test_cpal, cbar_breaks, cbar_limits)

# # test function ----
# get_static_plot <- function(eov_df = test_eov_df, turtles_df = test_turtles_df, e, cpal = cpal, cbar_breaks, cbar_limits, tzcf_contour){
# 
# gg <- 
#     ggplot() +
#     geom_tile(
# 
#         data = eov_df,
#         aes(x = x, y = y, fill = val, group = date)) +
# 
#     
#     {
#         if (!is.null(tzcf_contour)) {
#             # add tzcf contour
#             geom_contour(data=eov_df, aes(x=x, y=y, z = val), colour = "white", linewidth = 1.25,
#                          breaks = c(tzcf_contour)) 
#         }
#     } +
#     
#     # # add tzcf contour
#     # geom_contour(data=eov_df, aes(x=x, y=y, z = val), colour = "white", linewidth = 1.25,
#     #              breaks = c(tzcf_contour)) +
# 
#     # add coast 
#     geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
#     
# 
#     {
#         if ( params$eov=='chla'){
#             scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
#                               breaks = seq(0,1.8,0.1),
#                               # limits = c(0, 2.0),
#                               na.value = 'snow',
#                               name = "Chl (mg/m^3)")
#         } else if ( params$eov=='sst') {
#             scale_fill_gradientn(colours = 
#                                      # cpal[12:length(cpal)],
#                                      # breaks=seq(10,25,2),
#                                      # limits = c(9.25,25),
#                                  cpal[9:length(cpal)],
# 
#                                  breaks=cbar_breaks, #seq(6,32,2),
#                                  limits = c(min(cbar_limits),max(cbar_limits)),
#                                  na.value = 'snow',
#                                  name = "SST (°C)")
#         }
#     } +
#     
#     
#     # turtle daily movements
#     geom_point(data=turtles_df %>% 
#                         mutate(lon = make360(lon)),
#                     aes(x=lon,y=lat), color = "azure2",
#                      # fill = "#2a9d8f", shape = 21,
#                     fill = "#3c096c", shape = 21,
#                     stroke = 1, alpha = 0.90, size=3.25) +
# 
#     # release location
#     geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
#                color = "black", shape = 4, size = 5.5) +
# 
#     labs(x = "\n \n Longitude \n", y = "\n Latitude \n \n ") +
#     # theme(legend.position = "none") +
#     
#     theme_minimal() + theme(text=element_text(size=params$plot_text_size)) +
#     # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
#     coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) +
#     guides(fill = guide_colourbar(#title = "SST (°C)", 
#                                   barheight = params$barheight,
#                                   ticks = TRUE)) + 
#     # facet_wrap(~date) +
#     NULL
# 
# return(gg)
# }
# 

# get static plot (for gganimate) ----
get_static_plot <- function(eov, eov_df, turtles_df, e, release_loc, cpal = cpal, cbar_breaks, cbar_limits){
    
    mapdata <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) %>%
        filter(long >= 120 & long <= 270 & lat >= 15 & lat <=80) 
    
    p_barheight = 28.5 #38
    p_plot_text_size = 14
    
    if(eov == 'sst'){
        tzcf_contour = 17
        tzcf_color = 'white'
    } else if(eov == 'chla'){
        tzcf_contour = 0.2
        tzcf_color = 'gray10'
    } else if(eov == 'ssta'){
        tzcf_contour = NULL
    }
    
    gg <- 
        ggplot() +
        geom_tile(
            
            data = eov_df,
            aes(x = x, y = y, fill = val, group = date)) +
        
        
        {
            if (!is.null(tzcf_contour)) {
                # add tzcf contour
                geom_contour(data=eov_df, aes(x=x, y=y, z = val), colour = tzcf_color, linewidth = 1.25,
                             breaks = c(tzcf_contour)) 
            }
        } +
        
        # # add tzcf contour
        # geom_contour(data=eov_df, aes(x=x, y=y, z = val), colour = "white", linewidth = 1.25,
        #              breaks = c(tzcf_contour)) +
        
        # add coast 
        geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
        
        
        {
            if (eov=='chla'){
                scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
                                  breaks = seq(0,1.8,0.1),
                                  # limits = c(0, 2.0),
                                  na.value = 'snow',
                                  name = "Chl (mg/m^3)")
            } else if (eov=='sst') {
                scale_fill_gradientn(colours = 
                                         # cpal[12:length(cpal)],
                                         # breaks=seq(10,25,2),
                                         # limits = c(9.25,25),
                                         cpal[9:length(cpal)],
                                     
                                     breaks=cbar_breaks, #seq(6,32,2),
                                     limits = c(min(cbar_limits),max(cbar_limits)),
                                     na.value = 'snow',
                                     name = "SST (°C)")
            }
        } +
        
        
        # turtle daily movements
        geom_point(data=turtles_df %>% 
                       mutate(lon = make360(lon)),
                   aes(x=lon,y=lat), color = "azure2",
                   # fill = "#2a9d8f", shape = 21,
                   fill = "#3c096c", shape = 21,
                   stroke = 1, alpha = 0.90, size=3.25) +
        
        # release location
        geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
                   color = "black", shape = 4, size = 5.5) +
        
        labs(x = "\n \n Longitude \n", y = "\n Latitude \n \n ") +
        # theme(legend.position = "none") +
        
        theme_minimal() + theme(text=element_text(size=p_plot_text_size)) +
        # # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
        coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) +
        guides(fill = guide_colourbar(
            barheight = p_barheight,
            ticks = TRUE)
        ) + 
        # facet_wrap(~date) +
        NULL
    
    return(gg)
}

# try saving ----
## Animate Tracks by Date ----- ----------------
if(params$eov == 'sst'){
    title_eov <- 'sea surface temperature (SST)'
    subtitle_text_col <- 'snow'
        caption_iso <- 'The white line represents the 17°C isotherm. '
        eov_source <- 'NOAA Coral Reef Watch 5km Daily SST'
} else if(params$eov == 'chla'){
    title_eov <- 'chlorophyll-a (Chl)'
    subtitle_text_col <- 'gray8'
        caption_iso <- 'The 0.2 mg/m^3 isopleth represents the approximate TZCF position'
        eov_source <- 'VIIRS Near Real-Time, DINEOF Gap-Filled 9km Daily Chl Concentration'
}

anim_trial = gg_static + transition_time(date) +    # fyi, this requires install of transformr (devtools::install_github("thomasp85/transformr"))
    labs(title = str_c("STRETCH Daily turtle movements (n=25) with ", title_eov),
         subtitle = "Date: {frame_time}", 
         caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location per turtle \n ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
    # caption = test) + #"Raw tracking data from ARGOS averaged to 1 daily location per turtle.\n The white line represents the 17°C isotherm. Ship release location (X). \n Data source: NOAA Coral Reef Watch 5km Daily SST \n Dana Briscoe") +
    theme(
        plot.title = element_text(size=16, face="bold", margin=margin(t=20,b=0), hjust=0.03),
        plot.subtitle = element_text(size = 16, face="bold", margin=margin(t=30,b=-30), hjust=0.025, color=subtitle_text_col),
        plot.caption = element_text(size=12),
        plot.margin = unit(c(0.75, 0, 0.5, 0), "cm")) +
    
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


###

gg + scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
                       breaks = seq(0,2,0.1),
                       # limits = c(0, 2.0),
                       na.value = 'snow',
                       name = "Chl (mg/m^3)")





## try contours 17-18
tzcf_contour = c(17,18)


## Geos plots ---------------
# example source: https://semba-blog.netlify.app/03/20/2019/plotting-streamlines-of-surface-current-with-ggplot2-and-metr-package/

require(metR)
require(tidyverse)
require(lubridate)
require(oce)
require(ocedata)
require(sf)
library(ncdf4)
library(raster)

## convert drifter observation into simple features
# nc <- nc_open('~/Downloads/nesdisSSH1day_264e_3354_38b8_U1690769775078.nc')
# nc <- nc_open('~/Downloads/miamicurrents_6801_d6a2_830a.nc')
nc <- '~/Downloads/miamicurrents_d2cc_c518_5667.nc'
print(nc_open(nc))

ugos_ras <- raster::brick(nc, varname='u_current') # ssha vectors 
vgos_ras <- raster::brick(nc, varname='v_current') # ssha vectors 

uras_df <- ugos_ras %>%
    rasterToPoints %>%
    as.data.frame() %>%
    `colnames<-`(c("x", "y", names(ugos_ras))) %>%
    pivot_longer(cols = starts_with("X20"),
                 names_to = "layer",
                 values_to = "u") %>%
    mutate(layer = substr(layer, 2, 14)) %>%
    mutate(date = as.POSIXct(layer, "%Y.%m.%d", tz = "UTC")
    )

vras_df <- vgos_ras %>%
    rasterToPoints %>%
    as.data.frame() %>%
    `colnames<-`(c("x", "y", names(vgos_ras))) %>%
    pivot_longer(cols = starts_with("X20"),
                 names_to = "layer",
                 values_to = "v") %>%
    mutate(layer = substr(layer, 2, 14)) %>%
    mutate(date = as.POSIXct(layer, "%Y.%m.%d", tz = "UTC")
    )


uv_df <- uras_df %>% left_join(., vras_df, by=c("x", "y", "date")) %>%
    dplyr::select(-c("layer.x", "layer.y")) %>%
    relocate(date, .after = y) %>%
    `colnames<-`(c("lon", "lat", "date", "u", "v"))

head(uv_df)

# rename for consistency
drifter.split <- uv_df  

drifter.split.sf = drifter.split %>% 
    st_as_sf(coords = c("lon", "lat")) %>%
    st_set_crs(4326)

drifter.grid = drifter.split.sf %>% 
    st_make_grid(n = c(70,60))%>%
    st_sf()

drifter.split.sf.se = drifter.split.sf #%>% filter(season=="SE")

drifter.gridded = drifter.grid %>% 
    mutate(id = 1:n(), contained = lapply(st_contains(st_sf(geometry),drifter.split.sf.se),identity),
           obs = sapply(contained, length),
           u = sapply(contained, function(x) {median(drifter.split.sf.se[x,]$u, na.rm = TRUE)}),
           v = sapply(contained, function(x) {median(drifter.split.sf.se[x,]$v, na.rm = TRUE)})) 


drifter.gridded = drifter.gridded %>% 
    dplyr::select(obs, u, v) %>% na.omit()

## obtain the centroid coordinates from the grid as table
coordinates = drifter.gridded %>% 
    st_centroid() %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    rename(x = X, y = Y)

## remove the geometry from the simple feature of gridded drifter dataset
st_geometry(drifter.gridded) = NULL

## stitch together the extracted coordinates and drifter information int a single table for SE monsoon season
current.gridded.se = coordinates %>% 
    bind_cols(drifter.gridded) #%>% 
    # mutate(season = "SE")

# ## bind the gridded table for SE and NE
# ## Note that similar NE follow similar procedure, hence not shown in the post
# drifter.current.gridded = current.gridded.ne %>% 
#     bind_rows(current.gridded.se)
drifter.current.gridded = current.gridded.se

## select grids for SE season only
drf.se = drifter.current.gridded #%>%
    # filter(season == "SE")

## interpolate the U component
u.se = interpBarnes(x = drf.se$x, y = drf.se$y, z = drf.se$u)

## obtain dimension that determine the width (ncol) and length (nrow) for tranforming wide into long format table
dimension = data.frame(lon = u.se$xg, u.se$zg) %>% dim()

## make a U component data table from interpolated matrix
u.tb = data.frame(lon = u.se$xg, 
                  u.se$zg) %>% 
    gather(key = "lata", value = "u", 2:dimension[2]) %>% 
    mutate(lat = rep(u.se$yg, each = dimension[1])) %>% 
    dplyr::select(lon,lat, u) %>% as.tibble()

## interpolate the V component
v.se = interpBarnes(x = drf.se$x, 
                    y = drf.se$y, 
                    z = drf.se$v)

## make the V component data table from interpolated matrix
v.tb = data.frame(lon = v.se$xg, v.se$zg) %>% 
    gather(key = "lata", value = "v", 2:dimension[2]) %>% 
    mutate(lat = rep(v.se$yg, each = dimension[1])) %>% 
    dplyr::select(lon,lat, v) %>% 
    as.tibble()

## stitch now the V component intot the U data table and compute the velocity
uv.se = u.tb %>% 
    bind_cols(v.tb %>% 
    dplyr::select(v)) %>% 
    mutate(vel = sqrt(u^2+v^2))

# visualise
## update cpal
library(khroma)
limits <- c(0,1.6)
smooth_rainbow <- khroma::colour("smooth rainbow")
cpal <- c(smooth_rainbow(length(seq(floor(limits[1]), ceiling(limits[2]), 1)), range = c(0, 0.9))), "#9e2a2b", "firebrick4", "#540b0e")
cpal <- 

ggplot() +
    metR::geom_contour_fill(data = uv.se, aes(x = lon, y = lat, z = vel), na.fill = TRUE, bins = 70) + 
    metR::geom_vector(data = uv.se, aes(x = lon, y = lat, dx = u, dy = v), 
                  arrow.angle = 30, arrow.type = "open", arrow.length = .5, 
                  pivot = 0,preserve.dir = TRUE, direction = "ccw", color = 'snow')+
    # geom_sf(data = wio,fill = "lightgrey", col = "black")+
    # coord_sf(ylim = c(-15,-6), xlim = c(39, 52))+
    coord_sf(ylim = c(35,50), xlim = c(-160, -140))+    
    scale_fill_viridis_c(name = "Speed\n(m/s)",#colours = cpal, 
                     limits = c(0,1.6), breaks =seq(0.1,1.6,.3))+
    theme_bw()+
    theme(legend.position = "right",
      legend.key.height = unit(1.2, "cm"), 
      legend.background = element_blank(),
      axis.text = element_text(size = 12, colour = 1))+
    scale_mag(max = 0.15, name = "Speed", max_size = 0.7)+
    labs(x = "", y = "")


library(RColorBrewer)
palette <- rev(brewer.pal(9, "YlGnBu"))
library(colorspace)
test <- 
ggplot() +
    metR::geom_contour_fill(data = uv.se, aes(x = lon, y = lat, z = vel), 
                            na.fill = TRUE, bins = 70) + 
    metR::geom_streamline(data = uv.se, 
                          aes(x = lon, y = lat, dx = u, dy = v),
                          L = 1.5, res = .9, n = 40, jitter = 4)+
    # geom_sf(data = wio,fill = "lightgrey", col = "black")+
    coord_sf(ylim = c(35,50), xlim = c(-160, -140))+
    # scale_fill_gradientn(name = "Current",colours = rev(rainbow(20)), 
    #                      limits = c(0,1.6), breaks = seq(0.1,1.6,.3))+
    # scale_fill_distiller(palette = "Spectral", direction = -1,
    # name = "Current",
    # limits = c(-0.2,0.2), breaks = seq(-0.2,0.2,.05))+
    # scale_fill_continuous_sequential(palette = "Viridis", trans = "reverse") +

    scale_fill_viridis_c(name = "Surface \nCurrent (m/s)",
                                                    limits = c(0, 0.25),
                                                    breaks = seq(0, 0.2, 0.05))+
        
    theme_bw()+
    theme(legend.position = "right",
          legend.key.height = unit(1.4, "cm"), 
          legend.background = element_blank(),
          axis.text = element_text(size = 12, colour = 1))+
    labs(x = "", y = "")

test <- 
ggplot()+
    metR::geom_streamline(data = uv.se, 
                          aes(x = lon, y = lat, dx = u, dy = v, 
                              color = sqrt(..dx..^2 + ..dy..^2), 
                              alpha = ..step..),
                          L = 2, res = 2, n = 60, 
                          arrow = NULL, lineend = "round")+
    coord_sf(ylim = c(35,50), xlim = c(-160, -140))+
    scale_color_viridis_c(name = "Currents")+
    scale_size(range = c(0.2, 1.5), guide = "none") +
    scale_alpha(guide = "none") +
    theme_bw()+
    theme(legend.position = "right",
          legend.key.height = unit(1.4, "cm"), 
          legend.background = element_blank(),
          axis.text = element_text(size = 12, colour = 1))+  
    labs(x = "", y = "")


gg <- 
    # ggplot() +
    test + 
    # turtle daily movements
    geom_point(data=turtles_df %>% 
                   mutate(lon = make180(lon)),
               aes(x=lon,y=lat), color = "azure2",
               # fill = "#2a9d8f", shape = 21,
               fill = "snow", shape = 21,
               stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
    
    # release location
    geom_point(data=release_loc, aes(x=make180(lon), y=lat), fill = "lightgray",
               color = "lightgray", shape = 4, size = 5.5) +
    
    labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ") +
    # theme(legend.position = "none") +
    
    theme_minimal() + theme(text=element_text(size=plot_params$plot_text_size)) +
    # # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
    coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) +
    guides(fill = guide_colourbar(
        barheight = plot_params$barheight,
        ticks = TRUE)
    ) + 
    # facet_wrap(~date) +
    NULL


## - try animating -----------
params$varname = 'currents'
ncs <- list.files(path='~/Downloads/', pattern =  params$varname, full.names=T)
print(str_c('most recent ncdf - ', ncs[length(ncs)]))

ncs <- '/Users/briscoedk/Downloads//miamicurrents_d2cc_c518_5667.nc'
# will need to fix fdates when start batch downloading with api. for now, it was a bulk download from erddap
ncIn <- sapply(1:length(ncs), function(x) ncs[grepl(ncs[x],ncs)])

# ugos_ras <- raster::brick(nc, varname='u_current') # ssha vectors 
# vgos_ras <- raster::brick(nc, varname='v_current') # ssha vectors 

ret_uv.se<- prep_uv_geostrophic(ncIn)        

# rename for consistency
uv.se <- ret_uv.se %>%
# uv.se <- uv.se %>% 
    mutate(date = as.Date(date)) %>%
    filter(date >= '2023-07-11')

# ret_uv.se_360 <- ret_uv.se %>% mutate(lon = make360(lon))
turtles_df_hauoli <- turtles_df %>% filter(id == '243194')


# dt <- seq.Date(min(uv.se$date)+1,max(uv.se$date), by=1)
dt <- seq.Date(min(uv.se$date),max(uv.se$date), by=1)
dt_str <- format(`dt`)

plot_params <- list(
    turtle_pt_size = 5.25,
    barheight = 28,
    plot_text_size = 22,
    title_size = 16,
    subtitle_size = 16,
    caption_size = 12
)
plot_list <- 
    lapply(dt, function(i) {

# gg_static <- 
gg_currents <- 
# ggplot()+
#     # ggplot()+
#     metR::geom_streamline(data = uv.se %>%
#                               filter(date == i), 
#                           aes(x = lon, y = lat, dx = u, dy = v, 
#                               color = sqrt(..dx..^2 + ..dy..^2), 
#                               alpha = ..step..),
#                           L = 2, res = 2, n = 60, 
#                           arrow = NULL, lineend = "round")+
#     coord_sf(ylim = c(35,42), xlim = c(-155, -140), expand = FALSE, crs = st_crs(4326)) +
#     
#     scale_color_viridis_c(name = "Surface \nCurrent (m/s)",
#                           limits = c(0, 0.25),
#                           breaks = seq(0, 0.2, 0.05))+
#     scale_size(range = c(0.2, 1.5), guide = "none") +
#     scale_alpha(guide = "none") +
#     theme_bw()+
#     theme(legend.position = "right",
#           legend.key.height = unit(1.4, "cm"), 
#           legend.background = element_blank(),
#           axis.text = element_text(size = 12, colour = 1))+  
#     labs(x = "", y = "") +
#     
ggplot() +
    metR::geom_contour_fill(data = uv.se %>%
                              filter(date == i), 
                            aes(x = lon, y = lat, z = vel), na.fill = TRUE, bins = 70) +
    metR::geom_vector(data = uv.se  %>%
                          filter(date == i), 
                      aes(x = lon, y = lat, dx = u, dy = v), 
                      arrow.angle = 30, arrow.type = "open", arrow.length = 1.25, 
                      show_guide = FALSE, 
                      pivot = 0,preserve.dir = TRUE, direction = "ccw", color = 'snow')+
    scale_mag(max = 0.15, name = "", max_size = 0.7)+
    # geom_sf(data = wio,fill = "lightgrey", col = "black")+
    # coord_sf(ylim = c(-15,-6), xlim = c(39, 52))+
    coord_sf(ylim = c(35,42), xlim = c(-155, -140), expand = FALSE, crs = st_crs(4326)) +
    scale_fill_viridis_c(name = "Velocity\n(m/s)",#colours = cpal, 
                         limits = c(0, 0.25),
                         breaks = seq(0, 0.2, 0.05)) +
    scale_alpha(guide = "none") +
    theme_bw()+
    theme(legend.position = "right",
          legend.key.height = unit(1.4, "cm"),
          legend.background = element_blank(),
          axis.text = element_text(size = 12, colour = 1))+
    labs(x = "", y = "") +

    # pts up to current
    geom_point(data=turtles_df_hauoli %>% 
                   mutate(date = as.Date(date)) %>%
                   filter(date <= i) %>%
                   mutate(lon = make180(lon)),
               aes(x=lon,y=lat), color = "gray90",
               # fill = "#2a9d8f", shape = 21,
               fill = "black", shape = 21,
               stroke = 0.5, alpha = 0.90, size=plot_params$turtle_pt_size-1.5) +
    
    geom_line(data=turtles_df_hauoli %>%
                  filter(date <= i) %>%
                  mutate(lon = make180(lon)),
              aes(x=lon,y=lat, group=1), color = "gray20", alpha = 0.40, linewidth=1.25)+
    
    # current frame
    # pts up to current
    geom_point(data=turtles_df_hauoli %>% 
                   mutate(date = as.Date(date)) %>%
                   filter(date == i) %>%
                   mutate(lon = make180(lon)),
               aes(x=lon,y=lat), color = "gray90",
               # fill = "#2a9d8f", shape = 21,
               fill = "orange", shape = 21,
               stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
    
    geom_line(data=turtles_df_hauoli %>%
                  filter(date == i) %>%
                  mutate(lon = make180(lon)),
              aes(x=lon,y=lat, group=1), color = "gray20", alpha = 0.40, linewidth=1.25)+
    
    # release location
    geom_point(data=release_loc, aes(x=make180(lon), y=lat), fill = "black",
               color = "black", shape = 4, size = 6.5) +
    
    labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ",
         subtitle = paste0('Date: ', i)) +
    
    # coord_sf(ylim = c(35,42), xlim = c(-155, -140))+
    
    # facet_wrap(~date) +
    NULL

title_eov <- 'surface currents'
subtitle_text_col <- 'gray8'
    caption_iso <- 'The orange circle is the current date-location. The darker circles are the previous locations. ' #The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
    eov_source <- 'Velocity & vectors (white arrows) are calculated from Daily Near Real-Time Geostrophic Current (u, v), 0.2 degrees spatial res, NOAA CoastWatch'
    
gg_currents + 
    labs(title = str_c("STRETCH Daily turtle movements of Turtle Hau'oli (#17) with ", title_eov),
         # subtitle = i, 
         caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location (circles) \n ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
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
    ))
    

})


# name the elements in the list
p_names <- c()
for(x in 1:length(dt)){
p_names <- c(p_names, str_c("hauoli_day_", dt[x],"_plot"))
}
# names(plot_list) <- p_names


# # save list and load in 'test_ppt_stretch.Rmd'
# save(monthly_tracks_plot_list, file = './data/interim/outputs_MOL_with_tracks_by_month_plots.rds')

# save plots as pngs (to create gif)
save_figs = TRUE

if(save_figs){
    lapply(seq(1:length(p_names)), function(j) {ggsave(plot_list[[j]], 
                                                         file = str_c('~/Downloads/', "stretch_", p_names[j], "_vectors.png"), 
                                                         width=13, 
                                                         # height=8.5,   # comment out height to get auto aspect ratio set!
                                                         bg = 'white')
    })
}

# create movie
library(magick)

# List plots files
# plot_files <- list.files('~/Downloads', pattern = "hauoli",full.names=T)
plot_files <- list.files('~/Downloads', pattern = "vectors",full.names=T)
pfiles <- plot_files[order(basename(plot_files))]

# # arrange by month order
# lsfiles <- file.info(plot_files)
# pfiles <- lsfiles[order(lsfiles$mtime),]

# Read the plots as image
plots_list <- lapply(pfiles, image_read)

# Join the list of plots
plot_joined <- image_join(plots_list)

# Create animation, defining the frames per second (fps)
plot_animated <- image_animate(plot_joined, fps = 1)

# Write animation as gif
image_write(image = plot_animated,
            path = str_c('~/Downloads/', "hauoli_surface_currents_vectors.gif"))




raw_data[raw_data$lon > -130,]


## try to functionalize currents
gg_static <-
# gg_currents <- 

ggplot() +
    metR::geom_contour_fill(data = uv.se #%>%
                                # filter(date == i)
                                , 
                            aes(x = lon, y = lat, z = vel), na.fill = TRUE, bins = 70) +
    metR::geom_vector(data = uv.se  %>%
                          filter(date == i), 
                      aes(x = lon, y = lat, dx = u, dy = v), 
                      arrow.angle = 30, arrow.type = "open", arrow.length = 1.25, 
                      show_guide = FALSE, 
                      pivot = 0,preserve.dir = TRUE, direction = "ccw", color = 'snow')+
    scale_mag(max = 0.15, name = "", max_size = 0.7)+
    # geom_sf(data = wio,fill = "lightgrey", col = "black")+
    # coord_sf(ylim = c(-15,-6), xlim = c(39, 52))+
    coord_sf(ylim = c(35,42), xlim = c(-155, -140), expand = FALSE, crs = st_crs(4326)) +
    scale_fill_viridis_c(name = "Velocity\n(m/s)",#colours = cpal, 
                         limits = c(0, 0.25),
                         breaks = seq(0, 0.2, 0.05)) +
    scale_alpha(guide = "none") +
    theme_bw()+
    theme(legend.position = "right",
          legend.key.height = unit(1.4, "cm"),
          legend.background = element_blank(),
          axis.text = element_text(size = 12, colour = 1))+
    labs(x = "", y = "") +
    
    # pts up to current
    geom_point(data=turtles_df_hauoli %>% 
                   mutate(date = as.Date(date)) %>%
                   # filter(date <= i) %>%
                   mutate(lon = make180(lon)),
               aes(x=lon,y=lat), color = "gray90",
               # fill = "#2a9d8f", shape = 21,
               fill = "black", shape = 21,
               stroke = 0.5, alpha = 0.90, size=plot_params$turtle_pt_size-1.5) +
    
    geom_line(data=turtles_df_hauoli %>%
                  filter(date <= i) %>%
                  mutate(lon = make180(lon)),
              aes(x=lon,y=lat, group=1), color = "gray20", alpha = 0.40, linewidth=1.25)+
    
    # current frame
    # pts up to current
    geom_point(data=turtles_df_hauoli %>% 
                   mutate(date = as.Date(date)) %>%
                   # filter(date == i) %>%
                   mutate(lon = make180(lon)),
               aes(x=lon,y=lat), color = "gray90",
               # fill = "#2a9d8f", shape = 21,
               fill = "orange", shape = 21,
               stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
    
    geom_line(data=turtles_df_hauoli %>%
                  # filter(date == i) %>%
                  mutate(lon = make180(lon)),
              aes(x=lon,y=lat, group=1), color = "gray20", alpha = 0.40, linewidth=1.25)+
    
    # release location
    geom_point(data=release_loc, aes(x=make180(lon), y=lat), fill = "black",
               color = "black", shape = 4, size = 6.5) +
    
    labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ",
         subtitle = paste0('Date: ', i)) +
    
    # coord_sf(ylim = c(35,42), xlim = c(-155, -140))+
    
    # facet_wrap(~date) +
    NULL

title_eov <- 'surface currents'
subtitle_text_col <- 'gray8'
    caption_iso <- 'The orange circle is the current date-location. The darker circles are the previous locations. ' #The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
    eov_source <- 'Velocity & vectors (white arrows) are calculated from Daily Near Real-Time Geostrophic Current (u, v), 0.2 degrees spatial res, NOAA CoastWatch'
    
    gg_currents + 
        labs(title = str_c("STRETCH Daily turtle movements of Turtle Hau'oli (#17) with ", title_eov),
             # subtitle = i, 
             caption = str_c("\n \n Raw tracking data from ARGOS averaged to 1 daily location (circles) \n ", caption_iso, "Ship release location (X) \n Data source: ", eov_source," \n Dana Briscoe")) +
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
        ))
    
    
    # })
    # 
    
    ggplot() +
        geom_segment(data = uv.se %>% filter(date < '2023-07-12'), 
                     aes(x = lon, xend = lon+u/60, y = lat, 
                         yend = lat+v/60), arrow = arrow(length = unit(0.25, "cm"))) +
        # geom_sf(data = tz.ke, fill = "grey85", col = 1)+
        coord_sf(ylim = c(35, 50), xlim =  c(-160, -140))+
        # scale_x_continuous(breaks = c(38.8,40))+
        theme_bw()+
        theme(axis.text = element_text(size = 11, colour = 1))+
        labs(x = NULL, y = NULL)
    
    
## USE THIS TUTORIAL FOR VELOCITY AND ANIMATIONS -----------------------------------------------------------------------------
# source: https://semba-blog.netlify.app/10/29/2018/animating-oceanographic-data-in-r-with-ggplot2-and-gganimate/
    
# require(xtractomatic)
require(tidyverse)
require(oce)
require(lubridate)
require(gganimate)
require(sf)
    

## zonal compoent
wind_x = xtracto_3D(dtype = "qsux101day", 
                    xpos = c(38.85, 40), 
                    ypos = c(-6.8, -4.5), 
                    tpos = c("2000-01-01", "2008-12-31"))
# meridional component
wind_y = xtracto_3D(dtype = "qsuy101day", 
                    xpos = c(38.85, 40), 
                    ypos = c(-6.8, -4.5), 
                    tpos = c("2000-01-01", "2008-12-31"))
## Extract
longitude = wind_x$longitude
latitude = wind_x$latitude
time = wind_x$time %>% as.Date()
# eastward velocity (zonal)
u = wind_x$data
# northward velocity (meridional)
v = wind_y$data

    # calculate wind velocity
    velocity = sqrt(u^2 + v^2)
    
    
# similar to above    
ugos_ras <- raster::brick(ncIn, varname='u_current') # ssha vectors 
vgos_ras <- raster::brick(ncIn, varname='v_current') # ssha vectors 

uras_df <- ugos_ras %>%
    rasterToPoints %>%
    as.data.frame() %>%
    `colnames<-`(c("x", "y", names(ugos_ras))) %>%
    pivot_longer(cols = starts_with("X20"),
                 names_to = "layer",
                 values_to = "u") %>%
    mutate(layer = substr(layer, 2, 14)) %>%
    mutate(date = as.POSIXct(layer, "%Y.%m.%d", tz = "UTC")
    )

vras_df <- vgos_ras %>%
    rasterToPoints %>%
    as.data.frame() %>%
    `colnames<-`(c("x", "y", names(vgos_ras))) %>%
    pivot_longer(cols = starts_with("X20"),
                 names_to = "layer",
                 values_to = "v") %>%
    mutate(layer = substr(layer, 2, 14)) %>%
    mutate(date = as.POSIXct(layer, "%Y.%m.%d", tz = "UTC")
    )

    
uv_df <- uras_df %>% left_join(., vras_df, by=c("x", "y", "date")) %>%
    dplyr::select(-c("layer.x", "layer.y")) %>%
    relocate(date, .after = y) %>%
    `colnames<-`(c("lon", "lat", "date", "u", "v")) %>%
    mutate(date = as.POSIXct(date, "%Y.%m.%d")) %>%
    mutate(velocity = sqrt(u^2 + v^2))
    
head(uv_df)

#
wind = uv_df %>%
    mutate(day = yday(date) %>%as.integer(), 
           week = week(date) %>%as.integer(),  month = month(date) %>%as.integer(), 
           year = year(date) %>%as.integer()) %>%
    dplyr::select(date,day, week, month, year,lon,lat, u,
           v, velocity ) %>%
    mutate(date = as.Date(date))
    
wind %>% head()        

wind.date = wind %>% 
    group_by(lon, lat, date) %>% 
    summarise(u = median(u, na.rm = TRUE),
              v = median(v, na.rm = TRUE), 
              velocity = median(velocity, na.rm = TRUE)) %>%
    mutate(velocity_kmh = round(velocity * (3.6),2)) # aka 18/5 = 3.6


wind.date

# basic
ggplot() +
    geom_segment(data = wind.date %>% filter(date < '2023-07-12'),
                 aes(x = lon, xend = lon+u/250, y = lat,
                     yend = lat+v/250), arrow = arrow(length = unit(0.35, "cm")),color = 'dodgerblue') +
    # metR::geom_vector(data = wind.date %>% filter(date < '2023-07-12'), 
    #                   aes(x = lon, y = lat, dx = u, dy = v), 
    #                   arrow.angle = 30, arrow.type = "open", arrow.length = 1.25, 
    #                   show_guide = FALSE, 
    #                   pivot = 0,preserve.dir = TRUE, direction = "ccw", color = 'dodgerblue')+
    scale_mag(max = 0.15, name = "", max_size = 0.7)+
    # geom_sf(data = tz.ke, fill = "grey85", col = 1)+
    coord_sf(ylim = c(35, 50), xlim =  c(-160, -140))+
    # scale_x_continuous(breaks = c(38.8,40))+
    theme_bw()+
    theme(axis.text = element_text(size = 11, colour = 1))+
    labs(x = NULL, y = NULL)


turtles_df_hauoli <- turtles_df_hauoli %>% filter(date <= max(wind.date$date))

# metR
gg_currents <- 
ggplot() +
    # geom_raster(data = wind.date, aes(x = lon, y = lat, fill = velocity), interpolate = TRUE) +
    geom_raster(data = wind.date, aes(x = lon, y = lat, fill = velocity_kmh), interpolate = TRUE) +
    metR::geom_vector(data = wind.date,  
                      aes(x = lon, y = lat, dx = u, dy = v), 
                      arrow.angle = 30, arrow.type = "open", arrow.length = 1.25, 
                      show_guide = FALSE, 
                      pivot = 0,preserve.dir = TRUE, direction = "cw", color = 'snow') +
    scale_mag(max = 0.15, name = "", max_size = 0.7)+

    scale_fill_viridis_c(name = "Velocity\n(km/hr)",#colours = cpal, 
                         alpha = 0.9,
                         # limits = c(0, 0.25), # m/s
                         # breaks = seq(0, 0.2, 0.05)) +
                         limits = c(0, 1.2), # km/hr
                         breaks = seq(0, 1.2, 0.2)) +
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
title_eov <- 'surface currents'
subtitle_text_col <- 'gray8'
    caption_iso <- '' #'The orange circle is the current date-location. The darker circles are the previous locations. ' #The 0.2 mg/m^3 isopleth (black line) represents the approximate TZCF position in the eastern North Pacific. '
    eov_source <- 'Surface velocity and vectors (white arrows) are calculated from Near Real-Time Geostrophic Current (u, v) \n Daily, 0.2 degrees spatial resolution, NOAA CoastWatch'
    


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
    geom_raster(data = wind.date, aes(x = lon, y = lat, fill = velocity_kmh), interpolate = TRUE) +
    metR::geom_vector(data = wind.date,  
                      aes(x = lon, y = lat, dx = u, dy = v), 
                      arrow.angle = 30, arrow.type = "open", arrow.length = 1.0, 
                      show_guide = FALSE, 
                      pivot = 0,preserve.dir = TRUE, direction = "cw", color = 'snow') +
    scale_mag(max = 0.15, name = "", max_size = 0.7)+
    
    scale_fill_viridis_c(name = "Velocity\n(km/hr)",#colours = cpal, 
                         alpha = 0.9,
                         # limits = c(0, 0.25), # m/s
                         # breaks = seq(0, 0.2, 0.05)) +
                         limits = c(0, 1.2), # km/hr
                         breaks = seq(0, 1.2, 0.2)) +
    scale_alpha(guide = "none") +
    
    
    geom_point(data=turtles_df_hauoli %>% 
                   mutate(date = as.Date(date)) %>%
                   mutate(lon = make180(lon)),
               aes(x=lon,y=lat), color = "gray90",
               # fill = "#2a9d8f", shape = 21,
               fill = "orange", shape = 21,
               stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
    
    
    # release location
    geom_point(data=release_loc, aes(x=make180(lon), y=lat), fill = "black",
               color = "black", stroke = 1, shape = 4, size = 7.5) +
    
    
    labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ") +
    # coord_sf(ylim = c(35, 41), xlim =  c(-159, -141), expand = FALSE, crs = st_crs(4326)) +
    coord_sf(ylim = c(37.5, 39.5), xlim =  c(-148, -144), expand = FALSE, crs = st_crs(4326)) +
    
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
    eov_source <- 'Surface velocity and vectors (white arrows) are calculated from Near Real-Time Geostrophic Current (u, v) \n Daily, 0.2 degrees spatial resolution, NOAA CoastWatch'
    
    
    
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
                       renderer = av_renderer(str_c('~/Downloads/dbriscoe_animation_trial_', params$eov,'_full_contours_v3Aug_interp_kmh_zoom_v2.mp4')))
    
    

# animate(wind.vector)
    
    
    
## copernicus api ----
## Load libraries ----
pkgs <- c(
    "reticulate",
    "lubridate",
    "tidyverse",
    "config"
)

lapply(pkgs, library, character.only = TRUE)

# must for reticulate to work
os <- import("os")
os$listdir(".")
# py_install(c("motuclient")) # note: motuclient==1.8.4 _MUST_ be installed (via requirements.txt) for system(command) to work!
import("motuclient")
# glorys_key <- config::get(file = "./utils/glorys_config.yml")  
glorys_key <- config::get(file = "~/github/catalyst-bHABs/utils/glorys_config.yml")  


if(params$eov == 'sla_uv'){
    varnames = params_df$varname
    params_df <- params_df %>% slice(1)
}

dates <- getDateRange(startdate = params$startdate,enddate = params$enddate, unit=params$timestep) %>%
    str_c(., params_df$date_string, sep="")

i=24
for (i in 1:(length(glorys_dates))) {
    getNCDF_sla(
        url = 'https://nrt.cmems-du.eu/motu-web/Motu' #params_df$url,
        varnames = varnames,
        # location = "japan",
        bbox = params$bbox,
        dt = dates[i],
        # depths = tibble(min = 0.494, max = 155.8507),
        ncpath = params$nc_path
        ncpath = '~/Downloads'
    )
}

getNCDF_sla <- function(url, varnames, 
                               # location, 
                               bbox, dt = dates, 
                               # depths, 
                               ncpath) {
    startdate <- as.character(dt[i])
    
    os$chdir(ncpath)
    os$getcwd()
    
    filenm <- str_c("nrt-global-merged-allsat-phy_", params$eov,"_" ,as.Date(dates[i]), ".nc", sep = "")
    
    if (!file.exists(str_c(ncpath, "/", filenm))) {
        command <- str_c("python -m motuclient --motu ", url,
                         " --service-id SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046-TDS", 
                         " --product-id dataset-duacs-nrt-global-merged-allsat-phy-l4",
                         " --longitude-min ", bbox$xmin, " --longitude-max ", bbox$xmax,
                         " --latitude-min ", bbox$ymin, " --latitude-max ", bbox$ymax,
                         " --date-min ", startdate, " --date-max ", startdate,
                         # " --depth-min ", depths$min, " --depth-max ", depths$max,
                         " --variable ", varnames[1], " --variable ", varnames[2], " --variable ", varnames[3],
                         " --out-dir ", ".", "/", " --out-name glorys_global_multiyear_phy_", as.Date(dates[i]), ".nc --user ", glorys_key$user, " --pwd ", glorys_key$pwd,
                         sep = ""
        )
        return(system(command))
    } else {
        message("data already downloaded! \n Located at:\n", str_c(ncpath, filenm, sep = "/"))
    }
    
    os$chdir('~/github/cc-stretch-turtle-eov-animation/code')
}

x = '/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac/nrt-global-merged-allsat-phy_sla_uv_2023-08-04.nc'
ras = raster::stack(x, varname='sla')
plot(ras, col=rainbow(100))



library(ggplot2)
g <- ggplot(seals, aes(long, lat)) +
    geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2)

g + scale_mag("Seals velocity")

g + scale_mag("Seals velocity", max = 1)

g + scale_mag("Seals velocity", max_size = 2)
g + scale_mag("Seals velocity", default_unit = "mm")


# Load the package required to read JSON files.
library("rjson")

# Give the input file name to the function.
result <- fromJSON(file = "daily_ncdf.json") %>% as.data.frame()
ret <- jsonlite::read_json("ncdf_list.json")

ret2 <- data.table::rbindlist(ret, fill=TRUE) %>% 
    # unnest(eov) %>%  
    purrr::pmap_dfr(data.frame)  # deals with remaining cols in list form

# Print the result.
print(result)

dates
params_df <- ret2[ret2$eov == params$eov,] 

dates %>%
    as.character() %>%
    str_c(., params_df$date_string, sep=" ")


nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"
fname = 'noaa20NRTchlaGapfilledDaily_2023-08-03.nc'

nc = nc_open(paste0(nc_path, '/' ,fname))
print(nc)

ras_nc = raster::stack(paste0(nc_path, '/' ,fname))[[1]]
sp::plot(ras_nc, zlim=c(0,3))


## Get Max Lat from Cohort 1 Data ----------------------
raw_data[raw_data$lat == max(raw_data$lat),]

raw_data[raw_data$lat >=47,]




## redo eov animation static plot ---------------------------

add_baseplot <- function(bbox) {
    list(
        # add land
        geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black"),
        # sest cbar height
        guides(fill = guide_colourbar(barheight =12, ticks = TRUE,
                                      frame.colour = "black", ticks.colour = "black")),
        # set map extent
        coord_sf(xlim = c(bbox[1], bbox[2]), ylim = c(bbox[3], bbox[4]), 
                 expand = FALSE, crs = st_crs(4326)), 
        labs(x = "Longitude", y = "Latitude")
    )
}

update_map_extent <- function(xrange, yrange){
    list(
        coord_sf(xlim = c(xrange[1], xrange[2]), ylim = c(yrange[1], yrange[2]), 
                 expand = FALSE, crs = st_crs(4326))
    )
}

add_turtle_pts <- function(df, cpal=rainbow(25), size=3){
    list(
        geom_point(data=df, #aes(x = lon, y = lat, colour =  as.factor(id)), size = size),
        aes(x=lon,y=lat, color = as.factor(id)),#shape = 21,
        stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size, show.legend=FALSE),
        scale_colour_manual(values = cpal)
    )
}

add_turtle_pts_border <- function(df, cpal=rainbow(25), size=3){
    list(
        geom_point(data=df, #aes(x = lon, y = lat, colour =  as.factor(id)), size = size),
                   aes(x=lon,y=lat), color = "azure2",shape = 21,
                   stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size, show.legend=FALSE)
    )
}
# add_turtle_pts <- function(df, size=3){
#     list(
#         geom_point(data=df, aes(x = lon, y = lat, colour = as.factor(group)), size = size)
#     )
# }

add_track_spLines <- function(df, cpal, linewidth) {
    list(
        # add cohort 1 tracks
        geom_sf(data = df %>% makeSpatialLines(lon360=FALSE), 
                aes(colour = as.factor(id)),linewidth = linewidth, alpha = 0.9, show.legend=FALSE),
        scale_colour_manual(values = cpal)
    )
}


## TESTER
test <- ggplot() +
    # # # add ssta layer
    # # geom_raster(data = ssta_ras_mean_df, 
    # #             aes(x = lon, y = lat, fill = val, interpolate = TRUE)) +
    # geom_tile(
    #     
    #     data = eov_df,
    #     aes(x = x, y = y, fill = val, group = date)) +
    # {
    #     if (eov =='chla'){
    #         # scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
    #         scale_fill_gradientn(colours = c( "gray99", cpal[6:length(cpal)]),
    #                              breaks = cbar_breaks,
    #                              limits = cbar_limits,
    #                              na.value = 'snow',
    #                              name = "Chl \n(mg/m^3) \n ")
    #     } else if (eov =='sst') {
    #         scale_fill_gradientn(colours = 
    #                                  # cpal[12:length(cpal)],
    #                                  # breaks=seq(10,25,2),
    #                                  # limits = c(9.25,25),
    #                                  cpal[11:length(cpal)],
    #                              
    #                              breaks=cbar_breaks, #seq(6,32,2),
    #                              limits = c(min(cbar_limits),max(cbar_limits)),
    #                              na.value = 'snow',
    #                              name = "SST (°C) \n")
    #     }
    # } +
    # 
    # add sst sept all-time clim layer
    geom_raster(data = eov_df, 
                aes(x = x, y = y, fill = val, group = date), interpolate = TRUE) +

    scale_fill_gradientn(colours = 
                             # cpal[12:length(cpal)],
                             # breaks=seq(10,25,2),
                             # limits = c(9.25,25),
                             cpal[11:length(cpal)],
                         
                         breaks=cbar_breaks, #seq(6,32,2),
                         limits = c(min(cbar_limits),max(cbar_limits)),
                         na.value = 'snow',
                         name = "SST (°C) \n") +
    # add baselayers
    add_baseplot(bbox=e)
    

test + 
    # add_track_spLines(turtles_df %>% 
    #                          mutate(lon = make360(lon)), cpal=rainbow(25), linewidth=1) + 
    add_turtle_pts(df=turtles_df %>% 
                          mutate(lon = make360(lon)), cpal=rainbow(25)) + 

    add_turtle_pts_border(df=turtles_df %>% 
                              mutate(lon = make360(lon)))


gg <- 
    ggplot() +
    geom_tile(
        
        data = eov_df,
        aes(x = x, y = y, fill = val, group = date)) +
    
    
    {
        if (!is.null(tzcf_contour)) {
            # add tzcf contour
            geom_contour(data=eov_df, aes(x=x, y=y, z = val), colour = tzcf_color, linewidth = 1.25,
                         breaks = c(tzcf_contour)) 
        }
    } +
    
    # # add tzcf contour
    # geom_contour(data=eov_df, aes(x=x, y=y, z = val), colour = "white", linewidth = 1.25,
    #              breaks = c(tzcf_contour)) +
    
    # add coast 
    geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
    
    
    {
        if (eov =='chla'){
            # scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
            scale_fill_gradientn(colours = c( "gray99", cpal[6:length(cpal)]),
                                 breaks = cbar_breaks,
                                 limits = cbar_limits,
                                 na.value = 'snow',
                                 name = "Chl \n(mg/m^3) \n ")
        } else if (eov =='sst') {
            scale_fill_gradientn(colours = 
                                     # cpal[12:length(cpal)],
                                     # breaks=seq(10,25,2),
                                     # limits = c(9.25,25),
                                     cpal[11:length(cpal)],
                                 
                                 breaks=cbar_breaks, #seq(6,32,2),
                                 limits = c(min(cbar_limits),max(cbar_limits)),
                                 na.value = 'snow',
                                 name = "SST (°C) \n")
        }
    } +
    
    
    # turtle daily movements
    
    geom_point(data=turtles_df %>% 
                   mutate(lon = make360(lon)),
               # aes(x=lon,y=lat), color = "azure2",
               aes(x=lon,y=lat, fill = as.factor(id)), color = "azure2",shape = 21,
               # # fill = "#2a9d8f", shape = 21,
               # fill = "#3c096c", shape = 21,
               stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
    scale_fill_manual(values = rainbow(25)) +
    
    # release location
    geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
               color = "black", shape = 4, size = 5.5) +
    
    labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ") +
    # theme(legend.position = "none") +
    
    theme_minimal() + theme(text=element_text(size=plot_params$plot_text_size)) +
    # # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
    coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) +
    guides(fill = guide_colourbar(
        barheight = plot_params$barheight,
        ticks = TRUE)
    ) + 
    # facet_wrap(~date) +
    NULL



# set up base pac ocean map
npac <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) %>%
    filter(long >= 120 & long <= 270 & lat >= 15 & lat <=80) 

fake <-ggplot(npac) +
    
    geom_polygon(aes(x = long, y = lat, group = group), colour = "black", size = 0.4, fill = 'black') +
    coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) 

p_npac <- 
    ggplot(npac) +
    
    geom_polygon(aes(x = long, y = lat, group = group), colour = "black", size = 0.4, fill = 'black') + 
    theme_bw() + 
    ylab("") +  xlab("") +
    annotate(
        "rect",
        xmin = e[1],
        xmax = e[2]-5,
        ymin = e[3],
        ymax = e[4],
        color = 'gray',
        fill = 'transparent',
        size = .7
    ) +  
    coord_map(xlim = c(180, 250), ylim = c(20,55)) + 
    theme(
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        
        panel.border = element_rect(colour = "white", fill=NA, size=2),
        
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        
        legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_rect(fill = "transparent"),
        
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
    ) 
#   coord_map(xlim = c(172.5, 174.5), ylim = c(-41.75,-40.3))    # extended view (for polygons)

p_npac + NULL

# # library(cowplot)
test2 <-
    ggdraw() +
    draw_plot(fake) +
    draw_plot(p_npac, x = 0.725, y = .65, width = .2, height = .175)

test <- fake +
print(p_npac, vp = viewport(0.725, .65, width = 0.2, height = 0.175))



## plot with state outlines
library(sf)
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa <- st_as_sf(usa, wkt = "geom", crs = 4326)
st_set_crs(usa, 4326)
usa_360 = st_shift_longitude(usa)

ggplot() + geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") + geom_sf(data = usa_360, color = "snow", fill = "black", size=0.125)



test <- turtles_df_weekly %>% filter(date <= "2023-07-24") %>%
    mutate(time = date) %>%
    # 
    # uncount(100, .id = "frame") %>%
    # filter(time <= frame) %>%
    # arrange(frame, time) %>%
    # group_by(frame) %>%
    group_by(date) %>%
    mutate(x_lag = lag(make360(lon)), 
           y_lag = lag(lat),
           tail = last(time) - time,
           # Make the points solid for 1 frame then alpha 0.3
           point_alpha = if_else(tail == 0, 1, 0.3),
           # Make the lines fade out over 20 frames
           segment_alpha = pmax(0, (20-tail)/20)) %>%
    ungroup() %>%

ggplot(aes(x=make360(lon), y=lat, xend = y_lag, yend = x_lag, group = time)) +
    geom_segment(aes(alpha = segment_alpha)) +
    geom_point(aes(alpha = point_alpha)) +
    scale_alpha(range = c(0,1)) +
    guides(alpha = F) +
    transition_manual(date)



ggplot() +
    # geom_tile(
    #     
    #     data = eov_df,
    #     aes(x = x, y = y, fill = val, group = date)) +
    
    geom_raster(data = eov_df_weekly %>% filter(date == "2023-12-24") , 
                aes(x = x, y = y, fill = val, group = date), interpolate = TRUE) +
    
    
    {
        if (!is.null(tzcf_contour)) {
            # add tzcf contour
            geom_contour(data=eov_df %>% filter(date == "2023-07-24"), aes(x=x, y=y, z = val), colour = tzcf_color, linewidth = 1.25,
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
    # geom_sf(data = usa_360, color = "snow", fill = "black", size=0.5) +
    {
        if (eov =='chla'){
            # scale_fill_stepsn(colours = c( "gray99", cpal[2:length(cpal)]),
            scale_fill_gradientn(colours = c( "gray99", cpal[6:length(cpal)]),
                                 breaks = cbar_breaks,
                                 limits = cbar_limits,
                                 na.value = 'snow',
                                 name = "Chl \n(mg/m^3) \n ")
        } else if (eov =='sst') {
            scale_fill_gradientn(colours = 
                                     # cpal[12:length(cpal)],
                                     # breaks=seq(10,25,2),
                                     # limits = c(9.25,25),
                                     cpal[11:length(cpal)],
                                 
                                 breaks=cbar_breaks, #seq(6,32,2),
                                 limits = c(min(cbar_limits),max(cbar_limits)),
                                 na.value = 'snow',
                                 name = "SST (°C) \n")
        }
    } +
    

    # turtle daily movements -- wc pal
geom_point(data=turtles_df_weekly %>% filter(date <= "2023-07-24") %>%
               mutate(lon = make360(lon)), #aes(x = lon, y = lat, colour =  as.factor(id)), size = size),
           aes(x=lon,y=lat, color = as.factor(id)),#shape = 21,
           stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size, show.legend=FALSE) +
    scale_colour_manual(values = rainbow(25)) +
    
    ## add daily pts border
    geom_point(data=turtles_df_weekly %>% filter(date <= "2023-07-24") %>%
                   mutate(lon = make360(lon)),
               # aes(x=lon,y=lat), color = "azure2",shape = 21,
               aes(x=lon,y=lat, color = as.factor(id)), shape = 21,
               stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size, show.legend=FALSE) +
    
    
    ## add daily pts border
    geom_line(data=turtles_df_weekly %>% filter(date <= "2023-07-24") %>%
              mutate(lon = make360(lon)),
              aes(x=lon,y=lat, color = as.factor(id)),
              lwd = 1, 
              alpha = 0.90, #size=plot_params$turtle_pt_size, 
              show.legend=FALSE) +
    
    
    # release location
    geom_point(data=release_loc, aes(x=lon, y=lat), fill = "lightgray",
               color = "black", shape = 4, size = 5.5) +
    
    labs(x = "\n \n Longitude \n", y = "\n \n Latitude \n \n ") +
    # theme(legend.position = "none") +
    
    theme_minimal() + theme(text=element_text(size=plot_params$plot_text_size)) +
    # # coord_sf(xlim = c(make360(-160), make360(-140)), ylim = c(35, 45), expand = FALSE, crs = st_crs(4326)) +
    coord_sf(xlim = c(make360(e[1]+1), make360(e[2])), ylim = c(e[3], e[4]), expand = FALSE, crs = st_crs(4326)) +
    guides(fill = guide_colourbar(
        barheight = plot_params$barheight,
        ticks = TRUE)
    ) + 
    # facet_wrap(~date) +
    NULL


test = data.frame(x = seq(2011,2021,1),
                y = seq(35,45,1)
                )

xvar="x"; yvar="y"
xvar <- sym(xvar)
yvar <- sym(yvar)

ggplot(test, aes(x=(!!xvar), y=(!!yvar))) +
    geom_point(shape = 1, alpha=0.8) +
    geom_line(alpha=0.3) 



library(ggplot2)

discrete_gradient_pal <- function(colours, bins = 5) {
    ramp <- scales::colour_ramp(colours)
    
    function(x) {
        if (length(x) == 0) return(character())
        
        i <- floor(x * bins)
        i <- ifelse(i > bins-1, bins-1, i)
        ramp(i/(bins-1))
    }
}

scale_colour_discrete_gradient <- function(..., colours, bins = 5, na.value = "grey50", guide = "colourbar", aesthetics = "colour", colors)  {
    colours <- if (missing(colours)) 
        colors
    else colours
    continuous_scale(
        aesthetics,
        "discrete_gradient",
        discrete_gradient_pal(colours, bins),
        na.value = na.value,
        guide = guide,
        ...
    )
}


gg + 
    scale_fill_manual("SST (°C) \n", na.translate = F,
                      values = cpal[7:length(cpal)], 
                      # limits = c(6,34),  drop = FALSE,
                      na.value="transparent",
                      labels = seq(6,34,1)
    )




scale_fill_manual(values = cpal, #name = "", 
                  labels = cbar_labels, drop = F, na.translate = F,
                  guide = guide_legend(label.vjust=+1.2, barwidth = 1, barheight = 32,
                                       frame.colour = "black", ticks.colour = "black", ncol =1, reverse=T)) +
    theme(legend.key = element_rect(color="gray80", fill = 'white'))



## 31 Jan CBAR MANUAL


gg <- 
    ggplot() +
    
    # # geom_tile(data = eov_test %>% subset(!is.na(val)),  aes(x = x, y = y, fill = factor(cut(val, breaks=zCuts, labels=zCuts[2:29])), group = date)) +
    # geom_raster(data = eov_test %>% subset(!is.na(val)) ,
    #             aes(x = x, y = y, fill = val, group = date), interpolate = TRUE) +
    #             # aes(x = x, y = y, fill = factor(cut(val, zCuts)), group = date), interpolate = TRUE) +
    # 
    geom_contour_filled(data = eov_test %>% subset(!is.na(val)) ,
                        aes(x = x, y = y, z = val, group = date), breaks = seq(6,34,1)) + 
    
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
            # 
            scale_fill_gradientn(colours = 
                                     # cpal[12:length(cpal)],
                                     # breaks=seq(10,25,2),
                                     # limits = c(9.25,25),
                                     cpal[11:length(cpal)],
                                 
                                 breaks=cbar_breaks, #seq(6,32,2),
                                 limits = c(min(cbar_limits),max(cbar_limits)),
                                 na.value = 'snow',
                                 name = "SST (°C) \n")
            
        }
    }


gg + 
    scale_fill_manual(values = cpal[7:length(cpal)], name = "SST (°C) \n", 
                      # limits = c(6, 28),
                      labels = seq(cbar_limits[1], cbar_limits[2], 1), drop = F, na.translate = F,
                      guide = guide_legend(label.vjust=+1.2, barwidth = 1, barheight = 32,
                                           frame.colour = "black", ticks.colour = "black", ncol =1, reverse=T)) +
    theme(legend.key = element_rect(color="gray80", fill = 'white'))


gg + scale_fill_gradientn(limits = c(6,34),
                          colours= cpal[7:length(cpal)],
                          values = seq(cbar_limits[1], cbar_limits[2], 1),
                          breaks=seq(cbar_limits[1], cbar_limits[2], 1), labels=format(seq(cbar_limits[1], cbar_limits[2], 1)))

gg + scale_fill_manual(drop=FALSE, values=cpal[7:length(cpal)], na.value="#EEEEEE", name="SST (°C) \n",
                       labels = seq(cbar_limits[1], cbar_limits[2], 1)) 


# test <- eov_test %>%
#     mutate(ctry = cut(val, breaks = c(zCuts), labels = cpal[7:length(cpal)])) %>%
#     mutate(ctryx = factor(ctry))
#     
# test |> head()
# 
# ggg <- ggplot() + geom_raster(data = test %>% subset(!is.na(val)) , aes(x = x, y = y, fill = ctry, group = date), interpolate = TRUE) +
#     scale_fill_manual(values = test$ctryx, drop=F)
gg + scale_fill_brewer(palette = "Spectral", drop = FALSE)

gg + scale_fill_distiller(
    super = metR::ScaleDiscretised,
    palette = "Spectral",
   # palette = cpal[7:length(cpal)],
   # palette = rev(Spectral.colors(28)),
   limits = c(6, 34))

gg +   scale_fill_gradient(low="lightblue", high="red",
                           breaks=seq(6,34,1),
                           limits=c(6, 34), guide = "legend") 


Spectral.colors <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
gg +   scale_fill_manual(values = rev(Spectral.colors(28)),drop=TRUE,
                         # limits = c(6, 34),
                         )


gg + scale_fill_gradientn(
    colours = cpal[7:length(cpal)],
    breaks =seq(6, 34, 1), limits = c(6, 34))



ggplot() +
    
    # geom_tile(data = eov_test %>% subset(!is.na(val)),  aes(x = x, y = y, fill = factor(cut(val, breaks=zCuts, labels=zCuts[2:29])), group = date)) +
    geom_contour_filled(data = eov_test %>% subset(!is.na(val)) ,
                aes(x = x, y = y, z = val, group = date), breaks = seq(6,34,1)) + 
    
gg +
    theme(plot.title = element_text(size = 10,hjust = 0.5)) +
    scale_fill_manual(values = cpal[7:length(cpal)],drop=FALSE) +
    
    theme(plot.title = element_text(size = 10,hjust = 0.5)) +
    # scale_fill_manual(values = heat.colors(16),drop=FALSE) +
    guides(fill = guide_colorsteps(barheight = unit(par("pin")[2], "in")))
    

gg + 
    scale_fill_manual(values = cpal[7:length(cpal)], name = "SST (°C) \n", 
                      # limits = c(6, 28),
                      labels = seq(6, 34, 1), drop = F, na.translate = F,
                      guide = guide_legend(label.vjust=+1.2, barwidth = 1, barheight = 32,
                                           frame.colour = "black", ticks.colour = "black", ncol =1, reverse=T)) +
    theme(legend.key = element_rect(color="gray80", fill = 'white'))


# bottom lege
    guides(fill = guide_colorsteps(direction = "horizontal",
                                   barwidth = unit(par("pin")[1], "in"))) +
    theme(legend.position = "bottom")
    
    
    
    
## MASK GoC
    
# set goc mask
pl_2 <- rbind(c(-110, 23.75), c(-122, 40), c(-110, 40), c(-110, 23.75))
pl_2 <- SpatialPolygons(list(Polygons(list(Polygon(pl_2)), 1)))

# goc_mask <- st_as_sf(goc_df,coords = c("lon", "lat"), 
#                      crs = 4326, agr = "constant")
# 
# goc_mask <- st_as_sf(pl_2, fill=TRUE, plot =FALSE)
# goc_mask <- st_as_sf(goc_mask, wkt = "geom", crs = 4326)
# st_set_crs(goc_mask, 4326)
# # goc_mask_360 = st_shift_longitude(goc_mask)

## THIS WORKS!!!
ras_masked <- mask(ras_subset[[1]], goc_mask, inverse=T)
plot(ras_masked)
# test <- crop(ras_subset[[1]],pl_2)
# plot(test)

library(sf)
library(dplyr)

goc_df <- data.frame(id = 1,
                      lon = c(make360(-110), make360(-122), make360(-110), make360(-110)),
                      lat = c(23.75, 40, 40, 23.75)) %>%
    sf::st_as_sf(coords = c('lon','lat'), 
                 # this part is important!
                 crs = 4326) %>% 
    rename(geo_col = geometry)

# to double check a visual overview; looks legit...
mapview::mapview(goc_df)




## cbar masked
test + scale_fill_manual(values = cpal[9:length(cpal)], name = "SST (°C) \n", 
                         labels = brks2, drop = F, na.translate = F,
                         guide = guide_legend(label.vjust=+1.2, barwidth = 1, #barheight = 32,
                                              frame.colour = "black", ticks.colour = "black", ncol =1, reverse=T)) 

cpal <- c(smooth_rainbow(length(brks2), range = c(0, 0.9)), "#9e2a2b", "firebrick4")
brks2 = c(0.03, 0.1, 0.2, 0.4,  2, 10, 30)
test + scale_fill_gradientn(limits = c(0.01,20), #trans = "log",
                            colours= cpal[11:length(cpal)],
                            # values = brks2
                            breaks=brks2, labels=format(brks2)
                            )

## THIS WORKS!!!!! ---------
brks2 = c(0.03, 0.1, 0.2, 0.4,  2, 10, 30)
test + scale_fill_gradientn(trans="log10", colours = cpal[11:length(cpal)],
                            breaks=brks2, labels=format(brks2))
## -------

# library(scales)
# brks2 = c(seq(0.03,0.1, 0.005), 0.2, 0.3, 0.4, 1, 2, 6, 10, 20, 30)
# test + scale_fill_gradientn(name="..",
#                             limits=c(0,30),
#                             breaks=brks2,
#                             values = brks2,#these are percentage, for example, to get color red, you need to reach 1 (100%) of high_f
#                             colours=cpal[1:length(cpal)])

test + scale_fill_distiller(palette= "Spectral", direction=-1,
                            values = brks2,
                            limits = c(0.01, 30))


mapview::mapview(bathy_shp, color = 'black', alpha.regions = 0)

# vert cbar
test + scale_fill_manual(values = cpal[7:length(cpal)], name = "SST (°C) \n", 
                  labels = seq(4,30, 1), drop = F, na.translate = F,
                  guide = guide_legend(label.vjust=+1.2, barwidth = 1, #barheight = 32,
                                       frame.colour = "black", ticks.colour = "black", ncol =1, reverse=T)) 


## this works! ----------
panel_height <- unit(1,"npc") - sum(ggplotGrob(test)[["heights"]][-3]) - unit(1,"line")

t2 = test +  guides(fill = guide_legend(title = cbar_title, ncol=1, reverse=T, barheight = panel_height, limits = cbar_breaks,
                                   ticks = TRUE)) 

ggsave(t2, 
       file = str_c('./anim_figs/', i, "_", dates[i], "_", eov, "_tracks_test.png"), 
       width=14, 
       # height=8.5,   # comment out height to get auto aspect ratio set!
       bg = 'white')
 ## ---------


turtles_df %>%
    mutate(start_of_week = floor_date(date, "week") %>% as.Date(.)) %>%
    mutate(end_of_week = ceiling_date(date, "week") %>% as.Date(.)) %>%
    group_by(start_of_week, id) %>%
    # group_by(end_of_week, id) %>%
    summarize(lat = mean(lat, na.rm = TRUE),
              lon = mean(lon, na.rm = TRUE), .groups = "drop") %>%
    rename('date' = 'start_of_week')
    # rename('date' = 'end_of_week')




## bathy
i = n
weekly_tracks_plot_list[[i]] +
geom_contour(data=bathy_df, aes(x=make360(long), y=lat, z = z), colour = "azure3", linewidth = 0.75,
             breaks = c(-140)) +
    geom_contour(data=bathy_df, aes(x=make360(long), y=lat, z = z), colour = "azure4", linewidth = 0.75,
                 breaks = c(-500))


library(cowplot)
leg <- get_legend(tracks_plot_list[[1]]   + theme(legend.position="right"))

p1 <- tracks_plot_list[[1]] + theme(legend.position="none")

plot_grid(p1, leg, ncol = 2, rel_widths = c(2, .1))
