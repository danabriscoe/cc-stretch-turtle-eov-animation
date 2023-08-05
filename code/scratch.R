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