# title: 00_automate_EOV_helper_functions.R
#
# description: Helper functions used to download and process EOV data 
# original script found in : strethc-cc-sst-automation/code/00_automate_SST_helper_functions.R
# author: dk briscoe
#
# date: july 2023



# get date range
getDateRange <- function(startdate, enddate, unit = "month", format = "%Y/%mm/%dd"){
    
    ret <- seq(as.Date(startdate), as.Date(enddate), by = unit,format = format)
    
    return(ret)
}

# get ncdf
getNCDF <- function(url, eov, varname, dataset_ID, bbox, dt, ncpath, alt=NULL){
    
    dt <- as.character(dt)
    
    # assign filename
    filenm<-paste0(eov,"_",dataset_ID,"_",as.Date(dt),".nc") 

    if(dataset_ID == "nesdisVHNnoaaSNPPnoaa20NRTchlaGapfilledDaily"){
        filenm <- filenm %>%
            str_sub(.,22)
    } else {
        filenm <- filenm
    }

    
    # get netcdf
    if(!file.exists(paste0(ncpath, "/", filenm))){
        if(is.null(alt)){
            url_base <- paste(url,
                          dataset_ID,
                          ".nc?",
                          varname,
                          "[(", dt, "):1:(", dt, ")][(",
                          bbox$ymax, "):1:(", bbox$ymin, ")][(",
                          bbox$xmin, "):1:(", bbox$xmax, ")]",
                          sep="")
        } else {
            url_base <- paste(url,
                              dataset_ID,
                              ".nc?",
                              varname,
                              "[(", dt, "):1:(", dt, ")][(",
                              alt, "):1:(", alt, ")][(",
                              bbox$ymax, "):1:(", bbox$ymin, ")][(",
                              bbox$xmin, "):1:(", bbox$xmax, ")]",
                              sep="")
        }
        return(download.file(url_base, destfile=paste0(ncpath, "/", filenm)))
    } else {
        message(dataset_ID, 'data already downloaded! \n Located at:\n', str_c(ncpath,filenm, sep='/'))
    }
    
}

# # get_shipping_route
# get_shipping_route <- function(port_start, port_end, lon_type='360'){
#     
#     port_end[1] <- if_else(port_end[1] > 180, make180(port_end[1]), port_end[1])
#     
#     # source: https://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
#     suppressWarnings(
#         inter <-
#             geosphere::gcIntermediate(port_start, port_end, n = 100, addStartEnd = TRUE) %>%
#             as.data.frame()
#     )
#     
#     if(lon_type=='360'){
#         ret <- inter %>% mutate(lon = make360(lon))  #inter$lon <- make360(inter$lon)
#     } else if(lon_type=='180'){
#         ret <- inter
#     }
#     return(ret %>% 
#                mutate(across(everything(), round, 2)))
# }


# ## get_timeseries (formerly 'get_sst_ts')
# get_timeseries <- function(rasIn, pts2extract, subset_dt){
#     
#     # make sure rasIn and pts are in same lon system: 180/360. for simplicity, get everythign in -180/180 for now. revisit
#     
#     
#     if(!min(extent(rasIn)[1])<0){
#         rasIn <- rotate(rasIn)
#     }
#     if(!min(pts2extract$x)<0){
#         pts2extract <- pts2extract %>%
#             mutate(x = make180(x))
#     }
#     
#     rr <- raster::subset(rasIn, which(getZ(rasIn) >= subset_dt))
#     rr <- raster::setZ(rr, subset_dt)
#     
#     names(rr) <- getZ(rr) %>%
#         format(., "%b-%d-%Y") %>%       # prevents X in front of date
#         as.character() %>%
#         str_replace(., " ", "_")
#     
#     x_df <- cbind(raster::extract(rr, pts2extract, df = T), pts2extract)
#     
#     x_df_long <- reshape2::melt(x_df, id.vars = c("ID", "x", "y"), variable.name = "layer") %>%
#         mutate(layer = lubridate::mdy(layer),
#                # date  = layer %>% as.Date()) %>%
#                date = lubridate::parse_date_time(layer,"ymd")) %>%
#         
#         separate(layer, c("year", "month", "day"), "-") %>%
#         rename("lon" = "x", "lat" = "y")
#     
#     return(x_df_long)
# }



# parseDT
parseDT <- function(x, idx, start, stop, format){
    ret <- fs::path_file(x) %>%
        substr(., start=start, stop=stop) %>%
        as.character(strptime(.,format=format,tz='UTC'))
    return(ret)
}

## plot funcs ----
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


scale_x_longitude <- function(xmin=0, xmax=360, step=1, ...) {
    xbreaks <- seq(xmin,xmax,step)
    # xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 180, paste0(x, "°E"), ifelse(x > 0, paste0("", abs(make180(x)), "°W"),x))))
    xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 180, paste0(x, " E"), ifelse(x > 180, paste0("", abs(make180(x)), " W"),ifelse(x == 180, paste0(x, " "),x)))))
    return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0)))
}


library(leaflet)


get_npac_map <- function(xy, lon_type = '360', add_deploy_lons=TRUE, cpal, col_borders=TRUE){
    if(lon_type=='360'){
        xy <- xy %>% mutate(x = make360(x), ID = seq(1,nrow(.),1))
        ship_route_pts <- ship_route_pts |> mutate(lon = make360(lon))
    }
    
    # get labels
    labels <- sprintf(
        # "<strong>Lat, Long</strong><br/>%s°N, %s°W",
        # "<strong>Lat: %s°N</strong><br/> <strong>Lon: %s°W</strong>",
        "<strong>Potential Release Location </strong><br/><strong>Lat: %s °N</strong><br/> <strong>Lon: %s °W</strong>",
        # "Lat: %g °N<br/>Lon: %s °W",
        unique(xy$y), unique(make180(xy$x))
    ) %>% 
        lapply(htmltools::HTML)
    
    # leaflet map
    if(add_deploy_lons){
        ship_pts <- ship_route_pts %>% 
            slice(., 1:(n() - 2)) # trim shipping great circle route end pts
        
        map <- ship_pts |>
            leaflet() |>
            # fitBounds(120, 15, 250, 60) %>%
            # setView(210, 30, zoom = 3) |>. # full npac view
            setView(210, 32, zoom = 4) |>
            addTiles() |>
            
            addCircleMarkers(lng = ship_pts$lon, lat = ship_pts$lat, color = 'azure4',radius = 2, weight=0.5) |>
            
            addPolylines(
                data = ship_pts,
                lng = ~lon, 
                lat = ~lat,
                weight = 3,
                opacity = 3,
                color = 'gray'
            ) |>
            
            addCircleMarkers(lng = xy$x[ceiling(xy$x) == 200], 
                             lat = xy$y[ceiling(xy$x) == 200], #color = cpal[1],
                             color = ifelse(col_borders, "black", cpal[1]), weight = 2,
                             fillColor = cpal[1],
                             stroke = TRUE,
                             radius = 7, 
                             label = labels[1], 
                             labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                         style = list(
                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                             "font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)"
                                                         ))
            ) |>
            addCircleMarkers(lng = xy$x[ceiling(xy$x) == 210], 
                             lat = xy$y[ceiling(xy$x) == 210], #color = cpal[2],
                             color = ifelse(col_borders, "black", cpal[2]), weight = 2,
                             fillColor = cpal[2],
                             stroke = TRUE,
                             radius = 7, 
                             label = labels[2],
                             labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                         style = list(
                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                             "font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)"
                                                         ))
            ) |>
            addCircleMarkers(lng = xy$x[ceiling(xy$x) == 215], 
                             lat = xy$y[ceiling(xy$x) == 215], #color = cpal[3],
                             color = ifelse(col_borders, "black", cpal[3]), weight = 2,
                             fillColor = cpal[3],
                             stroke = TRUE,
                             radius = 7, 
                             label = labels[3],
                             labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                         style = list(
                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                             "font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)"
                                                         ))
            ) |>
            addCircleMarkers(lng = xy$x[ceiling(xy$x) == 220], 
                             lat = xy$y[ceiling(xy$x) == 220], #color = cpal[4], 
                             color = ifelse(col_borders, "black", cpal[4]), weight = 2,
                             fillColor = cpal[4],
                             stroke = TRUE,
                             radius = 7, 
                             label = labels[4],
                             labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                         style = list(
                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                             "font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)"
                                                         ))
            ) 
    } else { # end if add ship_pts == TRUE
        map <- #ship_pts |>
            leaflet() |>
            # fitBounds(120, 15, 250, 60) %>%
            # setView(210, 30, zoom = 3) |>. # full npac view
            setView(210, 32, zoom = 4) |>
            addTiles()
    }
    return(map)
}



# get static plot (for gganimate) --
get_static_plot <- function(eov, eov_df, turtles_df, e, release_loc, cpal = cpal, cbar_breaks, cbar_limits, plot_params){
   
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
                   aes(x=lon,y=lat), color = "azure2",
                   # fill = "#2a9d8f", shape = 21,
                   fill = "#3c096c", shape = 21,
                   stroke = 1, alpha = 0.90, size=plot_params$turtle_pt_size) +
        
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
    
    return(gg)
}

prep_uv_geostrophic <- function(ncIn){
    require(metR)
    require(tidyverse)
    require(lubridate)
    require(oce)
    require(ocedata)
    require(sf)

    
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
        mutate(date = as.POSIXct(date, "%Y.%m.%d", tz = "UTC"))
    
    head(uv_df)
    
    # rename for consistency
    drifter.split <- uv_df  
    
    ret_uv.se <- data.frame()
    uv_dates <- unique(uv_df$date)
    
    for(i in 1:length(uv_dates)){
    
        date_i <- uv_dates[i] # hold onto unique date. will need to add as col at end of ea loop
        
        drifter.split.sf = drifter.split %>% 
            filter(date == uv_dates[i]) %>%
            st_as_sf(coords = c("lon", "lat")) %>%
            st_set_crs(4326)
    
    # drifter.split.sf = drifter.split %>% 
    #     st_as_sf(coords = c("lon", "lat")) %>%
    #     st_set_crs(4326)
    
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
    
    uv.se_w_date <- uv.se %>%
        mutate(date = date_i)
    
    ret_uv.se <- rbind(ret_uv.se, uv.se_w_date)
    print(unique(ret_uv.se$date))
    rm(uv.se_w_date)
    } # end for loop
    
    return(uv.se)
}
