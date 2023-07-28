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


# # plot timeseries
# plot_timeseries <- function(data, #unit='day',
#                             cpal, ylimits, ybreaks, sst_thresh){
#     
#     g <- ggplot(data = data, aes(date, value, group = ID, text = str_c("Lon: ", make180(lon), ifelse(make180(lon) > 0, "°E \nSST", "°W \nSST: "), round(value,1),"°C"))) +
#         geom_point(aes(x=date, y=value, color = factor(make180(lon))), size = 3) +
#         geom_line(aes(group=ID, color = factor(make180(lon))), linewidth = 1) +
#         
#         scale_y_continuous(limits=ylimits, breaks=ybreaks) +
#         scale_x_date(
#             # date_breaks = "1 week", date_minor_breaks = "1 week",             # if you want Monday (first of next week)
#             breaks = seq(data$date[1], data$date[length(data$date)],7),         # else if you want Sunday of current weekly avg
#             date_labels = "%d %b",
#             limits = c(data$date[1], data$date[length(data$date)])) +
#         
#         theme_minimal() +
#         theme(legend.position='bottom',
#               axis.text.x=element_text(angle = 35, hjust = 0.5, 
#                                        size=8, vjust=0.5),
#               plot.caption = element_text(hjust=0)) +
#         # ggthemes::scale_color_tableau()
#         scale_colour_manual(values = cpal)
#     
#     return(g)
#     
# }
# 
