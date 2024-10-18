## 01_prep_wc_data.R

# script to pull:
#   - wc data (currently raw ARGOS data) from STRETCH cohort 1 (released July 2023)
#   - eventually will update to use pre-processed tracks

# save output as rds or just load straight into index.rmd

# dk briscoe: 18 Jan 2024 (modified from cc-stretch-northern-lats version)


## Load Libraries ----
library(tidyverse)
library(data.table)
library(glue)
library(here)

# Get Params from Rmd -----
# params$release_yr = 24


## Source functions ----

## Attach metadata to df
attach_metadata <- function(dat_df, meta_df, by="id"){
    merged_df <- 
        dat_df %>%
        merge(., meta_df, by="id", all.x = TRUE)
    
    return(merged_df)
}

## Calc avg daily location
calc_avg_daily_loc <- function(x, ...) {
    # check date attr
    if(!is.Date(x$date)){
        x <- x %>%
            mutate(date = as.Date(date))
    } else {
        print('error: fix date attribute')
    }
    
    # vars to group
    vars1 <- syms(...) # must be in quotes
    
    # calc daily avg by groups 
    ret <- x %>%
        group_by(!!!vars1) %>%
        summarise(lat = mean(lat), 
                  lon = mean(lon),
                  scl_cm = mean(scl_cm), 
                  .groups = 'drop')
    return(ret)
}

## Load raw data (from wc downloads)
load_wc_downloads <- function(wc_files){
    ret <- rbindlist(lapply(wc_files, fread)) %>%
        dplyr::select('Platform ID No.', 'Latitude', 'Longitude', 'Loc. quality', 'Loc. date') %>%
        dplyr::rename(id = 1,
                      lat = 2, 
                      lon = 3, 
                      loc_quality = 4, 
                      date = 5
        ) %>%
        mutate(date = as.POSIXct(date, format= "%m/%d/%Y %H:%M:%S", tz="UTC")) %>%
        as_tibble()
    
    return(ret)
}

## Load metadata files
load_metadata_xls <- function(meta_files){
    ret <- readxl::read_excel(path = meta_files) %>%
        dplyr::select(1,3,5,6) %>%
        dplyr::rename("turtle_num"=1, "id"=2, "turtle_name"=3, "scl_cm"=4)
    return(ret)
}    


## 1) Load Cohort Data ----

# wc_files <- list.files('~/Downloads/batch/', pattern = "All.csv", recursive=T, full.names=T) # change dir later

if(params$release_yr == "23"){
    wc_files = list.files('~/Downloads/batch/', pattern = "All.csv",recursive=T, full.names=T) # temp location while tags still running - change dir later

    raw_data_cohort_1 <- load_wc_downloads(wc_files) %>%
        filter(lat > 0 & lat < 60) %>% # discard extraneous loc's
        filter(date >= '2023-07-11 04:30:00') # guarantees pre-release locs aren't included
    
    # get metadata
    meta_cohort_1 <- tryCatch( 
        {
            # load_metadata_xls("../utils/2023_Turtle_Info.xlsx")
            load_metadata_xls(file.path(
                here() %>% dirname(),
                'cc-stretch-northern-lats',"utils","2023_Turtle_Info.xlsx"))
        },
        error = function(e) {
            # load_metadata_xls("./utils/2023_Turtle_Info.xlsx")
            print('check file path!')
        }
    )
    
    # merge dfs to align ID and Turtle Names
    raw_data_cohort_w_names <- raw_data_cohort_1 %>%
        attach_metadata(., meta_cohort_1)
    
    
    ## 3) Calc Daily Location Avg for each ID -----
    daily_avg_data_cohort <- raw_data_cohort_w_names %>%
        
        filter(loc_quality != 0) %>% # added 17 Jan 2024 -- to deal with raw argos bad data point (tsubaki, 12 jan 2024 locs - remove loc class = 0)
        
        calc_avg_daily_loc(., c("id", "turtle_num", "turtle_name", "date")) %>%
        mutate(lon360 = make360(lon)) %>%
        relocate("lon360", .after = "lon") %>%
        mutate(year = lubridate::year(date),
               month = lubridate::month(date),
               day = lubridate::day(date)) %>%
        mutate(group = 'cohort1') %>%
        as_tibble()
    
    # # generalize output for consistency
    # daily_avg_data_cohort <- daily_avg_data_cohort_1
    
    
} else if(params$release_yr == "24"){
    wc_files = list.files('~/Downloads/batch_cohort2/', pattern = "All.csv",recursive=T, full.names=T) # temp location while tags still running - change dir later

    raw_data_cohort_2 <- load_wc_downloads(wc_files) %>%
        filter(lat > 0 & lat < 60) %>% # discard extraneous loc's
        filter(date >= '2024-07-07 04:30:00') # guarantees pre-release locs aren't included  
    
    # get metadata
    meta_cohort_2 <- tryCatch( 
        {
            # load_metadata_xls("../utils/2024_Turtle_Info.xlsx")
            # load_metadata_xls(here("utils","2024_Turtle_Info.xlsx"))
            load_metadata_xls(file.path(
                here() %>% dirname(),
                'cc-stretch-northern-lats',"utils","2024_Turtle_Info.xlsx"))
        },
        error = function(e) {
            # load_metadata_xls("./utils/2024_Turtle_Info.xlsx")
            print('check file path!')
        }
    )
    
    # merge dfs to align ID and Turtle Names
    raw_data_cohort_w_names <- raw_data_cohort_2 %>%
        attach_metadata(., meta_cohort_2)
    
    
    ## 3) Calc Daily Location Avg for each ID -----
    daily_avg_data_cohort <- raw_data_cohort_w_names %>%
        
        filter(loc_quality != 0) %>% # added 17 Jan 2024 -- to deal with raw argos bad data point (tsubaki, 12 jan 2024 locs - remove loc class = 0)
        
        calc_avg_daily_loc(., c("id", "turtle_num", "turtle_name", "date")) %>%
        mutate(lon360 = make360(lon)) %>%
        relocate("lon360", .after = "lon") %>%
        mutate(year = lubridate::year(date),
               month = lubridate::month(date),
               day = lubridate::day(date)) %>%
        mutate(group = 'cohort2') %>%
        as_tibble()
    
    # # generalize output for consistency
    # daily_avg_data_cohort <- daily_avg_data_cohort_2
}



## fin
