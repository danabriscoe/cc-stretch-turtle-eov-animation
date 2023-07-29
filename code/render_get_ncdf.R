# render EOV report.R
# note: include ../ fpath when running script from command line

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")


# render plots function -----
render_ncdfs = function(node, url, eov, varname,
                        dataset_ID, enddate, startdate, timestep, alt,
                        nc_path, bbox) {
    rmarkdown::render(
        "../code/01_get_ncdf.Rmd",
        # "code/01_get_ncdf.Rmd",
        params = list(node = node, url = url, eov=eov, varname=varname,
                      dataset_ID = dataset_ID, enddate = enddate, startdate = startdate,  alt = alt,
                      nc_path = nc_path),
        envir = parent.frame()
    )
}



## 1 Get new netcdfs ----
# # get sst daily
# render_ncdfs(
#     node = "pacioos",
#     url = "https://pae-paha.pacioos.hawaii.edu/erddap/griddap/",
#     eov = "sst",
#     varname = "CRW_SST",
#     dataset_ID = "dhw_5km",
#     enddate <- Sys.Date() - 2,
#     startdate <- enddate - 15,
#     timestep = "day",
#     nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/deploy_reports",
#     bbox <- tibble(ymin=20, ymax=50,xmin=-180, xmax=-110)
# )


# get ssta daily
render_ncdfs(
    node = "pacioos",
    url = "https://pae-paha.pacioos.hawaii.edu/erddap/griddap/",
    eov = "ssta",
    varname = "CRW_SSTANOMALY",
    dataset_ID = "dhw_5km",
    enddate <- Sys.Date() - 2,
    startdate <- "2023-07-10",
    timestep = "day",
    alt = NULL,
    nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac",
    bbox <- tibble(ymin=20, ymax=50,xmin=-180, xmax=-110)
)

# get chla daily
render_ncdfs(
    node = "swfsc",
    url = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/",
    eov = "chla",
    varname = "chlor_a",
    dataset_ID = "nesdisVHNnoaaSNPPnoaa20NRTchlaGapfilledDaily",
    enddate <- Sys.Date() - 4,
    startdate <- "2023-07-10",
    timestep = "day",
    alt = 0,
    nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac",
    bbox <- tibble(ymin=20, ymax=50,xmin=-180, xmax=-110)
)



