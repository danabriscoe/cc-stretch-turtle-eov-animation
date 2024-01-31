# render EOV report.R
# note: include ../ fpath when running script from command line

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")


# render plots function -----
render_ncdfs = function(eov, 
                        enddate, startdate, timestep,
                        nc_path, bbox) {
    rmarkdown::render(
        "code/01_get_ncdf.Rmd",
        # "code/01_get_ncdf.Rmd",
        params = list(eov=eov, 
                      enddate = enddate, startdate = startdate,
                      nc_path = nc_path, bbox = bbox),
        envir = parent.frame()
    )
}



## 1 Get new netcdfs ----

# get ssta daily
render_ncdfs(
    eov = "ssta",
    timestep = "daily",
    enddate <- Sys.Date() - 2,
    startdate <- "2023-07-10",
    nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac",
    bbox <- dplyr::tibble(ymin=20, ymax=50,xmin=-180, xmax=-110)
)

# get chla daily
render_ncdfs(
    eov = "chla",
    timestep = "daily",
    enddate <- Sys.Date() - 4,
    startdate <- "2023-07-10",
    nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac",
    bbox <- dplyr::tibble(ymin=20, ymax=50,xmin=-180, xmax=-110)
)

# sla with u, v
render_ncdfs(
    eov = "sla_uv",
    timestep = "daily",
    enddate <- Sys.Date() - 2,
    startdate <- "2023-07-10",
    nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac",
    bbox <- dplyr::tibble(ymin=20, ymax=50,xmin=-180, xmax=-110)
)

