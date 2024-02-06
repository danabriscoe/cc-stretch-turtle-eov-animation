# crop_image.R

# script to quickly crop figure and reduce negative space 

crop_image <- function(flist){
library(magick)

    m_png <- lapply(seq(1:length(flist)), function(i) {
              image_border(image_trim(image_read(flist[i])), "white", "30x30")
             })

    lapply(seq(1:length(flist)), function(i) {image_write(image=m_png[[i]], str_c(flist[i]), format='png')})

}