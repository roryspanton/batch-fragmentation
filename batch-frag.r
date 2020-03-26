## Batch fragmentation in R

rm(list=ls())

options(scipen=999)
library(tidyverse)
library(magick)

# Create 'not in' function
`%notin%` <- Negate(`%in%`)

# Get a list of all pdf images in a folder of input images
imglist <- list.files(path=paste0(getwd(), "/input/"))

# Import images, scale them to 400x400 (assuming equal width/height in first place)
images <- map(imglist, ~image_read_pdf(paste0(getwd(), "/input/", .x))) %>%
  map(~image_scale(.x, 400))

# Create tbl with number of visible squares at each fragmentation level
vistb <- tibble(frag_level = 1:10, 
              prop_vis = map_dbl(1:10, ~ 0.75^(10-.x)),
              nvis = ceiling(400 * prop_vis))

# Work out the proportion of new squares to conceal in each stage of fragmentation
# (working backwards from not fragmented to most fragmented)
conceal_prop <- rev(map_dbl(2:10, ~ vistb$nvis[.x] - vistb$nvis[.x-1]))

# Get lists of all possible 20x20 squares within 400x400 grid
gridcombos <- tibble(
  row     = 1:400,
  ytop    = rep(seq(from=0, to=380, by=20), 20),
  ybottom = rep(seq(from=20, to=400, by=20), 20),
  xleft   = map(seq(from=0, to=380, by=20), ~rep(.x, 20)) %>% unlist(),
  xright  = map(seq(from=20, to=400, by=20), ~rep(.x, 20)) %>% unlist()
)

for (img in 1:length(imglist)) {
  
  # Create empty tbl as first part of 'drawn_sqs' list
  drawn_sqs <- list(tibble())
  # Create temporary copy of gridcombos tbl
  tmpgrid <- gridcombos
  # Create list for fragmented images
  
  for (i in 1:9) {
    # Select new squares from the tmpgrid
    new_sqs <- sample_n(tmpgrid, conceal_prop[i])
    # create list of drawn squares for each stage
    drawn_sqs[[i+1]] <- bind_rows(drawn_sqs[[i]], new_sqs)
    # filter newly chosen squares from tmpgrid
    tmpgrid <- filter(tmpgrid, row %notin% new_sqs$row)
    
    # Draw image
    fragimg <- image_draw(images[[img]])
    
    # xleft, ybottom, xright, ytop
    for (q in 1:length(drawn_sqs[[i+1]]$row)) {
      rect(drawn_sqs[[i+1]]$xleft[q], drawn_sqs[[i+1]]$ybottom[q], 
           drawn_sqs[[i+1]]$xright[q], drawn_sqs[[i+1]]$ytop[q], col="white", border=NA)
    }
    
    # Write jpeg file
    dev.off()
    
    # Write image
    image_write(fragimg, 
                path=paste0(getwd(), "/output/", img, "_", "frag", 10-i, ".jpg"),
                format="jpg")
    
  }
  
}

