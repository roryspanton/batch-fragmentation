## Batch fragmentation in R

rm(list=ls())

library(tidyverse)
library(magick)

# Define global variables
imglen <- 400 # img width and height in px
fraglen <- 20 # fragment width and height in px


# Create 'not in' function
`%notin%` <- Negate(`%in%`)

# Get a list of all pdf images in a folder of input images
imglist <- list.files(path=paste0(getwd(), "/input/"))

# Import images, scale them to 400x400 (assuming equal width/height in first place)
images <- map(imglist, ~image_read_pdf(paste0(getwd(), "/input/", .x))) %>%
  map(~image_scale(.x, imglen))

# Create tbl with number of visible squares at each fragmentation level, according to a power law
vistb <- tibble(frag_level = 1:10, 
              prop_vis = map_dbl(1:10, ~ 0.75^(10-.x)),
              nvis = ceiling(imglen * prop_vis))

# Work out the proportion of new squares to conceal in each stage of fragmentation
# (working backwards from not fragmented to most fragmented)
conceal_prop <- rev(map_dbl(2:10, ~ vistb$nvis[.x] - vistb$nvis[.x-1]))

# Get lists of all possible 20x20 squares within 400x400 grid
gridcombos <- tibble(
  row     = 1:imglen,
  ytop    = rep(seq(from=0, to=imglen-fraglen, by=fraglen), fraglen),
  ybottom = rep(seq(from=fraglen, to=imglen, by=fraglen), fraglen),
  xleft   = map(seq(from=0, to=imglen-fraglen, by=fraglen), ~rep(.x, fraglen)) %>% unlist(),
  xright  = map(seq(from=fraglen, to=imglen, by=fraglen), ~rep(.x, fraglen)) %>% unlist()
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
    
    # Draw image object and store
    fragimg <- image_draw(images[[img]])
    
    # Draw each white rectangle on stored image sequentially
    for (q in 1:length(drawn_sqs[[i+1]]$row)) {
      rect(drawn_sqs[[i+1]]$xleft[q], drawn_sqs[[i+1]]$ybottom[q], 
           drawn_sqs[[i+1]]$xright[q], drawn_sqs[[i+1]]$ytop[q], col="white", border=NA)
    }
    
    # Collate jpeg file
    dev.off()
    # Write image
    image_write(fragimg, 
                path=paste0(getwd(), "/output/", img, "_", "frag", 10-i, ".jpg"),
                format="jpg")
  }
  
  # Draw and write the final, unfragmented image
  fragimg <- image_draw(images[[img]])
  image_write(fragimg,
              path=paste0(getwd(), "/output/", img, "_frag10.jpg"), format="jpg")
}

