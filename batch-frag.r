## Batch fragmentation in R
## Author: Rory Spanton

rm(list=ls())

library(tidyverse)
library(magick)

## Parameter setting ------------

# Define global parameters
imglen <- 400 # img width and height in px
fraglen <- 20 # fragment width and height in px
fraglevels <- 15 # fragmentation levels (including fully revealed image)

## Image read/preliminary adjustments ------------

# Create 'not in' function
`%notin%` <- Negate(`%in%`)

# Get a list of all pdf images in a folder of input images
imglist <- list.files(path=paste0(getwd(), "/input/"))

# Import images, scale them to 400x400 (assuming equal width/height in first place)
images <- map(imglist, ~image_read(paste0(getwd(), "/input/", .x))) %>%
  map(~image_scale(.x, imglen)) %>%
  map(~image_modulate(.x, saturation = 0))

# Get lists of all possible 20x20 squares within 400x400 grid
gridcombos <- tibble(
  row     = 1:imglen,
  ytop    = rep(seq(from=0, to=imglen-fraglen, by=fraglen), fraglen),
  ybottom = rep(seq(from=fraglen, to=imglen, by=fraglen), fraglen),
  xleft   = map(seq(from=0, to=imglen-fraglen, by=fraglen), ~rep(.x, fraglen)) %>% unlist(),
  xright  = map(seq(from=fraglen, to=imglen, by=fraglen), ~rep(.x, fraglen)) %>% unlist()
)

## Fragmentation algorithm -------------

# Initialize progress bar
cat("Fragmentation Progress:")

pb <- txtProgressBar(title = "Fragmentation Progress", 
                     label = paste("0 /", length(imglist), " images fragmented:"),
                     min = 0, max = length(imglist), initial = 0)

for (img in 1:length(imglist)) {
  
  # Create empty tbl as first part of 'drawn_sqs' list
  drawn_sqs <- list(tibble())
  # Work out which n by n squares of the image are white or not
  fillvector <- map2(gridcombos$xleft, gridcombos$ytop, 
                     ~ image_crop(image_edge(images[[img]]), geometry = paste0(fraglen, "x", fraglen, "+", .x, "+", .y))) %>%
    map_lgl(~ any(as.vector(image_data(.x))) != 00)
  
  # Append to temporary copy of grid, then filter out white squares
  tmpgrid <- gridcombos %>%
    mutate(filled = fillvector) %>%
    dplyr::filter(filled == T)
  # Calculate proportion of visible squares at each frag level, and number of visible squares at each level
  tmpvistb <- tibble(frag_level = 1:fraglevels, 
                     prop_vis = map_dbl(1:fraglevels, ~ 0.75^(fraglevels-.x)),
                     nvis = ceiling(length(tmpgrid$filled) * prop_vis))
  
  # Work out the number of new squares to conceal in each stage of fragmentation
  # (working backwards from not fragmented to most fragmented)
  conceal_prop <- rev(map_dbl(2:fraglevels, ~ tmpvistb$nvis[.x] - tmpvistb$nvis[.x-1]))
  
  for (i in 1:(fraglevels-1)) {
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
    image_write(fragimg, path=paste0(getwd(), "/output/", img, "-frag", fraglevels-i, ".jpg"), format="jpg")
  }
  
  # Draw and write the final, unfragmented image
  fragimg <- image_draw(images[[img]])
  image_write(fragimg, path=paste0(getwd(), "/output/", img, "-frag", fraglevels, ".jpg"), format="jpg")
  
  # Set progress bar status
  setTxtProgressBar(pb, img, label=paste(img, "/", length(imglist), "images fragmented"))
}

# Close progress bar
close(pb)
