##
## Kaggle Competition - Ultrasound Nerve Segmentation
## Quick start to be able to look at training data
##

#Load libraries
##Probably will need to download bioconductor package
##source("http://bioconductor.org/biocLite.R")
##biocLite("EBImage")
library("EBImage")

#Read file
input_dir = "input/train/"
files = dir(input_dir)
imgs = files[seq(1, length(files), 2)]
masks = files[seq(2, length(files), 2)]

#Get back all image entries by metaprogramming
get_images <- function() {
  for(i in 1:length(imgs)) {
    print(paste0("Assigning image: ", imgs[i]) )
    fName <- paste("imgs.", i, sep="")
    assign(fName, eval(
      substitute(
        toRGB(readImage(paste0(input_dir, imgs[i]))) #Read and convert to rgb
      )
    ),
    envir=parent.frame()
    )
  }
}
get_images()

#Get back all masks entries by metaprogramming
get_masks <- function() {
  for(i in 1:length(imgs)) {
    print(paste0("Assigning mask: ", imgs[i]) )
    fName <- paste("imgs_masks.", i, sep="")
    assign(fName, eval(
      substitute(
        readImage(paste0(input_dir, masks[i]))
      )
    ),
    envir=parent.frame()
    )
  }
}
get_masks()

#Display overlaid mri
display_mri = function(entry){
  #Prep data
  mask = get(paste0("imgs_masks.", entry))
  img = get(paste0("imgs.", entry))
  
  #Paint overlay and display
  overlaid_mri = paintObjects(mask , img, col=c("red"))
  display(overlaid_mri, method = "raster")
}
