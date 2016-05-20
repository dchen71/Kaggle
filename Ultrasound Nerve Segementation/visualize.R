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
train1 = readImage('input/train/1_8.tif')
train1.mask = readImage('input/train/1_8_mask.tif')

#Convert to rgb
train1 = toRGB(train1)

#Overlay mask
segmented = paintObjects(train1.mask, train1, col=c('red'))

#Display image
display(segmented)
