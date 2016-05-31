##
## Kaggle Competition - Ultrasound Nerve Segmentation
## Trying to build a model
##

#Load libraries
##Probably will need to download bioconductor package
#library(h2o)

#Startup h2o
#localH2O = h2o.init()

#test = h2o.importFile(localH2O, path = "input/train/1_1.tif")

#Close h2o
#h2o.shutdown()

#Read mask csv
file_list = read.csv("input/train_masks.csv")

#Add label to indicate presence of Brachial Plexus
file_list$BrachialPresence = (nchar(as.vector(file_list$pixels)) != 0)

#Load libraries
library("doParallel")
library("foreach")
library("mxnet")
library("tiff")
library("data.table")
library("nnet")

#Init list to save images
train_img = vector("list" , 30) #nrow(file_list)

#Load in the photos to the list
for (i in 1:30) { #nrow(file_list)
  file_name = paste0(file_list$subject[i], "_", file_list$img[i], ".tif")
  im = readTIFF(paste0("input/", "train/", file_name))
  train_img[[i]] = im
}


