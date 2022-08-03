library(FIELDimageR)
library(raster)
library(logging)

logging::basicConfig()
logging::addHandler(logging::writeToConsole, logger = "User")

parser <- argparse::ArgumentParser()
parser$add_argument("-i", "--image_path", help = "Path to the pollen image.")
parser$add_argument("-c", "--crop_value_normal", default = 15, help = "Crop value for the normal pollen.")
parser$add_argument("-o", "--crop_value_germinated", default = 16, help = "Crop value for the germinated pollen.")
parser$add_argument("-w", "--min_size_normal", default = 1, help = "Minimum considerable size for the normal pollen particle.")
parser$add_argument("-m", "--min_size_germinated", default = 0.005, help = "Minimum considerable size for the germinated pollen particle.")
args <- parser$parse_args()

main <- function(args){
  
  image_path <- args$image_path
  crop_value_normal <- args$crop_value_normal
  crop_value_germinated <- args$crop_value_germinated
  min_size_normal <- args$min_size_normal
  min_size_germinated <- args$min_size_germinated
  
  logging::loginfo("Reading image.")
  EX.P<-stack(image_path)

  logging::loginfo("Reducing image resolution for faster analysis.")
  EX.P<-aggregate(EX.P,fact=4) 
  plotRGB(EX.P, r = 1, g = 2, b = 3)
  
  logging::loginfo("Create a shapefile for the entire image.")
  EX.P.shapeFile<-fieldPolygon(EX.P,extent = T)
  
  logging::loginfo(paste0("Using the bright index modifed. Every crop value above ", crop_value_normal, " will be removed."))
  EX.P.R1<- fieldMask(mosaic = EX.P,index = "BIM", cropValue = crop_value_normal, cropAbove = T)
  plotRGB(EX.P.R1$newMosaic)
  
  logging::loginfo(paste0("Counting all the pollens with size above: ", min_size_normal))
  EX.P.Total<-fieldCount(mosaic = EX.P.R1$mask, fieldShape = EX.P.shapeFile$fieldShape, minSize = min_size_normal) 
}


main(args)
