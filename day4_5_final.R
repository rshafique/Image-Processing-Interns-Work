rm(list = ls())

library(EBImage)
library(png)
library(jpeg)
library(foreign)
library(colorspace)

#### Setting the path in which images are stored
#### You have to customize it according to your location of images in your laptop


setwd("C:/Users/md.forhad.hossain/Desktop/Sample_picture")


#### Creatinig a new object x (list) for using as input 


x <- list.files(all.files = TRUE, no.. = TRUE)
length(x)
x

###################### Finding maximum & minimum dimension of all images


dummy <- matrix(c(vector("numeric",10)), ncol = 2) 
dim(dummy)
 
for(i in 1:length(x)){
   rimage <- readImage(x[i])
   di <- as.vector(dim(rimage))[1:2]
   dummy[i,] <- di
 }

head(dummy)

apply(dummy, 2, max)
apply(dummy, 1, min)

################################################

data <- matrix(rep(0, 51*81*length(x)), nrow = length(x), ncol = 51*81)







####### Processing the data matrix 
for(i in 1:length(x)){
  
  #### input images
  read_original_image <- readJPEG(x[i])
  
  #### Taking only the first channel
  y <- read_original_image[,,1]
  original_version <- as.matrix(y, nrow = dim(y)[1], ncol = dim(y)[2])
  
  #### Displaying the original version of the image
  #### display(rotate1(rotate1(t(nmr))))
  
  #### Grayscale matrix of the original images
  
  test_in_mat <- rotate_row(rotate_row(t(original_version)))
  ## display(test_in_mat)
  
  #### Resizing images
  
  resized_size <- resize_image(test_in_mat, 500,500) 
  
  #### Cropping the desired area 
  
  cropped_img <- as.matrix(resized_size[450:500, 200:280,]) ### Random position (need to be corrected)

  data[i,] <- cropped_img  
  
  
  
  
}

##### Making the dataframe 

dim(data)
final_data <- as.data.frame(data)

#### Storing the data file into a new ".CSV" format file


write.table(final_data, file = "train.csv", sep = ",", col.names = NA)







#######################################


##### Image resizing function

resize_image <- function(input_image, desired_width, desired_hieght){
  
  
  pixels <- as.vector(input_image)
  
  #### initial width/height
  
  initial_width <- nrow(input_image)
  initial_height <- ncol(input_image)
  
  #### Target width/height
  
  target_width <- desired_width
  target_height <- desired_hieght
  
  #### Creating empty vector
  
  temp_vector <- vector("numeric", target_width*target_height)
  
  #### Computing aspect ratios
  
  width_ratio <- initial_width/target_width
  height_ratio <- initial_height/target_height
  
  #### Resizing Image
  
  for(i in 0:(target_height-1)){
    for(j in 0:(target_width-1)){
      pixels_width <- floor(j*width_ratio)
      pixels_height <- floor(i*height_ratio)
      temp_vector[i*target_width + j] <- pixels[(pixels_height*initial_width) + pixels_width]
    }
  }
  
  image_matrix <- matrix(temp_vector, target_height, target_width)
  return(image_matrix)
}


#### Matrix rotating function
rotate_row<-function(x) t(apply(x,1,rev))
rotate_col<-function(x) t(apply(x,2,rev))