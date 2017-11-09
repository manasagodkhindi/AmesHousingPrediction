library(dplyr)
library(ggplot2)
library(leaflet)

AmesData <- read.csv('/Users/michaelzolla/Desktop/Kaggle/MG_Ames_train_imputed.csv')

coordinates <- data.frame(Neighborhood = levels(AmesData$Neighborhood),
                          lat = c(42.062806, 42.009408, 42.052500, 42.033590, 42.025425,
                                  42.021051, 42.025949, 42.022800, 42.027885, 42.019208, 
                                  41.991866, 42.031307, 42.042966, 42.050307, 42.050207,
                                  42.060356, 42.051321, 42.028863, 42.033611, 42.035540, 
                                  42.052191, 42.060752, 42.017578, 41.998132, 42.040106),
                          
                          lng = c(-93.639963, -93.645543, -93.628821, -93.627552, -93.675741, 
                                  -93.685643, -93.620215, -93.663040, -93.615692, -93.623401,
                                  -93.602441, -93.626967, -93.613556, -93.656045, -93.625827, 
                                  -93.657107, -93.633798, -93.615497, -93.669348, -93.685131,
                                  -93.643479, -93.628955, -93.651283, -93.648335, -93.657032))





nbh.price <- summarise(group_by(AmesData, Neighborhood),
                       sales = n(),
                       mean.price = mean(SalePrice))

coordinates <- merge(x = coordinates,
                     y = nbh.price, 
                     by = "Neighborhood",
                     all.x = T)

pal <- colorNumeric(palette = "Reds",
                    domain = coordinates$nbh.price)

Ames <- leaflet(coordinates) %>% 
  addTiles() %>% 
  addCircles(lat = ~lat,
             lng = ~lng, weight = 10,
             radius = ~sales*8,
             color = ~pal(coordinates$mean.price)) %>%
  addMarkers(popup = ~paste(Neighborhood,", Mean:",
                            round(mean.price),sep = ""))
Ames