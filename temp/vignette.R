RDPCPP(cbind(c(1:4),c(1:4)),0.5)

# pick a trip from driver Alexander
sample_trip <- as.matrix(subset(tele_data,{trip_id==tele_data$trip_id[1]})[,c("lat","long")])
RDPCPP(sample_trip,0.001)

library(ggplot2)

cutoff = 0.001

ggplot(data = data.frame(sample_trip), aes(x=lat,y=long)) + geom_point(size=0.2) +
  geom_point(data = data.frame(RDPCPP(sample_trip,cutoff)), aes(x=X1, y=X2), size = 3,col="blue") +
  geom_line(data = data.frame(RDPCPP(sample_trip,cutoff)), aes(x=X1, y=X2), size = 0.5,col="red")

