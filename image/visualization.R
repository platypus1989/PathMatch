pkg <- c("PathMatch","ggplot2","animation","dplyr")
sapply(pkg,require,character.only=TRUE)

sample_trip <- subset(tele_data,{trip_id==tele_data$trip_id[1]})[,c("lat","long")]

order_point <- 0
RDP_pointset <- matrix(0,nrow=0,ncol=3)

RDP2 <- function(points,epsilon){
  
  dmax  <- 0
  index <- 0
  n     <- nrow(points)
  for (i in 2:(n-1)){
    d <- dist2d(points[i,],points[1,],points[n,])
    if (d > dmax){
      index <- i
      dmax  <- d
    }
  }
  
  if (dmax > epsilon){
    order_point <<- order_point + 1
    RDP_pointset <<- rbind(RDP_pointset,cbind(points[c(1,index,n),],order_point))
    
    result1 <- RDP2(points[1:index,],epsilon)
    result2 <- RDP2(points[index:n,],epsilon)
    results <- rbind(result1[-nrow(result1),],result2)
    
  } else {
    results <- rbind(points[1,],points[n,])
  }
  colnames(results) <- NULL
  return(results)
}

dist = 0.005

RDP2(as.matrix(sample_trip),epsilon = dist)


saveGIF({
  
  RDP_point <- matrix(0, nrow=0, ncol=2)
  for (i in 1:order_point){
    point_set <- RDP_pointset[RDP_pointset[,3] == i,][c(1,3),c(1,2)] 
    
    p <- ggplot(sample_trip, aes(lat,long)) + geom_point(size=0.2) # + coord_fixed()
    
    df <- data.frame(t(GeneratePolygon(point_set[1,],point_set[2,],dist=dist)))
    
    
    if (nrow(RDP_point)!=0){
      print(
        p + geom_polygon(data=df, aes(x=X1,y=X2),fill='lightblue',alpha=0.5) + 
          geom_point(data = data.frame(matrix(RDP_point,ncol=2)), aes(x=X1,y=X2),col='red',size=7)  
      )
    } else {
      print(
        p + geom_polygon(data=df, aes(x=X1,y=X2),fill='lightblue',alpha=0.5)  
      )
      
    }
    
    
    RDP_point <- rbind(RDP_point, RDP_pointset[RDP_pointset[,3] == i,][2,c(1,2)])
    
    print(
      p + geom_polygon(data=df, aes(x=X1,y=X2),fill='lightblue',alpha=0.5) + 
        geom_point(data = data.frame(matrix(RDP_point,ncol=2)), aes(x=X1,y=X2),col='red',size=7)
    )
    
  }
  
  for (j in 1:10){
    new_df <- sample_trip[c(1,nrow(sample_trip)),]
    print(
      p + geom_point(data = data.frame(matrix(RDP_point,ncol=2)), aes(x=X1,y=X2),col='red',size=7) + 
        geom_point(data = new_df, aes(x=lat,y=long),col='blue',size=7)
    )
  }
  
}, interval = 1, movie.name = "image/RDP_process.gif", ani.width = 1800, ani.height = 600)



saveGIF({
  
  RDP_point <- matrix(0, nrow=0, ncol=2)
  point_set <- RDP_pointset[RDP_pointset[,3] == i,][c(1,3),c(1,2)] 
  new_df <- sample_trip[c(1,nrow(sample_trip)),]
  p <- ggplot(sample_trip, aes(lat,long)) + geom_point(size=0.2) +
    geom_point(data = new_df, aes(x=lat,y=long),col='blue',size=7)
  print(p)
  
  df <- data.frame(t(GeneratePolygon(point_set[1,],point_set[2,],dist=dist)))
  print(p + geom_polygon(data=df, aes(x=X1,y=X2),fill='lightblue',alpha=0.5))  
  
}, interval = 1, movie.name = "image/RDP_polygon_example.gif", ani.width = 1800, ani.height = 600)



saveGIF({
  
  pt1 <- c(1,1)
  pt2 <- c(2,3)
  df <- data.frame(rbind(pt1,pt2))
  print(ggplot(df,aes(x=X1, y=X2)) + geom_point(col="blue",size=2) + geom_line()+ xlim(-1.5,3) + ylim(-1.5,3)) 
  
  freq <- 10
  
  V <- (pt1 + pt2)/2
  for (i in 1:freq){
    df1 <- data.frame(rbind(pt1-V*i/freq,pt2-V*i/freq))
    print(ggplot(df1,aes(x=X1, y=X2)) + geom_point(col="blue",size=2) + geom_line()+ xlim(-1.5,3) + ylim(-1.5,3))  
  }
  
  theta <-  atan((pt2[2]-pt1[2])/(pt2[1]-pt1[1]))
  
  for (i in 1:freq){
    cos_theta <- cos(theta*i/freq)
    sin_theta <- sin(theta*i/freq)
    
    df2 <- data.frame(as.matrix(df1)%*%matrix(c(cos_theta,-sin_theta,sin_theta,cos_theta),2,byrow=TRUE))
    print(ggplot(df2,aes(x=X1, y=X2)) + geom_point(col="blue",size=2) + geom_line()+ xlim(-1.5,3) + ylim(-1.5,3))  
  }
  # 
  # poly_df <- 
  # 
  # for (i in 1:freq){
  #   cos_theta <- cos(-theta*i/freq)
  #   sin_theta <- sin(-theta*i/freq)
  #   transform_M <- matrix(c(cos_theta,-sin_theta,sin_theta,cos_theta),2,byrow=TRUE)
  #   df3 <- data.frame(as.matrix(df2)%*%transform_M)
  #   print(ggplot(df3,aes(x=X1, y=X2)) + geom_point(col="blue",size=2) + geom_line()+ xlim(-3,3) + ylim(-3,3))  
  # }
  # 
  
  
}, interval = 0.2, movie.name = "image/RDP_polygon_generation.gif", ani.width = 600, ani.height = 600)


