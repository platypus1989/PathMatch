#' Log loss score for competition.
#'
#' @param solution .
#' @param submission .
#' @param adj .
#' @return multinomial log loss score.
#' @examples
#'
#' @export
LogLoss <- function(solution, submission, adj = 1e-15) {
  if (!setequal(names(solution),names(submission))){
    stop("driver names do not match")
  }
  if (!setequal(solution$trip_id,submission$trip_id)){
    stop("trip ids do not match")
  }
  solution <- solution[order(solution$trip_id),]
  submission <- submission[order(submission$trip_id),]
  X <- solution[,setdiff(names(solution),"trip_id")]
  Y <- submission[,names(X)] # rearrage submission column in the same order
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  Y <- Y/rowSums(Y) # transform the submission values to into probabilities
  Y_adj = pmin(pmax(Y, adj), 1-adj)
  return( - sum(X*log(Y_adj) )/ nrow(solution) )
}

#' Individual scores at each record.
#'
#' @param solution .
#' @param submission .
#' @param adj .
#' @return multinomial log loss score.
#' @examples
#'
#' @export
LogLoss_Analysis <- function(solution, submission, adj = 1e-15) {
  if (!setequal(names(solution),names(submission))){
    stop("driver names do not match")
  }
  if (!setequal(solution$trip_id,submission$trip_id)){
    stop("trip ids do not match")
  }
  solution <- solution[order(solution$trip_id),]
  submission <- submission[order(submission$trip_id),]
  X <- solution[,setdiff(names(solution),"trip_id")]
  Y <- submission[,names(X)] # rearrage submission column in the same order
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  Y <- Y/rowSums(Y) # transform the submission values to into probabilities
  Y_adj = pmin(pmax(Y, adj), 1-adj)
  score <- data.frame(trip_id=solution$trip_id,score= - rowSums(X*log(Y_adj) ))
  return( score )
}

#' Accuracy rate.
#'
#' @param solution .
#' @param submission .
#' @return accuracy rate.
#' @examples
#'
#' @export
accuracy_rate <- function(solution, submission){
  sum(diag(table(apply(solution[,-1],1,which.max),apply(submission[,-1],1,which.max))))/nrow(solution)
}

#' Convert probablities to votes.
#'
#' @param submission .
#' @return votes of submission.
#' @examples
#'
#' @export
prob_to_vote <- function(submission){
  vote_submission <- submission
  vote_submission[,-1] <- 0
  vote_submission[cbind((1:nrow(submission)),apply(submission[,-1],1,which.max)+1)] <- 1
  return(vote_submission)
}

#' Convert probablities to rank.
#'
#' @param submission .
#' @return rank of submission.
#' @examples
#'
#' @export
prob_to_rank <- function(submission){
  rank_submission <- data.frame(trip_id=submission[,1],t(apply(submission[,-1],1,function(V) 36-rank(V))))
  return(rank_submission)
}

#' Distance from a point to a line linked by two other points.
#'
#' @param a .
#' @param b .
#' @param c .
#' @return read-valued distance.
#' @examples
#'
#' @export
dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  if (sum(abs(v1))==0){
    return(sqrt(sum((v2)^2)))
  } else{
    d <- abs(v1[1]*v2[2]-v1[2]*v2[1])/sqrt(sum(v1^2))
    return(d)
  }
}

#' Ramer–Douglas–Peucker algorithm.
#'
#' @param points .
#' @param epsilon .
#' @return RDP points.
#' @examples
#'
#' @export
RDP <- function(points,epsilon){
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
    result1 <- RDP(points[1:index,],epsilon)
    result2 <- RDP(points[index:n,],epsilon)
    results <- rbind(result1[-nrow(result1),],result2)
  } else {
    results <- rbind(points[1,],points[n,])
  }
  colnames(results) <- NULL
  return(results)
}


#' Generate polypon points for Ramer–Douglas–Peucker algorithm visualization.
#'
#' @param pt1 .
#' @param pt2 .
#' @param dist .
#' @return a matrix with dimension 4*2 denoting the 4 vertices of the polygon.
#' @examples
#' library(ggplot2)
#' point_set <- rbind(c(0,0),c(2,2))
#' GeneratePolygon(point_set[1,],point_set[2,])
#' df <- data.frame(t(GeneratePolygon(point_set[1,],point_set[2,],dist=0.2)))
#' ggplot(df,aes(X1,X2)) + geom_polygon(fill='lightblue',alpha=0.5)

#' @export
GeneratePolygon <- function(pt1,pt2,dist = 1){
  
  V <- (pt2 + pt1)/2
  theta <- atan((pt2[2]-pt1[2])/(pt2[1]-pt1[1]))
  cos_theta <- cos(theta)
  sin_theta <- sin(theta)
  V_len <- sqrt(sum((pt2-pt1)^2))
  Poly_points <- matrix(c(-V_len/2,-V_len/2,V_len/2,V_len/2,-dist,dist,dist,-dist),nrow=2,byrow=TRUE)
  
  # step 1
  transform_M <- matrix(c(cos_theta, -sin_theta, sin_theta, cos_theta),nrow=2,byrow=TRUE)
  Poly_points <- transform_M %*% Poly_points
  
  # step 2
  Poly_points <- Poly_points + matrix(V,nrow=2,ncol=4,byrow=FALSE) 
  
  return(Poly_points)
}

#' trip matching algorithm R version 1.
#'
#' @param M1 .
#' @param M2 .
#' @param dist_cut .
#' @param heading_cut .
#' @return logical vector indicating matched index.
#' @examples
#'
#' @export
trip_match_v1 <- function(M1,M2,dist_cut=1e-5,heading_cut=1e-5){
  match_index <- rep(FALSE,nrow(M1))
  nrow_M2 <- nrow(M2)
  for (i in 1:nrow(M1)){
    diff_dist <- abs(M2[,1] - M1[i,1])
    diff_heading <- abs(M2[,2] - M1[i,2]) # note that this heading is in radian rather than degree
    j <- 1
    while ((diff_dist[j]>dist_cut | diff_heading[j]>heading_cut) & j <= nrow_M2) {
      j <- j+1
    }
    if (j<=nrow_M2){
      match_index[i] <- TRUE
    }
  }
  return(match_index)
}

#' trip matching algorithm R version 2.
#'
#' @param M1 .
#' @param M2 .
#' @param dist_cut .
#' @param heading_cut .
#' @return logical vector indicating matched index.
#' @examples
#'
#' @export
trip_match_v2 <- function(M1,M2,dist_cut=1e-5,heading_cut=1e-5){
  match_index <- rep(FALSE,nrow(M1))
  nrow_M2 <- nrow(M2)
  for (i in 1:nrow(M1)){
    j <- 1
    diff_dist <- abs(M2[j,1] - M1[i,1])
    diff_heading <- abs(M2[j,2] - M1[i,2])

    while (diff_dist>dist_cut | diff_heading>heading_cut) {
      j <- j+1
      if (j > nrow_M2) break
      diff_dist <- abs(M2[j,1] - M1[i,1])
      diff_heading <- abs(M2[j,2] - M1[i,2])
    }
    if (j<=nrow_M2){
      match_index[i] <- TRUE
    }
  }
  return(match_index)
}

#' trip matching algorithm R version 3.
#'
#' @param M1 .
#' @param M2 .
#' @param dist_cut .
#' @param heading_cut .
#' @param match_n_cut .
#' @return logical vector indicating matched index.
#' @examples
#'
#' @export
trip_match_v3 <- function(M1,M2,dist_cut=0.001,heading_cut=0.01,match_n_cut=5){
  match_found <- FALSE
  nrow_M1 <- nrow(M1)
  nrow_M2 <- nrow(M2)
  if (nrow_M1 < match_n_cut | nrow_M2 < match_n_cut){
    return(match_found)
  }
  for (i in 1:(nrow_M1-match_n_cut+1)){
    for (j in 1:(nrow_M2-match_n_cut+1)){
      trip_diff <- abs(M1[i:(i+match_n_cut-1),] - M2[j:(j+match_n_cut-1),])
      if (max(trip_diff[,1]) < dist_cut & max(trip_diff[,2]) < heading_cut){
        match_found <- TRUE
      }
    }
  }
  return(match_found)
}


#' brake stat calculation R version.
#'
#' @param speed .
#' @param lag .
#' @return speed before brake.
#' @examples
#'
#' @export
brake_calc <- function(speed,lag=1){
  speed <- speed[!is.na(speed)]
  n <- length(speed)
  index1 <- 1
  brake_stat <- vector("numeric",n)
  index2 <- 1
  while (index1 < n){
    if (speed[index1]==0 & index1 > lag){
      brake_stat[index2] <- speed[index1-lag]
      index2 <- index2+1
      while (speed[index1]==0 & index1 < n-1){
        index1 <- index1+1
      }
    }
    index1 <- index1+1
  }
  return(brake_stat[brake_stat>0])
}

#' start stat calculation R version.
#'
#' @param speed .
#' @param lag .
#' @return speed after start.
#' @examples
#'
#' @export
start_calc <- function(speed,lag=1){
  speed <- speed[!is.na(speed)]
  n <- length(speed)
  index1 <- 1
  start_stat <- vector("numeric",n)
  index2 <- 1
  while (index1 < n){
    if (speed[index1]==0 ){
      while (speed[index1]==0){
        index1 <- index1 + 1
        if (index1>n) break
      }
      if (index1+lag <= n){
        start_stat[index2] <- speed[index1+lag]
        index2 <- index2+1
      }
    }
    index1 <- index1+1
  }
  return(start_stat[start_stat>0])
}

