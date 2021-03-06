# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Columnwise standard deviations.
#'
#' @param X a matrix.
#' @param norm_type normalization type, integer input, default 0.
#' @return columnwise standard deviations.
#' @examples
#'
#'colSds(matrix(1:4,nrow=2))
#' @export
colSds <- function(X, norm_type = 0L) {
    .Call('_PathMatch_colSds', PACKAGE = 'PathMatch', X, norm_type)
}

#' Rowwise standard deviations.
#'
#' @param X a matrix.
#' @param norm_type normalization type, integer input, default 0.
#' @return rowwise standard deviations.
#' @examples
#'
#'rowSds(matrix(1:4,nrow=2))
#' @export
rowSds <- function(X, norm_type = 0L) {
    .Call('_PathMatch_rowSds', PACKAGE = 'PathMatch', X, norm_type)
}

#' Distance from a point to a line linked by two other points C++ version.
#'
#' @param a a numeric vetor with length 2.
#' @param b a numeric vetor with length 2.
#' @param c a numeric vetor with length 2.
#' @return real-valued Eucleadian distance from point a to the line linking b and c.
#' @examples
#'
#'dist2dCPP(c(1,1),c(0,0),c(0,1))
#' @export
dist2dCPP <- function(a, b, c) {
    .Call('_PathMatch_dist2dCPP', PACKAGE = 'PathMatch', a, b, c)
}

#' Ramer–Douglas–Peucker algorithm C++ version.
#'
#' @param points point matrix with 2 columns (x and y coordinate like).
#' @param epsilon cut-off distance for the algorithm.
#' @return RDP points.
#' @examples
#'
#'RDPCPP(cbind(c(1:4),c(1:4)),0.5)
#'
#'# pick a trip from driver Alexander
#'sample_trip <- as.matrix(subset(tele_data,{trip_id==tele_data$trip_id[1]})[,c("lat","long")])
#'RDPCPP(sample_trip,0.001)
#'
#'
#' @export
RDPCPP <- function(points, epsilon) {
    .Call('_PathMatch_RDPCPP', PACKAGE = 'PathMatch', points, epsilon)
}

#' Total number of sign changes in a logical vector.
#'
#' @param signs logical vector indicating the signs.
#' @return number of changes of signs.
#' @examples
#'
#'sign_change(c(TRUE,FALSE,TRUE,TRUE))
#' @export
sign_change <- function(signs) {
    .Call('_PathMatch_sign_change', PACKAGE = 'PathMatch', signs)
}

#' Total number of number changes in a numeric vector.
#'
#' @param numbers numeric vector.
#' @return number of changes of numbers.
#' @examples
#'
#'number_change(c(1,1,1,2,2,3,3,5))
#' @export
number_change <- function(numbers) {
    .Call('_PathMatch_number_change', PACKAGE = 'PathMatch', numbers)
}

#' Fixing the bearing changes close to 360 or -360.
#'
#' @param bearing_change bearing change vector.
#' @return fixed bearing changes.
#' @examples
#'
#'bearing_change_fix_cpp(c(1,30,90,190,259,350))
#' @export
bearing_change_fix_cpp <- function(bearing_change) {
    .Call('_PathMatch_bearing_change_fix_cpp', PACKAGE = 'PathMatch', bearing_change)
}

#' trip match algorithm that calculate matching distances.
#'
#' @param M1 distance and heading matrix of simplified trip 1.
#' @param M2 distance and heading matrix of simplified trip 2.
#' @param dist_cut distance cut-off.
#' @param heading_cut heading cut-off.
#' @return distances of matching trip segments.
#' @examples
#'
#' @export
trip_match_cpp <- function(M1, M2, dist_cut = 0.0001, heading_cut = 0.005) {
    .Call('_PathMatch_trip_match_cpp', PACKAGE = 'PathMatch', M1, M2, dist_cut, heading_cut)
}

