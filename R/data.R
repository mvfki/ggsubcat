#' @title data.frame of points with sub-categorical variables
#' @name subcatPoints
#' @description
#' A simulated dataset containing 900 points with grouped into 3 main categories
#' and each sub-divided into 3 sub-categories. The position (x, y) is designed
#' to have a far distances between the main categories, while sub-categories
#' are separated by closely placed.
#' @format
#' A data.frame with 900 rows and 4 variables:
#' \describe{
#' \item{x}{numeric, x-coordinate of the point}
#' \item{y}{numeric, y-coordinate of the point}
#' \item{main}{factor with 3 levels (A, B, C), main category}
#' \item{sub}{factor with 9 levels (a1, a2, a3, b1, b2, b3, c1, c2, c3), the
#' sub-categories.}
#' }
"subcatPoints"
