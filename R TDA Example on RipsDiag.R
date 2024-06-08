#From R TDA Documentation
## EXAMPLE 1: rips diagram for circles (euclidean distance)
X <- circleUnif(30)
maxscale <- 5
maxdimension <- 1
## note that the input X is a point cloud
DiagRips <- ripsDiag(
  X = X, maxdimension = maxdimension, maxscale = maxscale,
  library = "Dionysus", location = TRUE, printProgress = TRUE)
# plot
layout(matrix(c(1, 3, 2, 2), 2, 2))
plot(X, cex = 0.5, pch = 19)
title(main = "Data")
plot(DiagRips[["diagram"]])
title(main = "rips Diagram")
one <- which(
  DiagRips[["diagram"]][, 1] == 1 &
    DiagRips[["diagram"]][, 3] - DiagRips[["diagram"]][, 2] > 0.5)
plot(X, col = 2, main = "Representative loop of data points")
for (i in seq(along = one)) {
  for (j in seq_len(dim(DiagRips[["cycleLocation"]][[one[i]]])[1])) {
    lines(
      DiagRips[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1,
      col = i)
  }
}
  