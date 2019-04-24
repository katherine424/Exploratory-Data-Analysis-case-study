set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:40, 1:10, dataMatrix)
heatmap(dataMatrix)
set.seed(678910)
for (i in 1:40) {
  # flip a coin
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
  }
}
image(1:40, 1:10, dataMatrix)
heatmap(dataMatrix)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(2, 3))
image(t(dataMatrixOrdered))
plot(rowMeans(dataMatrixOrdered), 1:40, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered),1:10, xlab = "Column", ylab = "Column Mean", pch = 19)
svd1 <- svd(scale(dataMatrixOrdered))
image(t(dataMatrixOrdered))
plot(svd1$u[, 4], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[, 4], 1:10, xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained",
     pch = 19)

pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)

constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)

