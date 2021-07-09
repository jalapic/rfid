
## Find first and last element of repeated elements

x <- c(3,3,3,3,1,1,1,1,16,1,1,1,16,16,16,16,16,3,3,3,3,9,9,9,9,1,1,1,16,16,1,3,9,9,9)


x1 <- sort(unique(c(length(x) - cumsum(rle(rev(x))$lengths) + 1, cumsum(rle(x)$lengths) )))
x1

x[x1]
