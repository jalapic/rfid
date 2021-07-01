####  Imagine we have the following numbers in a sequence

ctime <- c(1.1,1.3,1.6,2.1,2.5,2.7,1.0,1.1,1.4,1.8,1.9,2.1,2.3,2.31,2.5,2.7,1.1,1.4,1.6)
ms <- c(12,15,16,17,18,19,25,26,27,30,33,36,36,37,39,40,41,43,45)

ctime
plot(ctime, type='l')

# find the drops in ctime by row index:
which(lead(ctime)<ctime) + 1 

# which of these row indices refer to what ms time?
ms[which(lead(ctime)<ctime) + 1 ]
