# STAT295 
# Revision 

1+1
a <- 1+1
a
1:100 
vec <- 1:100
vec
class(vec)
typeof(vec)
# 1::100 typo

# we can use R as a calculator 
1+5
3*5
100/5

# dice
1:6 
mydice <- 1:6
mydice

# case sensitive
aaa <- 555
AAA <- 666
ls()

mydice - 1
mydice * mydice

#inner multiplication
mydice %*% mydice
mydice %o% mydice


dim(mydice) <- c(2,3)
mydice
dim(mydice) <- c(1, 2, 3)
mydice

# matrix
m <- matrix(mydice, nrow=2)
m

m <- matrix(mydice, nrow=2, byrow=TRUE)
m

# array
myarray <- array(C(1:12), dim=c(2, 2, 3))
myarray

now <- Sys.time()
now
