# Lecture 2 

# Exceptions and Timings 
vec1 <- c(1,2,3)
vec2 <- c(1,2)
vec1*vec2
#warning message you still get output

warn_test <- function(x){
  if(x<=0){
    warning("watch out! 'x' is less than or equal to 0. Set it 1")
    x <- 1
  }
  return(2/x)
}
warn_test(5)
warn_test(0)

error_test <- function(x){
  if(x<= 0){
    stop("'x' is less than or eqaul to 0... TERMINATE!")
  }
  return(2/x)
} 
error_test(0)
error_test(2)

myfibrec <- function(n){
  if(n<0){
    warning("Assuming you meant 'n' to be positive -- doing that instead")
    n <- n*(-1)
  }else if(n == 0){
    stop("'n' is uninterpretable at 0")
  }
  if(n==1 || n==2){
    return(1)
  }else{
    return(myfibrec(n-1)+myfibrec(n-2))
  }
}
myfibrec(10)
#ginv and solve
#try function
try(myfibrec(0),silent=TRUE)
attempt1 <- try(myfibrec(0), silent=T)
attempt1
class(attempt1)

attempt2 <- try(myfibrec(6),silent = T)
attempt2
  
  

myfibrecvector <- function(nvec){
  nterms <- length(nvec)
  result <- rep(0, nterms)
  for (i in 1:nterms){
    result[i] <- myfibrec(nvec[i])
  }
return(result)
}
foo <- myfibrecvector(nvec=c(1,2,10,8))
foo
bar <- myfibrecvector(nvec=c(3,2,7,0,9,13))

myfibrecvectorTRY <- function(nvec){
  nterms <- length(nvec)
  result <- rep(0, nterms)
  for (i in 1:nterms){
    result[i] <- myfibrec(nvec[i])
  }
  return(result)
}
 

myfibrecvectorTRY <- function(nvec){
  nterms <- length(nvec)
  result <- rep(0, nterms)
  for (i in 1:nterms){
    attempt <- try(myfibrec(nvec[i]), silent=TRUE)
    if(class(attempt) == "try-error"){
      result[i] <- NA
    } else{
      result[i] <- attempt
    }
  }
  return(result)
}

baz <- myfibrecvectorTRY(nvec=c(3,2,7,0,9,13))
baz

myfibrec(-3)
attempt3 <- suppressWarnings(myfibrec(-3))
attempt3


# Progress and Timing 
# Textual Progress Bars

Sys.sleep(10)
sleep_test <- function(n){
  result <- 0 
  for (i in 1:n){
    result <- result + 1
    Sys.sleep(0.5)
  }
  return(result)
}
sleep_test(8)

prog_test <- function(n){
  result <- 0 
  progbar <- txtProgressBar(min=0, max=n, style=1, char="=")
  for(i in 1:n){
    result <- result + 1
    Sys.sleep(0.5)
    setTxtProgressBar(progbar, value=i)
  }
  close(progbar)
  return(result)
}
prog_test(15)


prog_test <- function(n){
  result <- 0 
  progbar <- txtProgressBar(min=0, max=n, style=3, char="=")
  for(i in 1:n){
    result <- result + 1
    Sys.sleep(0.5)
    setTxtProgressBar(progbar, value=i)
  }
  close(progbar)
  return(result)
}
prog_test(8)


# Measuring Completion Time 
Sys.time()

t1 <- Sys.time()
Sys.sleep(3)
t2 <- Sys.time()
t2 - t1
# you have to run them all together 
