## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  matInv <- NULL
  setMat <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  getMat <- function() x
  setInv <- function(inv) matInv <<- inv
  getInv <- function() matInv
  list(set = setMat, get = getMat,
       setInv = setInv,
       getInv = getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInv()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data, ...)
  x$setInv(matInv)
  matInv
}
