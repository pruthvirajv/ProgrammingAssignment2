## The source code provides a functions to output the inverse of a matrix either from the cache or by calculations
## Sample Usage-Step 1 xMat <- makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
## Sample Usage-Step 2 xMat$get()
## Sample Usage-Step 3 cacheSolve(xMat)
## Sample Usage-Step 4 xMat$get() %*% xMat$getInv()

## Following function returns a list of functions that could be used for fetching the mean of a matrix

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


## Returns a inverse of matrix only if it is not there in the cache

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
