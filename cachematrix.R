# Matrix inversion is usually a costly computation.
# We write a pair of functions that cache the inverse of a matrix.
# We assume the matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
#   makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse
#   get the value of the inverse

  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
#   set the value of the matrix  
#   if (re)set CacheMatrix, the inverse data 'i' is emptied
  
  get <- function() x
#   get the value of the matrix
  
  setinverse <- function(inverse) i <<- inverse
#   set the value of the inverse
  
  getinverse <- function() i
#   get the value of the inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
#   creates the list

}


cacheSolve <- function(x, ...) {
#   cacheSolve takes 'x', a matrix list created by 'makeCacheMatrix' function  
#   it returns a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
#   it first checks to see if the inverse has already been calculated.
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
#   if the cache inverse exist, return and exit...

  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
#   ...if not, calculate the matrix inverse via 'solve' function and cache the inverse

  i
#   return the calculated inverse
}
