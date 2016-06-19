## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL					
    set <- function(y) {				
      x <<- y						
      im <<- NULL				
    }
    get <- function() x
    setinv <- function(inv) im <<- inverse
    getinv <- function() im
    list(set = set, get = get,			 
         setinv = setinv,
         getinv = getinv)
}
  

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinv(im)
  im
}

## Return a matrix that is the inverse of 'x'
