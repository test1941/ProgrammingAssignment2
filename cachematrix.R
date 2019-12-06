## The makeCacheMatrix function returns four functions. When we pass
## a matrix to the function, it is hold in memory and can be
## retrieved by get function and inverse is firstly set to null. 
## Then, in the cacheSolve function, inverse is calculated. When a
## new matrix is passed to set function, the output is passed to
## cachesolve function and after first calculation, inverse is cached,
## which can be retrieved by getinverse function. Then finally, if
## same function as before is passed to cacheSolve function, then
## inverse is not calculated but retrieved from cache, with a message
## indicating if it is retrieved from cache.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
