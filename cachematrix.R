## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # <<- assigns value in an enviroment different from the current. 
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
cacheSolve <- function(x, ...) {
  ## Will return the inverse matrix of 'x'
  inv<- x$getinv()
  
  # if the inverse has already been calculated, this will catch it
  if (!is.null(inv)){
    # will get inverse from the cache, no need to do calculation. 
    message("getting cached data")
    return(inv)
  }
  
  # Calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # The inverse is set in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}