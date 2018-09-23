## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) x <<- y
  
  get <- function() x
  
  inverse <- function(x) inv <<- solve(x)
  
  getInverse <- function() inv
  
  list(set = set, get = get, inverse = inverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$inverse(data)
  inv
        ## Return a matrix that is the inverse of 'x'
}
