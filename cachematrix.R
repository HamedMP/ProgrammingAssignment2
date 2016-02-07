## Put comments here that give an overall description of what your
## functions do

## this function get a matrix and return a type that can cache results 
## of inverse of it. It has set, get, setInverse and getInverse items
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  calculates the invesrre of the special matrix returned
## by the makeCacheMatrix function above. If the invesre has
## already been computed, it gets the result and skips the calculation
## if it hasn't it computes the inverse and sets teh value in the cache

cacheSolve <- function(x, ...) {
  ## If m is not null return the cache data
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise calculate the matrix inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ## And then store the results in the cache
  x$setinverse(m)
  
  ## Return the newly calculate inverse of the matrix
  m
}
