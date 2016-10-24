#this program is for Assignment 2 - Caching the Inverse of Matrix

#getters and Setters

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  #setting names to list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#calculating Inverse of a matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #using solve to calculate inverse 
  m <- solve(data, ...)
  x$setinverse(m)
  #return Inverse
  m
}
