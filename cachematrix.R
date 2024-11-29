# creates a special object that stores a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # function to set the matrix
  set <- function(y) {
    x <<- y     
    inv <<- NULL
  }
  
  # function to get the matrix
  get <- function(){
    x
  }
  
  setInverse <- function(inverse){
    inv <<- inverse
  }

  getInverse <- function() inv
  
  # returns list of functions to interact with the object
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# if inverse is cached already, it gets cached value
cacheSolve <- function(x, ...) {
  # check if inverse is cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  
  # matrix invert
  mat <- x$get()
  
  # inverse
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

