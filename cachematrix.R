##We're building a couple of functions, first one of which creates a cache of matrix given by the user. Second function looks for presence of the matrix already in the cache. If it finds it then it is returned instead of calculation the inverse again for the given matrix.

## Cache of the matrix is created so as to look for it.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function takes the input and looks for its inverse in the cache collected from the above ##matrix.

cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting the cached data of the given matrix.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
