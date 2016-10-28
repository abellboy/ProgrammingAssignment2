## These two functions will calculate the inverse matrix or 
## retrieve the inverse matrix from the cache.


## creates a Matrix object that can cache its inverse. 
## It contains 4 functions: set, get, setinverse, getinverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## Cachesolve checks to see if the invese matrix has already been saved.
## If it has, it uses that one, if not itcomputes the inverse of the matrix 
## and saves it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}



