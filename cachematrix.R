## These two functions help to store an inverse of a matrix which could be retrieved
## from the cache memory until there is a change in the input matrix. It saves costly computation

## The following function takes a matrix as an input and checks whether inverse of the same
##exists or not. If its an existing matrix then the inverse is fetched from cache otherwise 
##it is passed to the next function

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## This function creates an inverse of a matrix using solve function

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
