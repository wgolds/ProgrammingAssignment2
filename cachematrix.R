## The below two functions allow for caching of a solved inverse matrix
## for time-saving retrieval

## this function creates a list of functions that store a matrix and will cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
      solution <- NULL
      set <- function(y) {
            x <<- y
            solution <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) solution <<- solve
      getInverse <- function() solution
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## this function pairs with makeCacheMatrix to retrieve the inverse from cache
## if already calculated or compute the inverse if not 

cacheSolve <- function(x, ...) {
      solution <- x$getInverse()
      if(!is.null(solution)) {
                  message("getting cached data")
                  return(solution)
      }
      input <- x$get()
      solution <- solve(input,...)
      x$setInverse(solution)
      
      ## Return a matrix that is the inverse of 'x'
      solution
      
      
      
}
