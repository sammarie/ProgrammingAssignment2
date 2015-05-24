## The following two functions will cache the inverse of matrix
## to save time.

## The first function makeCachematrix is a function 
## that sets and gets the value of a matrix, then sets and gets
## the value of that matrix's inverse:


makeCacheMatrix <- function(x = matrix()) {
      invrs <- NULL
      set <- function (y) {
            x <<- y
            invrs <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) invrs <<- solve
      getinverse <- function() invrs
      list(set=set, 
           get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}


## The function cacheSolve returns a matrix's inverse if it
## has been solved prior, or otherwise computes the inverse.

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
              message("Retrieving matrix inverse.")
              return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinverse(invrs)
        invrs
}
