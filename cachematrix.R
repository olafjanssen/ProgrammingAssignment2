## Put comments here that give an overall description of what your
## functions do

## Creates a cache for a matrix and its inverse containing convenience methods for getting and setting its inverse
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
	    set <- function(y) {
	            x <<- y
	            m <<- NULL
	    }
	    get <- function() x
	    setInverse <- function(inverse) m <<- inverse
	    getInverse <- function() m
	    list(set = set, get = get,
	         setInverse = setInverse,
	         getInverse = getInverse)

}


## Computes the inverse of a matrix using the object created with makeCacheMatrix. It returns a cached value if this
## function is called more than once.
cacheSolve <- function(x, ...) {
	    ## Return a matrix that is the inverse of 'x' from cache if available
	    m <- x$getInverse()
	    if(!is.null(m)) {
	            message("getting cached data")
	            return(m)
	    }
	    ## Compute the inverse and store the result in the cache
	    data <- x$get()
	    m <- solve(data, ...)
	    x$setInverse(m)
	    m
}
