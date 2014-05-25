## The following two functions cache the results of
## a computation of an inverse of a matrix

## The makeCacheMatrix function creates an object that can be used to cache
## an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(solve) i <<- solve
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
	
}

## The cacheSolve function calculates the inverse of a special matrix
## returned by makeCacheMatrix function. If the inverse has already been calculated 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinv()
		if(!is.null(i)) {
			message("retreiving cached value")
			return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinv(i)
		i
}
