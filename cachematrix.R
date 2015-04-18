## This is programming assignment 2 of coursera course introduction to R

## This function, creates a special "vector", which is really a list of functions
## The functions are to 
##      set the value of the vector
##      get the value of the vector
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	## Set function
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## Get function
	get <- function() x
	## Set inverse function
	setinverse <- function(solve) m <<- solve
	## Get inverse function
	getinverse <- function() m
	
	## List of functions
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## Using the cached version if it exists
cacheSolve <- function(x=matrix(), ...) {
	## Look for a cached version of the matrix.
	m <- x$getinverse()
	if(!is.null(m)) {
		## Found a cached version, return it.
		message("getting cached data")
		return(m)
	}
	## Didn't find a cached version, create one.
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
