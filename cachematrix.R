## Program comprised of a pair of functions that cache the inverse of a matrix.
## --> Written by: Tara Payne
## --> Initially written on: May 22, 2015


## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	## Set the value of the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Get the value of the matrix
	get <- function() x
	
	## Set the value of the inverted matrix
	setinvert <- function(solve) m <<- solve
	
	## Get the value of the inverted matrix
	getinvert <- function() m
	
	list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
    }


## Function to compute the inverse of the special "matrix" returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
	m <- x$getinvert()
	
	## If the inverse has already been calculated (and the matrix has not changed), then
	##    inverse will be retrieved from the cache.
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	data <- x$get()
	m <- solve(data, ...)
	x$setinvert(m)
	
	## Return a matrix that is the inverse of 'x'
	m
}