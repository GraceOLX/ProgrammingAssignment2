makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inversedata) m <<- inversedata
	getinverse <- function() m

}


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrixdata <- x$get()
	m <- solve(matrixdata, ...)
	x$setinverse(m)
	m
}
