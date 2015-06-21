## Put comments here that give an overall description of what your
## functions do

## This function creates a cached Matrix
#      The matrix is cached and get/set methods
#      to retrieve matrix and its inverse are provided 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	# Changing the matrix resets the inverse
	# so that inverse is calculated afresh
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(i) inv <<- i
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function returns inverse of a matrix
#      The inverse of matrix if cached is returned
#      else inverse is computed, cached and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

