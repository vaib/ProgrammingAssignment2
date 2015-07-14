## Put comments here that give an overall description of what your
## functions do

## This function creates a cached Matrix
#      The matrix is cached and get/set methods
#      to retrieve matrix and its inverse are provided 

makeCacheMatrix <- function(x = matrix()) {
	## Place holder for inverse matrix
	inv <- NULL
	
	## 'set' function
	##	checks for changed matrix and resets the inverse
	## 	so that inverse is calculated afresh
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## 'get' function
	##	returns the matrix
	get <- function() x
	
	## 'setinverse' function
	##	sets inverse matrix to the place holder
	setinverse <- function(i) inv <<- i
	
	## 'getinverse' function
	##	returns the inverse matrxi
	getinverse <- function() inv
	
	## return list of functions
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
	
	## Changing matrix via set() will nullify cached inverse
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

