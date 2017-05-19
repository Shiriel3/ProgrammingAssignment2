## These two functions essentially wrap a matrix and the
## solve function in order to provide a cached version
## of the common operation of calculating inverses of
## matrices

## Takes a matrix as input and returns a list containing
## function operations to perform on the matrix (get, set,
## getinverse, setinverse)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	get <- function() x
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	setinverse <- function(j) i <<- j
	getinverse <- function() i
	
	list(
		get = get,
		set = set,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## Returns the cached inverse of the matrix within the
## object created by makeCacheMatrix, if solved previously,
## and if not, calculates the inverse, caches the result
## and returns the inverse

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)

	i
}
