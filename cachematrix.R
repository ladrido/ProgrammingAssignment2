
## the following functions are used to create an object that stores a matrix and caches its inverse

## this function creates a vector that contains functions to set the value of a created matrix, get its value, set its inverse and get its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inversa) inv <<- inversa
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function calculates the inverse of a matrix, it first checks if the inverse has been calculated, if so, the inverse is cached and returned, otherwise, the inverse is calculated and saved with the setinv function 
cacheSolve <- function(x, ...) {
  ## return a matrix that is the inverse of 'x'
	inversa <- x$getinv()
	if(!is.null(inversa)){
		message("getting cached inverse")
		return(inversa)
	}
	data <- x$get()
	inversa <- solve(data, ...)
	x$setinv(inversa)
	inversa
}
