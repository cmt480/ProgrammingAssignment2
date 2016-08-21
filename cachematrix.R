## create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## sets the contents of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}	
	
	## gets the value of the matrix
	get <- function() x
	
	## sets the inverse of the matrix
	setinv <- function(solve) m <<- solve
	
	## gets the value of the inverse of the matrix
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv, 
		getinv = getinv)
}

## find the inverse of the matrix returned in makeCacheMatrix

cacheSolve <- function(x, ...) {
	## go into the list returned by the previous function 
	## and store the getinverse value in inv
	inv <- x$getinv()
	
	## if there's an object there, return it and get out
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	## otherwise go into the list for the get object
	data <- x$get()
		
	## compute the inverse of the special matrix returned 
	## by makeCacheMatrix
	inv <- solve(data)
	
	## assign that value to setinv
	x$setinv(inv)
	inv
}