#There are 2 functions:
#	- makeCacheMatrix
#	- cacheSolve

#makeCacheMatrix retrieves a list of 4 objects:
#	- set a new matrix
#	- get the matrix which is stored when calling this function
#	- set the inverse of this matrix (just set it, it does not calulate it)
#	- get the inverse of the matrix if it was already calculated

#cacheSolve checks whether the inverse of a matrix is stored in makeCacheMatrix
#and calculates this inverse when it is not stored in makeCacheMatrix




#makeCacheMatrix can set, reset, retrieve a matrix, stores and retrieve its inverse.
makeCacheMatrix <- function(xmatrix = matrix()) {
	inversedmatrix <- NULL
	setmatrix <- function(newmatrix) {
		xmatrix <<-  newmatrix
		inversedmatrix <<- NULL
	}
	getmatrix <- function() xmatrix
	setinverse <- function(inverse) inversedmatrix <<- inverse
	getinverse <- function() inversedmatrix
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse , getinverse = getinverse)
}

#cacheSolve is used to calculate or retrieve the inverse of a matrix
cacheSolve <- function(xmatrix, ...) {
	inversedmatrix <- xmatrix$getinverse()
	if(!is.null(inversedmatrix)) {
		message("getting cached data")
		return(inversedmatrix)
	}
	data <- xmatrix$getmatrix()
	inversedmatrix <- solve(data, ...)
	xmatrix$setinverse(inversedmatrix)
	inversedmatrix
}
