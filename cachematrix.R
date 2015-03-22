## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <<- NULL
	set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
	setInverseMatrix <- function(inv) inverseMatrix <<- inv;
    	getInverseMatrix <- function() return(inverseMatrix);
	list(set = set, 
		get = get, 
		setInverseMatrix = setInverseMatrix, 
		getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseMatrix <- x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}
