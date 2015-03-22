## This function makes a matrix and can cache its inverse.

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


## This method calculates the inverse of the matrix returned by 
## makeCacheMatrix.  It returns the inverse if it is already available in cache,
## if not, it uses 'solve' method to get the inverse.

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
