## This function will cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
		## initialize the inverse matrix "inv" to NULL
		inv <- NULL
		## If matrix is created for first time or it is changed
		## then, set the new matrix to x and reset inverse to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		## get the value of matrix
        get <- function() x
		## find the inverse of the matrix using solve function
        setinv <- function(solve) inv <<- solve
		## get the inverse of matrix
        getinv <- function() inv
		## passes all the values to the makeCacheMatrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the cached value of inverse of the matrix
## which has been set to cache the inverse of matrix
cacheSolve <- function(x, ...) {
        ## Get the cached inverse of the x 
		inv <- x$getinv()
		## If the inverse is null, calculate the inverse of matrix
        if(is.null(inv)) {
				## Get the marix and 
				data <- x$get()
				## calculate the inverse of matrix
				inv <- solve(data,...)
				## cache the newly calculated inverse 
				x$setinv(inv)
		}
		## return the inverse
        inv
}
