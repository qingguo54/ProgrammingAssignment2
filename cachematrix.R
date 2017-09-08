## These twos functions will output the inverse of an input matrix.
## cached method applied for these functions to improve the performance when
## same matrix input

## This makeCacheMatrix class includes 4 main methods which are:
## $set: set x as input matrix
## $get: get the matrix x 
## $setInverse : set the i which is the inverse of matrix x
## $getInverse : return the inverse of x

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
		}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(	set = set, get = get, 
			setInverse = setInverse, 
			getInverse = getInverse)
}


## This function is to get the inverse of matrix x.
## if i is not null, will return i, if not, will use the solve function
## to get the inverse then call the setInverse funtion to store i

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
		}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
