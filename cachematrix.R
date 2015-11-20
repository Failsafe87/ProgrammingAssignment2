## These two functions work together by using lexical scoping and free variables. One stores a matrix
## and sets invmat to NULL. The second function inverts the matrix unless that variable has already been
## defined within the second function, meaning it has previously been inverted, in which case the
## cached version that is stored is returned.

## This function stores a matrix and sets a flag to NULL. It also has other fucntions that store the
##and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	invmat <- NULL
	set <- function(y){
		x <<-y
		invmat <<- NULL

	}

	get <- function() x
	setinverse <- function(inverse) invmat <<- inverse
	getinverse <- function() invmat
	list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)

}


## This function checks the inverse to see if it has been flaged as NULL. If it has, it returns that
## variable and displays a message. Otherwise, it will store the data of the matrix in to other variables
## and take the inverse of the matrix. This also gets stored in the first function's function setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinverse()

		if(!is.null(invmat)){
			message("getting cached data!")
			return(invmat)

		}

	data <- x$get()
	invmat <- solve(data, ...)
	x$setinverse(invmat)
	return(invmat)

}
