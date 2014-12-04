## two functions that implement the programming asssignment 2 of r-programming course

## this function creates an object that contains the original matrix, and the inverse of it if it has been calculated
## it also contains 4 methods that allow manipulating these values: 
##		set/get for the matrix value 
##		and setinverse,getinverse for the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## this function return the matrix inverse
## x must have been created with makeCacheMatrix function from a matrix() object
## if the inverse has already been calculated, it returns the cached value stored inside the object x
## otherwise, it computes and stores the matrix inverse

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if( !is.null(inv) )
	{
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
