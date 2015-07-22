## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Clear the variables. then return a list with the 4 functions, (set,get, setinv, getinv. )
makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(setVal) {
		x <<- setVal
		inv <<- NULL
	}
	get = function() x
	setinv = function (inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## Basically we just pull off of setinv if it's set.  and store the inverse. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()

 
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}