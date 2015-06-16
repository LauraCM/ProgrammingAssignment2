makeCacheMatrix <- function(x = matrix()) {
     Inv <- NULL
    set <- function(y) {
       x <<- y
       Inv <<- NULL 
	 }
    
    get <- function() x
    setinverse <- function(solve) Inv <<- solve
    getinverse <- function() Inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		Inv <- x$getinverse()
		if(!is.null(Inv)) {
		     message("getting cached data")
			 return(Inv)
			 }
			 data <- x$get()
			 Inv <- solve(data, ...)
			 x$setinverse(Inv)
			 Inv
}
