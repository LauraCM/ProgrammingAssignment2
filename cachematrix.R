##MakeCacheMatrix creates a matrix object (x) that can cache its inverse (Inv)
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
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix (x). If the inverse has already been 
##calculated, cacheSolve retrieves the inverse from the cache (Inv)
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
