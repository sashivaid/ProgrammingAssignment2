
## WmakeCacheMatrix will create a list containing function to
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
		set <- function(y) {
		     x <<- y
			 s <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) s <<- solve
		getinverse <- function () s
		list(set = set, get = get,
		    setinverse = setinverse,
			getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix. It first checks to see 
## if the inverse has already been calcuated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise it calculates the inverse
## and set the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        s <- x$getinverse()
		if(!is.null(s)) {
		        message("getting cached data")
				return(s)
		}
		data <- x$get()
		s <- solve(data,...)
		x$setinverse(s)
		s
		
}
