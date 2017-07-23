makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL #Setting cache to null
#setting up the matrix
	  set <- function(y) {
    		x <<- y
  		inv <<- NULL
	  }
#getting the matrix
	  
  	get <- function() x
#inversing the matrix
  	setinv <- function(invers) inv <<- invers
#getting the inversed matrix
  	  	getinv <- function() inv
#returning all functions in a list  	
  	list(set = set, get = get, setinv = setinv, getinv = getinv)
  }

  
  ## This function computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix above. If the inverse has already
  ## been calculated (and the matrix has not changed), then the
  ## cachesolve will retrieve the inverse from the cache.

  cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      	inv <- x$getinv()
      	if(!is.null(inv)) {
        		message("getting cached data")
        		return(inv)
        	}
      data <- x$get()
      	inv <- solve(data)
      	x$setinv(inv)
    	inv
      }