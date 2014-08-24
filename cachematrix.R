## Functions that compute the inverse of a matrix through caching.
## These don't use nested loops.

## List of functions that create the special matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Creating an initial value   
  iv <- NULL
  
  ## setting the matrix  
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  
  ## getting the matrix
  get <- function() x
  
  ## setting the inverse
  setinverse <- function(inverse) iv <<- inverse
  
  ## getting the inverse
  getinverse <- function() iv
  
  ## Returning the list of functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special matrix
## created by the function above.

cacheSolve <- function(x, ...) {
  
  iv <- x$getinverse()
  ## Checking if the inverse has been calculate. 
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }

  ##else
  ##getting the matrix 
  data <- x$get()

  ## Assigning the inverse. 
  iv <- solve(data, ...)

  ## setting the inverse
  x$setinverse(iv)

  ## Returning the inverse of x
  iv

}