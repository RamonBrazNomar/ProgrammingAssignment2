## These functions exemlify a way to cache a result from a potentially
## costly operation. In this case, it's the inverse of a matrix
## calculation.


## MakeCacheMatrix() is the function that creates all the conditions
## for caching, keeping proper copies of the variables 
makeCacheMatrix <- function(x = matrix()){
  
  inv <- NULL  ##deletes any results for inv in the present environment
  
  set <- function(y){
    x <<- y  ##cache x
    inv <<- NULL  ##deletes any cached result
  }
  get <- function() x  ##simply returns x
  
  setinv <- function(inverse) inv <<- inverse ##cache inverse
  getinv <- function () inv ##simply returns inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  ## they're all functions.
}

## cacheSolve() makes use of the above function to execute the verification
## of cached data.
cacheSolve <- function(x, ...){
  
  inv <- x$getinv()  ##use stored function from makeCacheMatrix's environment
  
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)  ##returns inv and exits function
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
