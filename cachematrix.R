## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## inv will store the catched inverse matrix
  inv <- NULL
  
  ## set for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get function for matrix
  get <- function() x
  
  ## set for inverse
  setinv <- function(inverse) inv <<- inverse
  
  ## get for inverse
  getinv <- function() inv
  
  ## Return the matrix with the new defined function we created before
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function


## It will compute the inverse of the matrix and return the cached inverse, if the inverse is already calculated before
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)){
    message("getting the cached data")
    return(inv)
  }
  
  # calculate the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  ## Return it
  inv
}
