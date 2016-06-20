## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function that creates a matrix and that contains functions that set the values of the matrix, get the values of the matrix, set the inverse of the matrix and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
    setInv = setInv,
    getInv = getInv)

}


## Write a short comment describing this function
## This function retireves the inverse of the matrix created by the function above if the inverse has already been calculated.
## Otherwise it calculates the inverse using solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
