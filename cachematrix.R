## A pair of functions are provided to calculate the inverse of a
## square matrix, and store this result. The stored result will be recalled
## if an inverse of an identical matrix is needed. Use makeCacheMatrix first.

# makeCacheMatrix
# this function creates a special list object to contain references to functions 
# that provide the matrix to be inverted (get, called as x$get), the inversion
# (inv,called as x$getinv), and a function to set the inversion (setinv).
makeCacheMatrix <- function(x = matrix()) {
  
  #makes sure we have an empty variable
  i <- NULL
  ## Create a list to store values
  set <- function(y) {
    x <<- y    #Link to object
    i <<- Null #Holder for the inverse
  }
  get <- function() x
  setinvers <- function(solve) i <<- solve 
  getinvers <- function() i
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


## this function checks to see if the inverse of square matrix x has been calculated, 
## and if not, returns the inverse through the solve function. If the inverse
## has been calculated previously, it returned the cached value (inv).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Attempts to get the casched value if it exits return it
  i <- x$getinvers()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If it does not exits set calculate the value and save to I and return
  data <- x$get()
  i <- solve(data )
  x$setinvers(i)
  i  
}
