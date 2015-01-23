## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## Default
  inverse <- NULL
  
  ## Set function: "loads" a matrix, clears inverse value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Get function: retrieves the matrix
  get <- function() x
  
  ## Set_inverse function: "loads" the inverse of the matrix
  set_inverse <- function(inverse_matrix) inverse <<-- inverse_matrix
  
  ## Get_inverse function: retrieves the inverse of the matrix
  get_inverse <- function() inverse
  
  ## Create a list of functions
  list( set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
 ## Get the current stored value of the inverse matrix
  inverse <- x$get_inverse()
  
  ## If this is not null, then the inverse value is cached already
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  ## Otherwise, no inverse value has been computed yet
  ## Get the matrix
  m <- x$get()
  ## Compute its inverse
  inverse <- solve(m)
  ## Store the inverse in the matrix object
  x$set_inverse(inverse)
  ## Return the inverse value (no message: the inverse has been computed for the first time)
  inverse
  
}
