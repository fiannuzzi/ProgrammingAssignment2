## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function, once given a matrix in input, creates a list of four subfunctions.
## These subfunctions return the matrix itself, its inverse (if available) 
## or can be used to set the matrix value and its inverse.

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
## The function takes a matrix of the form returned by the above function in input 
## and returns its inverse. 
## The inverse is either read in from the matrix object via the get_inverse() subfunction
## or evaluated from scratch. 
## In the latter case, the result is stored in the matrix object via the set_inverse() 
## subfunction.

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
