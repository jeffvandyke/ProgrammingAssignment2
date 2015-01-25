## These two functions work with a special type of matrix that can cache
## its inverse so that it doesn't need to recompute it everytime it's needed.

## the object returned has access to a privately stored matrix and its inverse,
## both of which can be publicly modified and accessed using their accessor
## functions.



## makeCacheMatrix(x = matrix())
##
## This function returns a list which represents an object with functions
## to interact with the private matrix and its inverse.
## list members:
##  set(y) - sets the internal matrix to y and clears the cached 'inverse'
##  get() - returns the internal matrix
##  setinverse(i) - sets the internal inverse to i
##  getinverse(i) - returns the internal inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  # function definitions to access private 'x' and 'inverse'
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x

  setinverse <- function(i) inverse <<- i
  
  # (it would actually be best to put the whole solver inside this function,
  #  proper way to work a straightforward cache, oh requirements ...)
  getinverse <- function() inverse
  
  # return the object with the members needed to 
  # interact with the private variables.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## takes a "cache Matrix" as an argument, and returns the inverse 
## of the matrix passed to it.
## If the matrix needs an inverse (result of 'solve()') assigned to it, then
## it will assign the computed 

cacheSolve <- function(x, ...) {
  
  # Try to get the inverse
  inverse <- x$getinverse()
  
  # use the inverse if it exists
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  # if not, compute the inverse from the solve function,
  # using any additional arguments if needed.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  inverse
}
