# ndunna 2/20/2015 R Programming Language Course : Assignment 2
# These functions were written based on the example given in course material. 
# 
# Solve() method is used to generate the matrix inverse.
# This function is used to cache the inverse as the matrix inverse computaion is expensive
# and time consuming.
# Below are two special functions to achieve the result.

# The first function, makeCacheMatrix creates a special "vector", which is 
# really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix 
# get the value of the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  # function to return the matrix
  setinverse <- function(inverse) m <<- inverse # function to set the inverse 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# The following function cacheSolve() returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the # computation. If not, it computes 
# the inverse, sets the value in the cache via setinverse function.


#For this assignment, it is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    # return the value if the inverse is already coomputed. Check against the null.
    if(!is.null(m)) { 
      message("getting cached data")
      return(m)
    }
    
    # Get the matrix, compute inverse using solve method before returning the inversse value.
    
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}