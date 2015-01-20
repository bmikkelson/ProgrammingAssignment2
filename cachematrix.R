## This pair of functions solves an invertible square matrix, then caches the 
## solution for subsequent requests instead of running the calculation again.
## Where b is an invertible square matrix, they should be run as:
## > yoyo <- makeCacheMatrix(b)
## > cacheSolve(yoyo)
## Subsequent runs of cacheSolve(yoyo) will look up the solution in the cache
## instead of running the calculation again.

# makeCacheMatrix
# The first time this program is run it resets the value of "i" within 
# makeCacheMatrix (which will be used to store a cached solution to the matrix
# called in cacheSolve).
# The function "set" will take an input "y" and overwrite it onto "x" within 
# makeCacheMatrix, and also reset "i" to NULL within makeCacheMatrix, so that 
# there is no cached solution.
# The function "get" will send back the value of "x" no matter the input.
# The function "setinv" will globally assign the input value "inv" to "i", the 
# previous cached solution, within makeCacheMatrix.
# The function "getinv" will send back the value of "i" no matter the input.
# FInally, the list allows all of the functions to be called by another program
# by using x$"fxn_name".

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



# cacheSolve
# Runs getinv() in makeCacheMatrix, which copies the data for "i" into this
# function. 
# If "i" is not null, then that means that "i" is the cached solution for the
# inverse of x, and "i" is returned, following a note signaling that cached data
# exists, and ends the program.
# However, if "i" is null, then that means that there is no cached solution for 
# the inverse of x, so it needs to be calculated:
# Runs get(), which gives back x, which is then stored as "data".
# Then the inverse of data (the inverse of x) is calculated and stored as "i".
# Then setmean(i) is run, which copies "i" to the "i" in makeCacheMatrix, so 
# there is now a cached solution if we run this program again.
# Finally, the solution "i" is given.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

