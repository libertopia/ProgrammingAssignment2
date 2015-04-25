## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. sets what of the matrix needs to be inversed
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
      x <<- y
      s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## Write a short comment describing this function

## The following function inverses the 
## special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean 
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()
      if(!is.null(s)) {
      message("getting cached data")
      return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
