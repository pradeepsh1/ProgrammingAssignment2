## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## define the cache cachedInverserMatrix
  cachedInverserMatrix <- NULL
  
  set <- function(y) {
    ## assign the input matrix to the variable x in the parent environment
    x <<- y
    
    ## re-initialize cachedInverserMatrix to null
    cachedInverserMatrix <<- NULL
  }
  
  get <- function() x ## return the matrix x
  
  ## set the cache cachedInverserMatrix equal to the inverse of the matrix x
  setinverse <- function(inverse) cachedInverserMatrix <<- inverse
  
  ## return the cached inverse of x
  getinverse <- function() cachedInverserMatrix
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  cachedInverserMatrix <- x$getinverse()
  
  if(!is.null(cachedInverserMatrix)) {
    message("getting cached data.")
    return(cachedInverserMatrix)
  }
  
  data <- x$get()
  cachedInverserMatrix <- solve(data)
  x$setinverse(cachedInverserMatrix)
  cachedInverserMatrix
}
