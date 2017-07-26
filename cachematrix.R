## Vasuprad.Kanade@accentre.com
## Function to use Lexical Scoping of R
## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # 1. set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # 2. get the value of the matrix
  get <- function() x

  # 3. set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse

  # 4. get the value of inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  # 1. Get the value of the matrix
  inv <- x$getinverse()
  # 2. Checks if the inverse has already been computed
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  # 3. Computes the inverse
  data <- x$get()
  inv <- solve(data)
  # 4. Sets the value in the cache via setinverse function
  x$setinverse(inv)
  inv
}
