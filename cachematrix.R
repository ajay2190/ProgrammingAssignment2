## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to compute the inverse of the special "matrix" created by 
## makeCacheMatrix above. Inverse has already been calculated (and the 
## matrix has not changed), then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


##My solutions

my_matrix <- makeCacheMatrix(matrix(3:6, 2, 2))

my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

cacheSolve(my_matrix)

my_matrix$getInverse()

my_matrix$set(matrix(c(3, 4, 1, 5), 2, 2))

my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

cacheSolve(my_matrix)

my_matrix$getInverse()

