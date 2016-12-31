## caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(matrix_inverse) matrix_inverse <<- matrix_inverse
  get_inverse <- function() matrix_inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$get_inverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$set_inverse(inverse_matrix)
  inverse_matrix
}


solve(matrix(c(3,2,1,1),nrow=2,ncol=2)) # inversable matrix example
matrixCache <- makeCacheMatrix(matrix(c(3,2,1,1),nrow=2,ncol=2)) # inversable matrix cache
cacheSolve(matrixCache) # calculated
cacheSolve(matrixCache) # form cache