## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  Set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  Get <- function() x
  SetInverseMatrix <- function(inverse) InvMatrix <<- inverse
  GetInverseMatrix <- function() InvMatrix
  list(Set=Set, Get=Get,
       SetInverseMatrix = SetInverseMatrix,
       GetInverseMatrix = GetInverseMatrix)
}

## The following function calculates the inverse of the provide matrix input. It will first check to determine if
## we have previously saved the inverse. If previously saved, the cached result will be returned without additional
## computation. If not, it will computes the inverse, save the value in the cache via SetInverseMatrix function and
## return the result.

cacheSolve <- function(x = matrix(), ...) {
  InvMatrix <- x$GetInverseMatrix()
  if(!is.null(InvMatrix)) {
    message("getting cached data")
    return(InvMatrix)
  }
  matrix <- x$Get()
  InvMatrix <- solve(matrix, ...)
  x$SetInverseMatrix(InvMatrix)
  InvMatrix
}
