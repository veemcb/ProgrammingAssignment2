## The cachematrix function comprises two related functions which store the matrix
## and its inverse in the cache

cachematrix <- function (x = matrix()){
  inv <- NULL

## This function puts a new matrix, z, in the cache and nulls the current inverse
  makeCacheMatrix <- function(z) {
    x <<- z
    inv <<- NULL
  }

## This function solves for the inverse of the matrix, x, and keeps the result in cache.
## It will only recalculate the inverse if a new matrix has been read in.
  cacheSolve <- function() {
        ## Return a matrix that is the inverse of 'x'
    if (!is.null(inv)){
      return(inv)
    } else {
      inv <<- solve(x)
      return(inv)
    }
  }
  list(makeCacheMatrix=makeCacheMatrix, cacheSolve=cacheSolve)
}
