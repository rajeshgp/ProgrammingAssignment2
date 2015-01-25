# Purpose of the R script is to Cache the costly function to inverse matrix.
# if the matrix is already inversed, the cached version is returned, otherwise
# the inverse operation is performed on the matrix and its cached

#The makeCacheMatrix creates a list of function to 
# get the matrix
# set the matrix
# get inverse of the matrix
# set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invMatrix <<- solve
  getInverse <- function() invMatrix  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

  
}


## Function calculates the inverse of a matrix, if the inverse has already been calculated,
## function returns the cached version, otherwise it inverses the matrix and
## caches a copy


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  
  ## determine if the matrix is invertible
  if ( det( invMatrix > 0 ))
  {
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
  }
    invMatrix
}



