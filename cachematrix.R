## makeCacheMatrix returns a list with 4 functions 

## input of the matrix is done initially by makeCacheMatrix and can be updated by vec$set

## get returns the value of the matrix

## setinverse is used by the function cache solve to save
## the invese of the matrix to the variable m 

## getinverse returns the inverse of the matrix if it has
## been calculated by cacheSolve


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function returns the inverse of a matrix, either 
## from cached data if the inverse has been calculated, 
## or calculate it if it hasn't, the update the inverse
## to the variable m 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
