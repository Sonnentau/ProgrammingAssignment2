## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  inMatrix <- NULL
  #set value matrix
  setMatrix <- function(y){
    x <<- y
    inMatrix <<- NULL
  }
  #get value matrix and set get value invert matrix
getMatrix <- function() x
setInverse <- function(inverse) inMatrix <<- inverse
getInverse <- function() inMatrix
list(setMatrix = setMatrix, getMatrix=getMatrix,
     setInverse = setInverse, getInverse = getInverse)
}


#get value of invertible matrix from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  #get original matrixdata, use solve function to inverse matrix, set and return inv matrix
  mMatrix <- x$getInverse()
  if(!is.null(inMatrix)) {
    message("Getting Cached Invertible Matrix")
    return(inMatrix)
}
  #if value of the invertible matrix is NULL then
  MatrixData <- x$getMatrix()
  inMatrix <- solve(MatrixData, ...)
  x$setInverse(inMatrix)
  return(inMatrix)
}
#return matrix inverse of x
