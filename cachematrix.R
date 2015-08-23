## The first function makeCacheMatrix creates special matrix object, 
## which is a list containing function to: 
## set the value of matrix
## get the value of matrix
## set the value of inverse matrix
## get the value of inverse matrix
## The second function CacheSolve computes the inverse of the special matrix. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value of 
## inverse matrix in the cache via setinversematrix function

## Function creates special matrix object than can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
  setmatrix<-function(y) {
    x<<-y
    m<<-NULL
  }
  getmatrix<-function() x
  setinversematrix<-function(s) m<<-s
  getinversematrix<-function() m
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinversematrix=setinversematrix,getinversematrix=getinversematrix)

}

## Function computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  l<-x$getinversematrix()
  if (!is.null(l)){
    message("getting cached matrix")
    return(l)
  }
  matrix<-x$getmatrix()
  l<-solve(matrix, ...) ##compute inverse matrix
  x$setinversematrix(l) ##
  l   ## Return a matrix that is the inverse of 'x'
}
