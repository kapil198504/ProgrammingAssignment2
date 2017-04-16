## Below there are two functions
# 1. makeCacheMatrix
# 2. cacheSolve
## The function makeCacheMatrix createa list with functions like set,get,setinverse and getinvers.
# set function stores the input matrix in cache
# get fuction returns the same when called
# setinverse function calculates the inverse of the matrix and stores it in cache in a variable 'm'.
# getinverse function returns the inverse of the function stored in cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)

}


## The function cacheSolve takes list, returned from the makeCacheMatrix as its input.
# It invokes the functioni getinverse(). Then it validates the value retrunded whether it is null or not null.
# If it is found to be not null then returns the inverse of the matrix with a message.
# If it is null then it invokes the function get() from the list. It returns the matrix for which inverse is to be calculated.
# Then it invokes the function solve(). This function calculates the inverse of the matrix.
# Once the inverse is calculated the function setinverse() is invoked to hold the value in cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if( ! is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
