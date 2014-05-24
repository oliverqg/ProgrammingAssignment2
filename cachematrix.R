## Put comments here that give an overall description of what your
##  This function creates a special matrix which is really a list containing a
##  function to set the value of the matrix
##  function to get the value of the matrix
##  function to set the value of inverse
##  function to get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set<-function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get<-function() x

  setinverse <- function(i) inv<<-i
  
  getinverse <-function() inv
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## The following function calculates the inverse of the special matrix created
## makeCacheMatrix.  It first checks if the inverse has already been calculated
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m,...)

  x$setinverse(i)
}
