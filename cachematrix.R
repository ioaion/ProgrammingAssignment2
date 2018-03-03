## Matrix inversion

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse1<-NULL
  set<-function(y){
    x<<-y
    inverse1<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse2)  inverse1<<-inverse2
  getinverse<-function () inverse1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.  

cacheSolve <- function(x, ...){
  inverse1<-x$getinverse()
  if(!is.null(inverse1)){
    message("getting cached data")
    return(inverse1)
  }
      matrix_data<-x$get()
      inverse1<-solve(matrix_data)
      x$setinverse(inverse1)
      inverse1
}
