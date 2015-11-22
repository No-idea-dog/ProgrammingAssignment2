## These functions create a matrix, and allows the user to find
## its inverse from the cached data. This means that an intensive
## process need only occur once.

## makeCacheMatrix creates a matrix, the inverse of which may
## be cached. It creates and sets the matrix's value and inverse.

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function()i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix created above.
## If the inverse has been cached it will used the cached data
## rather than computing it again.

cacheSolve<-function(x, ...){
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
    ## Return a matrix that is the inverse of 'x'
}
