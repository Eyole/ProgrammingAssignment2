## Put comments here that give an overall description of what your
## functions do

library (MASS)

## Write a short comment describing this function
## The function makeCacheMatrix is to Get/Set the value of the Matrix and Set/Get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) 
{
  m <-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  
  setmatrix<-function(ginv) m<<- ginv
  
  getmatrix<-function() m
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function 
## The cacheSolve fuction takes a Matrix and outputs an inverse of the matrix from cache
## or computation of its own - if not present in cache.

cacheSolve <- function(x=matrix(), ...) 
{
  m<-x$getmatrix()
  
  if(!is.null(m)) #if already computed retrieve from cache.
  {
    message("getting cached data")
    return(m)
  }
  
  matx<-x$get()
  
  m<-ginv(matx, ...) #perform the matrix inverse.
  
  x$setmatrix(m)
  
  m #return the inverse matrix.
}