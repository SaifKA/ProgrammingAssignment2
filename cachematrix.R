## makeCacheMatrix function takes a matrix as input. It produces a list containing functions
## to set value of a matrix, get value of the matrix, set inverse of the matrix and get inverse
## of the matrix.
## cacheSolve function calculates the inverse of a matrix. If the result is already in cache, it 
## fetches it from cache. Otherwise, it calculates the inverse matrix and stores it in cache.
## Note: It assumes that the supplied matrix is invertible

## Function to take matrix as input. It contains four functions in a list which are used  
## to set value of a matrix, get value of the matrix, set inverse of the matrix and get inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) m<<-inverse
  getInverse<-function() m
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse
       
  )
}


## Function to inverse a matrix. If result is in cache it fetches it, otherwise it recalculates
## the inverse and store it in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("Getting Cached Data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}




