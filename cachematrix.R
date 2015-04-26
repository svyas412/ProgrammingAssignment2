
## The Purpose of this programming assignment is to code R function 
## that is able to cache potentially time-consuming computation  such as Matrix inversion
## This code contains pair of functions that cache the inverse of a matrix.
##  ***********************************************************************************  ##


## The first function, "makeCacheMatrix" creates a special "matrix", 
## which is a list containing a function to
## 1) set the value of the matrix  2) get the value of the matrix
## 3) set the value of the mean    3) get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get<-function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list (set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)

}
##  -----------------------------------------------------------------  ##  


## The second function "cacheSolve" computes the 
## inverse of the special "matrix" returned by "makeCacheMatrix" above. 
## if it is not calculated already

cacheSolve <- function(x=matrix(), ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
  
}

## End ##
