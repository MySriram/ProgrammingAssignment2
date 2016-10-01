## The function makeCacheMatrix uses the solve function of R to solve the marix
## and finds the inverse of a given matrix
## and stores it in the cache
## get and set define the get and set functions
## Getinverse and setinverse are for the inverse values 

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  
  get<- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function takes the value of the matrix passed and returns the value of it's inverse.
## if the  same data is present in the cache, it avoids recalculating the inverse value and
## instead it prints the message "getting cached value" and then returns the value
## of the inverse back.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  if (!is.null(m)) {
    message("getting cached data")
    
    return (m)
  }
  data <-x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}


