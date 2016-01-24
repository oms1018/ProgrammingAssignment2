## The functions are used to create a matrix, perform martrix inversion operations and cache the
## inverted matrix

## The makeCacheMatrix function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  g <- NULL
  set <- function(y) {
    x <<- y
    g <<- y
    m <<- NULL
    
  }
  get <- function() x
  getmatrix <- function() g
  setmatrix <- function(gmatrix) g <<- gmatrix
  setinverse <- function(inversematrix) m <<- inversematrix
  getinverse <- function() m
  list(set = set, get = get, getmatrix = getmatrix, setmatrix = setmatrix,
       setinverse = setinverse, getinverse = getinverse)
  

}


##The cacheSolve function compute inverse of a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (is.null(m)) {
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    x$setmatrix(data)
    m
  } else if (identical(x$getmatrix(),x$get())) {
    message("getting cached data")
    x$setmatrix(data)
    return(m)
  } else {
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    x$setmatrix(data)
    m
  }
    

   
}