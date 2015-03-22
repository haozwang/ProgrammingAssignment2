## This function calculates the inverse of a matrix and caches that result. The function
## calls up the precalculated value from the cache if the we need to calculate the inverse
## for the same matrix


## Generate a list of 4 functions
## set makes m NULL in the y environment and assigns a matrix to x under the makeCacheMatrix environment
## get returns the value of x under set
## setminv takes a value and set it to the value of m in the makeCacheMatrix environment 
## getminv returns up m after performing setminv
## finally creates a list of the four 4 functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setminv <- function(solve) m <<- solve
  getminv <- function() m
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
  
}


## In this function: if there is a cached value for the matrix inverse, then display a message and take the cached
## value. Otherwise calculate the matrix inverse using solve() and store that as a cached value.

cacheSolve <- function(x, ...) {
  m <- x$getminv() 
  
  ## call cached value if there is a cached value
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  ## if a cached value does not exist, then calculate the invese of the matrix using solve(), store that value and
  ## display that value.
  data<- x$get()
  m <-solve(data, ...)
  x$setminv(m)
  m
  
}
