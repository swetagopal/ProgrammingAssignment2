## This function caches matrix and inverse of a single matrix at a time 
## to save repeated calculation of inverse for the same matrix in scope

## To Use: source <this file>
##         cacheSolve(my_mat)

## Cache Variables are globally intialized in the calling environment 
 
m_inv <- NULL
m_cache <- NULL


## This function is to set the value of martrix and its inverse in the cache

makeCacheMatrix <- function(mat = matrix()) {
  set <- function(y) {
    m_cache <<- y
    m_inv <<- NULL
  }
  get <- function() m_cache
  setinverse <- function(inverse) m_inv <<- inverse
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function cheks the cache if the matrix has been computed previously - if Yes, uses the values from cache
## If not, calculates and calls the above function to set it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- makeCacheMatrix(x)
  cachem<-m$get()
  if((exists("cachem")) && identical(cachem,x) ) {
    message("Getting cached data")
    return(m$getinverse())
  }
  message("Not in cache data. Calculating inverse")
  m$set(x)
  inv <- solve(x)
  m$setinverse(inv)
  inv
}
