## These are two functions that in combination will find the inverse of 
## an invertibe matrix and cache it, and return the cached matrix inverse
## if it has been previously cached.
##
## The function makeCacheMatrix returns a list given the input 
## of a square matrix x the list is intended be used in the 
## function cacheSolve defined below
## 
##
makeCacheMatrix <- function(x = matrix()) {
   my_mat <- NULL
   set <- function(y){
       x <<- y
       my_mat <<- NULL
   }
   get <- function() x
   setinv <- function(solve) my_mat <<- solve
   getinv <- function() my_mat
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv) 
}
##
## This function will take the list from the makeCacheMatrix function 
## and determine whether the inverse has been cached. 
##
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x' as 'my_mat'
## Check to see if the inverse is cached and let user know if it is
  my_mat <- x$getinv()
  if(!is.null(my_mat)) {
      message("getting cached data")
  }
  ## compute the inverse if not cached
  else{
  data <- x$get()
  my_mat <- solve(data, ...)
  x$setinv(my_mat)
  }
## In either case, return the inverse as my_mat
my_mat
}
##
## Examples - uncomment below and run
##
# D  <- matrix(c(1,0,1,2,1,0,0,0,1), nrow = 3, ncol= 3)
# B <- makeCacheMatrix(D)
# cacheSolve(B)
# cacheSolve(B)  #(the second run should retrieved the cached inverse)
#
# B$set(matrix(1:4,nrow=2,ncol=2))
# cacheSolve(B)
# cacheSolve(B)  (the second run should retieve the cached inverse)
