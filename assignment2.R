makeCacheMatrix <- function(x = matrix()) {
         ## x: a square invertible matrix
         ## return: a list containing functions to
         ##              1. set the matrix
         ##              2. get the matrix
         ##              3. set the inverse
         ##              4. get the inverse
         ##         this list is used as the input to cacheSolve()
         
         inv = NULL
         set = function(y) {

                 x <<- y
                 inv <<- NULL
         }
         get = function() x
         setinv = function(inverse) inv <<- inverse 
         getinv = function() inv
         list(set=set, get=get, setinv=setinv, getinv=getinv)
 }
 
 cacheSolve <- function(x, ...) {
         ## @x: output of makeCacheMatrix()
         ## return: inverse of the original matrix input to makeCacheMatrix()
         
         inv = x$getinv()
         
         # if the inverse has already been calculated
         if (!is.null(inv)){
                 # recover from the cache and skip the computation. 
                 message("Retrieveing from cached data")
                 return(inv)
         }
         
         # otherwise, calculates the inverse using R's solve function
         mat.data = x$get()
         inv = solve(mat.data, ...)
         
         # sets the value of the inverse in the cache via the setinv function
         x$setinv(inv)
         
         return(inv)
 }
# test <- matrix(c(4,7,2,6),2,2)
# testCached<-makeCacheMatrix(test)
# testCached
