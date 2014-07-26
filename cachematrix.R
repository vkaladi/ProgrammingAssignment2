## The two functions makeCacheMatrix and CacheSolve together 
## allow us to avoid recomputing the inverse of a given
## matrix when CacheSolve is used instead of calling Solve() 
## directly.
##
## Sample usage:
## 
## >  m <- rbind(c(1, -1/4), c(-1/4, 1)) ## a simple 2 X 2 matrix m
## >  mtrx < - makeCacheMatrix()         ## call makeCacheMatrix to create mtrx as a list of functions with no data
## >  mtrx$set(m)                        ## use the set() function in mtrx to assign the matrix m as the data
## >  cacheSolve(mtrx)                   ## call cacheSolve for the first time to return the inverse by calling Solve()
## >  cacheSolve(mtrx)                   ## call cacheSolve the 2nd time,  
##                                          The message "getting the cached inverse of the matrix"
##                                          is displayed before returning the inverse without calling Solve() again
## >  cacheSolve(mtrx) %*% m             ## Use matrix multiplication with original matrix m to get identity matrix
##
##
#
   
## This function makeCacheMatrix returns a list of functions: get, set, getinverse, setinverse
## This shows the use of lexical scoping to assign values to a variable in the parent environment
## The variable inv is defined within makeCacheMatrix.  However, the functions set() and setinverse() 
## assign values to inv from inside these functions by using <<- instead of <-

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
              x <<- y          ## the argument y to set() is assigned to x in the parent environment
              inv <<- NULL     ## this assigns NULL to the free variable inv in the parent environment
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse ## this assigns the argument inverse to variable inv in the parent  environment
      getinverse <- function() inv        ## this is returning the stored value in variable inv
      list (set=set, 
            get = get,  
            setinverse = setinverse,
            getinverse = getinverse
           )                           ## list of functions returned, similar to methods in Object oriented programming

}


## This function cacheSolve uses getinverse() to return the value of the variable inv
## that is stored.  The first time "inv" is always NULL. If inv has already been calculated once
## and stored, is.null(inv) will return false and hence the message is displayed 
## and the value in inv is returned without calling solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()                    
      if(!is.null(inv)) {                                         ## inv is not NULL
              message("getting the cached inverse of the matrix")
              return(inv)                                         ## return from this function with stored non-NULL inv
      }
      data <- x$get()                                             ## get  "data", the input matrix
      inv <- solve(data, ...)                                     ## call solve() to find inverse of "data"
      x$setinverse(inv)                                           ## store the calculated inverse
      inv                                                         ## return the newly calculated inverse in "inv"
}
