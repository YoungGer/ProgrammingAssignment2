## Put comments here that give an overall description of what your
## functions do

## build special matrix-list object
## list contains these methods:
## set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(invert) inv <<- invert
    getinv <- function() inv
    
    # a list object has been returned
    list(set=set, get=get,
         setinv=setinv,getinv=getinv)
}


## solve the invert matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #return the object if the invert matrix do exist
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # solve process if the invert matrix doesn't exist
    data <- x$get()
    inv <- solve(data,...)
    
    x$setinv(inv)
    inv
    
}
