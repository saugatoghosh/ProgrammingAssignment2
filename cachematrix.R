## In this assignment we use the scoping rules of R to cache the potentially 
## time-consuming computation of the inverse of a matrix. If the contents of a 
## matrix are not changing, it may make sense to cache the value of the inverse
##so that when we need it again, it can be looked up in the cache rather than 
##recomputed. 

## The first function creates a cache where the inverse of a matrix can be stored
## using the << operator. The value of the inverse is initially set to null.

makeCacheMatrix <- function(x = matrix()) {
              inv <- NULL
              set <- function(y) {
                      x <<- y
                      inv <<- NULL
              }
              get <- function() x
              setinv <- function(solve) inv <<- solve
              getinv <- function() inv
              list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}


## This function calculates the inverse of the special 'matrix' created with 
##the above function. However, it first checks to see if the inverse has already 
##been calculated. If so, it gets the inverse from the cache and skips the 
##computation. Otherwise, it calculates the inverse of the data and sets the value
##of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

##An example given below

a = matrix(c(4,2,7,6), 2,2)

b = makeCacheMatrix(a)

cacheSolve(b)