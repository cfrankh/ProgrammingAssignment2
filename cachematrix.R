## These functions are an example of the use of Lexical Scopting in R
## 
## Taken from R Documentation:
## The <<- assigment looks back in enclosing environments 
## for an environment that contains the symbol total and when 
## it finds such an environment it replaces the value, 
## in that environment, with the value of right hand side.

## The function makeCachMatrix creates a vector which contains a list 
## of four functions and allows initiation with the data (a matrix) 
## and a NULL value for the inverse of the matrix (variable called inv)

makeCacheMatrix <- function(x = matrix()) {
    # inv is the inverse of the matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix. However,
## it first checks to see if the value stored in inv is non-NULL.
## If this is the case it returns the stored value rather than
## calling the function to calculate the inverse (thereby saving time)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
