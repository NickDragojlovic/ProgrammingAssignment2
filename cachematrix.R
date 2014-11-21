## These two functions create a system through which the inverse of a specific matrix can
## be cached during the execution of an R program without defining a variable in the global
## environment. This allows other expressions and functions to retrieve the value of this inverse
## without having to re-calculate it if it has already been calculate (which can be 
## computationally intensive), but also calculates the inverse if it has not, meaning
## that these two functions return the inverse of the matrix used as an argument in
## the least effortful manner available.


## The makeCacheMatrix() function creates a vector containing four functions that share an
## environment which contains values for the input matrix (x) and its inverse (inv).
## "set" allows the cache to be re-set to NULL and a new matrix "y" to be set as the 
## input for the function. "get" returns the matrix that is the input for the function.
## "setinv" allows inv to be set at a specific value. "getinv" returns the value of the
## cached inverse (inv).

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## The cacheSolve() function first attempts to retrieve the cached inverse matrix from the
## vector created by makeCacheMatrix (which is the input argument of the function). If
## the cached inverse variable is not NULL, it returns the value assigned to it in the
## environment in which the vector was created. If inv is NULL, it calculates the inverse
## using the solve() function, assigns that value to the "inv" variable in the input
## vector's environment (the cache), and returns the inverse matrix.

cacheSolve <- function(x) {
        
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
}

