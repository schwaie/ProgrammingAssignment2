##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly 
##the first function makes a cache matrix, and the second function computes the 
##inverse of the matrix returned by the first function

##makeCacheMatrix is a function that takes as input a matrix, 
##and then makes a cache of the inverse of said matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                ##the <<- operator is used to assign a value to an object in an 
                ##environment that is different from the current environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inverse) inv <<- Inverse
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        ##if inverse matrix is not null
        if(!is.null(inv)) {
                message("getting cached data")
                ##return inverted matrix
                return(inv)
        }
        #if value of inverted matrix is null
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
#end
