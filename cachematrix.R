## These two functions construct a matrix object, calculate its inverse (solve the matrix) and caches the solution. If another call is made to solve the same matrix the solution will be retrived from the cache without repeating the calculation  

## makeCacheMatrix constructs a matrix object with its associated functions(methods); [set, get, setinv, getinv]. The original matrix and the initial NULL value of inv are placed in the defining environment of the set function where they can be accerssed by the second function using the matrix's get and getinv functions. The setinv function assigns the solved value (returned by the second function) to inv in its defining environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve first checks if the inverse of the the matrix object constructed by the above function is in the cache (defining environment of getinv function). If it is there it will be retrived without repeating the calculation. If the solution is not in the cache, it will be calculated and then placed in the cache by invoking the set function of the matrix object.

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
