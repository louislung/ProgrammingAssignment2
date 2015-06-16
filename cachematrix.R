## These two functions are used to calculate the inverse of a square matrix
## And if it has been calculated before, the value can be cached 

## this function create a special matrix that can set/get the value of 
## the matrix/inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        matrix(c(get, setinv, getinv))
}


## this function will check if the inverse has been calculated before
## If not, it will calculate the invese and store it inside the special
## matrix created above, so that we can cached the value next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x[[3]]()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x[[1]]()
        inv <- solve(data)
        x[[2]](inv)
        inv
}
