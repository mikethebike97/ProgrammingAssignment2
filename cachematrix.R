## These functions together make a special matrix object, and store its inverse with it
## When asked to find the inverse of a matrix, it first searches to see if already cached.
## If not, it then finds the inverse.
## Assuming matrix is always square and always invertible

## Similar to makeVector. Makes a special Matrix object, which is put into cacheSolve
## Creates 4 functions, set, get, getinv, setinv, where inv is short for inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                          
        set <- function(y) {               
                x <<- y                
                inv <<- NULL             
        }
        get <- function() x                
        setinv <- function(inverse) inv <<- inverse  
        getinv <- function() inv          
        list(set = set, get = get,        
             setinv = setinv,
             getinv = getinv)
}


## Similar to cacheMean. Searches to see if inverse of special matrix object is cached.
## If it is not, calculates the inverse

cacheSolve <- function(x,...) {
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
