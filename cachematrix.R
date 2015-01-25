## cachematrix.R
# Contains two functions:
## makeCacheMatrix - takes a matrix, stores it, and returns functions that can 
## get or set the matrix, as well as get or set the inverse of the matrix
## cacheSolve - take a list in the form of the return value from makeCacheMatrix()
## then determines the inverse by looking in cache first, and if not in cache, 
## computing the value using solve() function and returning it.

## makeCacheMatrix - caches a matrix, and returns list of 
#     that can be called to perform certain functions on that matrix.
# Args:
#     x: a matrix, assumed to be square and invertible.
#
# Returns:
#    A list with function names to perform certain operations on a matrix.
makeCacheMatrix <- function(x = matrix()) {
        # create variable to hold the inverse of the matrix passed in
        inv <- NULL
        
        # set the value of the matrix, and set variable holding
        # inverse of the matrix to NULL (essentially, reset the inv)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # return the the matrix we're using
        get <- function() x

        # set the inverse of the matrix
        setinverse <- function(inverse) {
                inv <<- inverse
                print("inside set inverse function")
        }
        # get the inverse of the matrix
        getinverse <- function() inv
        
        # get the environment we're using
        getevn<- function() environment()
        
        # return list of matrix operations we can use
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             getevn = getevn)
}

# cacheSolve - Computes inverse of a matrix either by pulling value from cache
# or solving using the solve() function
#
# Args:
# x: A list containing function names to perform various operations
#    on a matrix
#
# Returns:
#    Inverse of the matix stored in cache by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        
        # create a variable to hold the matrix
        inv <- x$getinverse()

        # check if it is NOT NULL
        # if NOT NULL, then return the result of solve(x)
        if (!is.null(inv)) {
                
                # the inverse exists in cache - return it
                message("getting cached data")
                
                return(inv)
        }
        
        # if we get here, matrix inverse was NULL - solve for inverse
        # then set the inverse value using set function 
        # finally, return inverse value
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}