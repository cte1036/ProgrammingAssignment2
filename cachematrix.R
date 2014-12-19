## Put comments here that give an overall description of what your
## functions do
## Objectives:
## I. Create a container with the following properties:
##    1) Stores 2 objects
##       1a) A square matrix
##       1b) The inverse of the square matrix
##    2) Contains functions that allow us to:
##       2a) Retrieve the values of the 2 stored objects
##       2b) Set the values of the 2 stored objects
##       Given R's lexical scoping rule, these functions must be defined
##       within the container.
##       Otherwise, they will not be able to access the stored objects
##       inside the container.
## II. Create a function that allows conditional initialization 
##     of one of the stored objects (i.e., the inverse of the matrix)
##     If the stored matrix has changed since the last time the inverse
##     was calculated, calculate a new inverse
##     If the stored matrix has not changed since the last time the inverse
##     was calculated, don't calculate a new inverse. Retrieve the inverse
##     stored in the container.
##     If a matrix is stored, but an inverse has not yet been calculated,
##     calculate an inverse

## Write a short comment describing this function
## makeCacheMatrix (to satisyf objective I above)
##  Creates placeholders for the 2 stored objects
##     x (formal argument): to store matrix
##     i (defined in function body): to store inverse
##  Defines (but does not call) 2 functions to initialize 2 stored objects:
##    setMatrix: Call this function to change the stored matrix.
##               Nulls the stored inverse (i <<- NULL) to satisfy objective II
##    setInverse: Don't call this function. Let cacheSolve call this function 
##                to ensure conditional initialization as described in II above
##  Defines (but does not call) 2 functions to retrieve values of stored objects
##    getMatrix
##    getInverse
##  Returns the 4 functions. Does not return the stored objects.
##  So, you can only access the stored objects through the functions.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        setMatrix <- function(y) {
            x <<- y
            i <<- NULL
        }
        
        getMatrix <- function() x
        
        setInverse <- function(inv) i <<- inv
        
        getInverse <- function() i
        
        list( setMatrix = setMatrix
              ,getMatrix = getMatrix
              ,setInverse = setInverse
              ,getInverse = getInverse
        )   
}


## Write a short comment describing this function
## cacheSolve (to satisfy objective II above)
## Formal argument is pointer to the object created by calling makeCacheMatrix
## Calls getInverse() to retrieve stored inverse
##   If an inverse is stored (inverse is not null), returns the stored inverse
##   and the function ends
##   If an inverse is not stored (inverse is null), does the following:
##      Calls getMatrix to retrieve stored matrix from container
##      Calculates the inverse of the stored matrix using solve()
##      Calls setInverse to store the calculated inverse in the container
##      Returns the inverse


cacheSolve <- function(q, ...) {
        i <- q$getInverse()
        
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        matrix <- q$getMatrix()
        inv <- solve(matrix, ...)
        q$setInverse(inv)
        inv        
}
