# Programming Assignment 2: Lexical Scoping 

# Description: Functions to create cache and calculate inverse
# makeCacheMatrix(): Create cache and return list of values/functions
# cacheSolve(): Check for existing value and if none, then solve matrix

# Source Citations - Referenced for developing solutions:
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

# makeCacheMatrix(): Create cache and get/set value from it
# Code Source Citation: Assignment 2 instructions/example code
# Note: Code modified for solution
makeCacheMatrix <- function(x = matrix()) {
    # Define get/set functions
    m <- NULL 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    # Define get/set inverse functions
    set_inv <- function(inv) m <<- inv
    get_inv <- function() m
    
    # Return values/functions in a list
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

# cacheSolve(): Check for existing value; it none exists, calculate it.
# Code Source Citation: Assignment 2 instructions/example code
# Note: Code modified for solution
cacheSolve <- function(x, ...) {
    # Checks if value already exists
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # If value does not exist, then calculate inverse
    data <- x$get()
    # Note: Use solve() function per AS2 instructions
    m <- solve(data, ...)
    x$set_inv(m)
    m
}

# Test Scripts: Run functions with 3x3 matrix
# https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

# Create matrix with objects/functions to be solved
i1 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

# Create matrix with objects/functions to be solved
testMatrix <- makeCacheMatrix(m1)
solveMatrix <- cacheSolve(testMatrix)
print('calculated matrix:')
print(solveMatrix)

# Verify with solve() function
checkMatrix <- solve(m1)
print('solve() results:')
print(checkMatrix)

# Verify that cache works correctly
checkCache <- cacheSolve(testMatrix)
print('verify cache (returns same result):')
print(checkCache)

# Test Scripts: Run individual functions with 3x3 matrices
# Source Citations:
# https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

# Initiate cache and verify results
testMatrix <- makeCacheMatrix(i1)

# Create matrix, return functions in list, then test them
print('create and get matrix with testMatrix$get() function:')
print(testMatrix$get())

# Try to retrieve inverse value, which should return null
print('check for value/null with testMatrix$get_inv() function:')
print(testMatrix$get_inv())

# Set matrix with new value
testMatrix$set(m1)

# Calculate inverse of new matrix
print('solve new matrix with cacheSolve(testMatrix) function:')
print(cacheSolve(testMatrix))

# Retrieve cached value directly
print('get new inverse directly with testMatrix$get_inv() function:')
print(testMatrix$get_inv())
