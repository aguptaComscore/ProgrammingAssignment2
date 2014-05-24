# makeCacheMatrix creates the matrix and calculates the inverse of the matrix.
# If the matrix inverse is calculated previously, it will find it in the cache 
# and return it, and not calculate it again.

# This function set the value of matrix, get the value of the matrix
# It sets the value of inverse and also get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL				# inv will store the inverse matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x		# Get the matrix
    set_inverse <- function(inverse) inv <<- inverse	# Set the inverse
    get_inverse <- function() inv						# Get the inverse
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

# cacheSolve calculates the inverse of the matrix. If the inverse is 
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {		# If the inverse is calculated, return it
        message("This is cached matrix")
        return(inv)
    }
    data_matrix <- x$get()
    inv <- solve(data_matrix)
    x$set_inverse(inv)
    inv						# return inverse
}
