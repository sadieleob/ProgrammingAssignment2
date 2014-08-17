## This two functions calculates the inverse of a given invertible matrix. In case the inverse has already been calculated the inverse is nto calculated but that is retrieved. The matrix given as argument is assumed to be invertible. 

## This function (makeCacheMatrix) creates a list and its components are 4 functions(1-set, 2-get, 3-setMatrix and 4-getMatrix).  

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
   x <<- y
   m <<-NULL
   }
   get <- function() {x}
   setMatrix <- function(matrix) {m <<-matrix}
   getMatrix <- function() {m}
   list(set = set, get = get, setMatrix = setMatrix, 
   getMatrix = getMatrix)

}


## This function (cacheSolve) calculates the inverse of the given matrix only if the inverse has not been calculated yet. If the inverse had been calculated, this function delivers a message "getting cached Matrix" and then return the already calculated inverse matrix. 

cacheSolve <- function(x, ...) {
        m <- x$getMatrix()
	if (!is.null(m)) {
	  message("getting cached Matrix")
	  return(m)
	}
	matrix <- x$get()
	m <-solve(matrix, ...)
        x$setMatrix(m)
	m
}

