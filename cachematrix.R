## These functions A) create a Matrix that is able to cache the inverse 
## of that Matrix and B)Retrieve the inverse of the Matrix created and Cached
## in A 

## Creates a Matrix that is able set, get, set inverse and get inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(y) {
	x <<- y
	inv <<- NULL }
	get = function() x
	setinv = function(inverse) inv <<- inverse 
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Returns the inverse of the Matrix above and can return from cache if it has been calculated previously

cacheSolve <- function(x, ...) {
	inv = x$getinv()
	if (!is.null(inv)){
      message("getting cached data")
      return(inv)
	mat.data = x$get()
      inv = solve(mat.data, ...)
	x$setinv(inv)
      return(inv)}
}
