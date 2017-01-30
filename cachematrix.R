## cacheSolve takes makeCacheMatrix to calculate inverse of the matrix
## and store it in the cache for later reuse.

## makeCacheMatrix takes a matrix and add helper functions to
## get/set matrix and get/set cached value of inverse of the matrix.
## Returns list with get, set, getInverse, setInverse functions.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL;

    get <- function () {
        x;
    };
    set <- function (y) {
        x <<- y;
        i <<- NULL;
    };
    getInverse <- function () {
        i;
    };
    setInverse <- function(inverse) {
        i <<- inverse;
    };

    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse);
};


## cacheSolve takes matrix wrapped in makeCacheMatrix function and extra arguments
## for the solve function.
## cacheSolve will return cached version of inverted matrix if available,
## othervise will calculate inverse of the matrix and store it in the cache.
## Returns inverse of the matrix.
cacheSolve <- function(mtxCache, ...) {
    ## Check if mtxCache has cache of inverted matrix
    i <- mtxCache$getInverse();
    if (!is.null(i)) {
        ## cache found return inverted matrix
        message("getting cached data");
        return (i);
    }

    ## cache not found get matrix
    m <- mtxCache$get();

    ## calculate inverse of the matrix
    i <- solve(m, ...);

    ## store that calculation back into the cache
    mtxCache$setInverse(i);

    ## return inverted matrix
    i;
};
