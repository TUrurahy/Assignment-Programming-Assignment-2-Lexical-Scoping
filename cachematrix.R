#[ENG] The makeCacheMatrix works as a function that creates "matrix", actually a list wich content is a function that:
#[POR] O makeCacheMatrix funciona como uma função que cria uma "matriz", na verdade uma lista cujo conteúdo é a função que:

# [ENG] 1. set the value of the matrix                  [POR] define o valor da matriz
# [ENG] 2. get the value of the matrix                  [POR] pega o valor da matriz
# [ENG] 3. set the value of the matrix inverse          [POR] define o valor da inversa da matriz
# [ENG] 4. get the value of the matrix inverse          [POR] pega o valor da inversa da matriz

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) m <<- inverse
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

# [ENG] The cacheSolve function calculates the inverse of "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it GETs the inverse and skips the computation.
# Otherwise, it calculates the matrix inverse and SETs the value of the inverse in the cache
# via the setInv function. [/ENG]

# [POR] A função cacheSolve calcula a inversa da "matriz" criada com a função acima.
# Porém, ela primeiro checa se a inversa já foi calculada.
# Se foi, ela  PEGA (GET) a inversa e não computa nada.
# Caso contrário, ela calcula a matriz inversa e DEFINE (SET) o valor da inversa no cache
# através da função setInv. [/POR]

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}