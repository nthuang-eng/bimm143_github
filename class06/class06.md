# Class06: R Functions
Nathalie Huang (PID: A19134713)

- [Background](#background)
- [A first function](#a-first-function)
- [A second function](#a-second-function)
- [A new cool function](#a-new-cool-function)

## Background

Functions are at the heart of using R. Everything we do involves calling
and using functions (from data input, analysis to results output).

All functions in R have at least 3 things:

1.  A **name** the thing we use to call the function.
2.  One or more input **arguments** that a are comma separated
3.  The **body**, lines of code between curly brackets { } that does the
    work of the function.

## A first function

Let’s write a silly wee function to add some numbers:

``` r
add <- function(x) {
  x + 1
}
```

Let’s try it out

``` r
add(100)
```

    [1] 101

Will this work

``` r
add( c(100, 200, 300) )
```

    [1] 101 201 301

Modify to be more useful and add more than just 1

``` r
add <- function(x, y=1) {
  x + y
}
```

``` r
add(100,10)
```

    [1] 110

Will this work?

``` r
add(100)
```

    [1] 101

``` r
plot(1:10, col="blue", typ="b")
```

![](class06_files/figure-commonmark/unnamed-chunk-7-1.png)

``` r
log(10, base=10)
```

    [1] 1

> **N.B.** Input arguments can be either **required** or **optional**.
> The later have a fall-back default that is specified in the function
> code with an equals sign.

``` r
#add(x=100, y=200, z=300)
```

## A second function

All functions in R look like this

    name <- function(arg) {
    body
    }

The `sample()` function in R is used to generate random samples from a
vector, which is useful for resampling, permutation tests, and
simulations.

``` r
sample(1:10, size =4)
```

    [1] 4 5 9 3

> Q. Return 12 numbers picked randomly from the input 1:10

``` r
sample(1:10, size =12, replace = TRUE)
```

     [1] 10  9  7  8  1  4  1  8  5  3  6  1

> Q. Write the code to generate a 12 nucleotide long DNA sequence?

``` r
bases <- sample(
  c("A", "C", "G", "T"), 12, replace = TRUE)
```

> Q. Write a first version function called `generate_dna()` that
> generates a user specified length `n` random DNA sequence?

    name <- function(arg) {
      body
    }

``` r
generate_dna <- function(n=6) {
  bases <- c("A", "C", "G", "T")
  sample(bases, size=n, replace=TRUE)
}
```

``` r
generate_dna(100)
```

      [1] "A" "G" "C" "T" "T" "A" "A" "G" "G" "C" "A" "T" "T" "A" "G" "G" "C" "T"
     [19] "G" "C" "A" "A" "A" "C" "A" "T" "A" "A" "G" "G" "A" "G" "A" "C" "G" "C"
     [37] "C" "T" "C" "G" "C" "C" "T" "C" "T" "C" "G" "C" "C" "T" "G" "A" "A" "C"
     [55] "G" "T" "A" "G" "A" "A" "C" "C" "A" "G" "C" "G" "T" "G" "G" "T" "A" "A"
     [73] "C" "G" "T" "A" "T" "C" "G" "A" "C" "G" "C" "C" "G" "G" "T" "G" "T" "T"
     [91] "A" "G" "A" "C" "G" "G" "G" "T" "A" "G"

> Q. Modify your function to return a FASTA like sequence so rather than
> \[1\] “G” “C” “A” “A” “T” we want “GCAAT”

``` r
generate_dna <- function(n=6) {
  bases <- c("A", "C", "G", "T")
  ans <- sample(bases, size=n, replace=TRUE)
  paste(ans, collapse = "")
  return(ans)
}
```

``` r
generate_dna(10)
```

     [1] "C" "T" "G" "G" "C" "T" "C" "A" "T" "T"

An example

``` r
# Example pattern (not using your bases)
x <- c("H", "E", "L", "L", "O")

paste(x, collapse = "****")
```

    [1] "H****E****L****L****O"

``` r
# returns "HELLO
```

> Q. Give the user an option to return FASTA format output sequence or
> standard multi-element vector format?

``` r
generate_dna <- function(n=6, fasta=TRUE) {
  bases <- c("A", "C", "G", "T")
  ans <- sample(bases, size=n, replace=TRUE)
  
  if(fasta){
    ans <- paste(ans, collapse = "")
    cat("Hello...")
  } else{
    cat("...is it me you are looking for")
  }
  
  return(ans)
}
```

``` r
generate_dna(10)
```

    Hello...

    [1] "TGCTGCATGG"

``` r
generate_dna(10, fasta=F)
```

    ...is it me you are looking for

     [1] "G" "C" "C" "A" "A" "C" "T" "T" "C" "G"

## A new cool function

> Q. Write a function called `generate_protein()` that generates a user
> specified length protein sequence in FASTA like format?

``` r
generate_protein <- function(n) {
  aa <- c(
    "A","C","D","E","F","G","H","I","K","L",
    "M","N","P","Q","R","S","T","V","W","Y"
  )
  ans <- sample(aa, size = n, replace = TRUE)
  return(paste(ans, collapse=""))
}
```

``` r
generate_protein(10)
```

    [1] "GLGYKYYLDA"

> Q. Use your new `generate_protein()` function to generate all
> sequences between length 6 and 12 amino acids in length and check of
> any of these are unique in nature (i.e. found in the NR database at
> NCBI)?

``` r
generate_protein(6)
```

    [1] "LLSFSH"

``` r
generate_protein(7)
```

    [1] "GPIEHAG"

``` r
generate_protein(8)
```

    [1] "CDPFMSWC"

``` r
generate_protein(9)
```

    [1] "KKSHQVECL"

``` r
generate_protein(10)
```

    [1] "RNCRVADNTA"

``` r
generate_protein(11)
```

    [1] "WQPRSCEVNRV"

``` r
generate_protein(12)
```

    [1] "IRWKNITVNKFM"

Or we could do a `for()` loop:

``` r
for(i in 6:12) {
  cat(">", i, sep="", "\n")
  cat(generate_protein(i), "\n")
}
```

    >6
    FMWMSH 
    >7
    GAKRWLL 
    >8
    CYLTYPSM 
    >9
    SNTLFHEFF 
    >10
    SHTQNNLLNK 
    >11
    WRMGCRARAWV 
    >12
    EVGDIVWITGEC 
