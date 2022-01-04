## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(2134)

## ----simple_recurse-----------------------------------------------------------
print_numbers <- function(n) {
  if(n >= 1) {
    print_numbers(n - 1)
    print(n)
  }
}

## ----recurse1-----------------------------------------------------------------
print_numbers(5)

## ----blow_up, error=TRUE------------------------------------------------------
print_numbers(10000)

## ----setup--------------------------------------------------------------------
library(trampoline)
## generator version

print_numbers <- coro::generator(function(n) {
  if(n >= 1) {
    yield(print_numbers(n - 1))
    print(n)
  }
})

catch <- capture.output(trampoline(print_numbers(10000))) ## capture output to prevent flooding document with numbers
head(catch)
tail(catch)

## ----plain_function-----------------------------------------------------------
print_numbers <- function(n) {
  if(n >= 1) {
    yield(print_numbers(n - 1))
    print(n)
  }
}

trampoline(print_numbers(5))

## ----factorial----------------------------------------------------------------
factorial <- function(n) {
  if(n <= 1) {
    return(1)
  }
  val <- factorial(n - 1)
  return(val * n)
}

## ----do_recursion, error=TRUE-------------------------------------------------
factorial(5000)

## ----factorial2, error=TRUE---------------------------------------------------
factorial1 <- function(n) {
  if(n <= 1) {
    return(trm_return(1))
  }
  val <- yield(factorial1(n - 1))
  return(val * n)
}

trampoline(factorial1(5000))

## ----factorial3---------------------------------------------------------------
factorial2 <- function(n, x = 1) {
  force(x) ## necessary thanks to R's lazy evaluation
  if(n <= 1) {
    return(trm_return(x))
  }
  val <- trm_tailcall(factorial2(n - 1, x * n))
  return(val)
}

trampoline(factorial2(5000))

## ----compare1-----------------------------------------------------------------

factorial(10)
trampoline(factorial1(10))
trampoline(factorial2(10))


## ----benchmark----------------------------------------------------------------
bench_res <- bench::mark(trampoline(factorial1(1000)), 
                         trampoline(factorial2(1000)), 
                         trampoline(factorial1(2000)), 
                         trampoline(factorial2(2000)), 
                         trampoline(factorial1(5000)), 
                         trampoline(factorial2(5000)),
                         trampoline(factorial1(10000)), 
                         trampoline(factorial2(10000)),
                         check = FALSE, iterations = 3)

bench_res

if(all(!is.na(as.numeric(bench_res$mem_alloc)))) {
  plot(as.numeric(bench_res$mem_alloc)[c(TRUE, FALSE)] ~ c(1000, 2000, 5000, 10000), type = "l", col = "red", 
       xlab = "n", ylab = "Bytes Allocated")
  points(as.numeric(bench_res$mem_alloc)[c(FALSE, TRUE)] ~ c(1000, 2000, 5000, 10000), type = "l", col = "blue")
  legend("right", legend = c("No Tail Call", "Tail Call"), col = c("red", "blue"), lty = 1)
}

## ----benchmark2---------------------------------------------------------------
if(all(!is.na(as.numeric(bench_res$total_time)))) {
  plot(as.numeric(bench_res$total_time)[c(TRUE, FALSE)] ~ c(1000, 2000, 5000, 10000), type = "l", col = "red", 
       xlab = "n", ylab = "Total Time")
  points(as.numeric(bench_res$total_time)[c(FALSE, TRUE)] ~ c(1000, 2000, 5000, 10000), type = "l", col = "blue")
  legend("right", legend = c("No Tail Call", "Tail Call"), col = c("red", "blue"), lty = 1)
}

## ----even_odd-----------------------------------------------------------------
even <- coro::generator(function(n) {
  if (n == 0) trm_return(TRUE) else yield(odd(n - 1))
})

odd <- coro::generator(function(n) {
  if (n == 0) trm_return(FALSE) else yield(even(n - 1))
})

trampoline(even(10000))
trampoline(even(10001))

trampoline(odd(10000))
trampoline(odd(10001))


## ----pass_named_args, error=TRUE----------------------------------------------
even <- function(n) {
  if (n == 0) trm_return(TRUE) else yield(odd(n - 1))
}

odd <- function(n) {
  if (n == 0) trm_return(FALSE) else yield(even(n - 1))
}

## doesn't work
trampoline(even(10000))

## does work
trampoline(even(10000), odd = odd)


