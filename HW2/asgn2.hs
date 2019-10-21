-- Jenhifer Salas 914329903
-- ECS 140B Assignment 2

-- PROBLEM 1
-- Takes two integers and returns their product using
-- sequences of addition non tail recursive
multiply :: Int -> Int -> Int
multiply fact1 fact2
    | fact1 == 0    = 0
    | fact2 == 0    = 0
    | otherwise     = fact1 + multiply fact1 (fact2 - 1)

-- PROBLEM 2
-- Takes two integers and returns their product using
-- sequences of addition with tail recursion
multiply_tr :: Int -> Int -> Int
multiply_tr fact1 fact2
    | fact1 == 0    = 0
    | fact2 == 0    = 0
    | otherwise     = multiply_tr_helper 0 fact1 fact2

multiply_tr_helper :: Int -> Int -> Int -> Int
multiply_tr_helper sum fact1 fact2
    | fact1 == 0    = sum
    | fact2 == 0    = sum
    | otherwise     = multiply_tr_helper (sum + fact1) fact1 (fact2 - 1)

-- PROBLEM 3
-- Takes two integers, a base and a power and returns
-- the the result using non-tail recursion
power :: Int -> Int -> Int
power base pow
    | pow == 0      = 1
    | otherwise     = multiply base (power base (pow - 1))

-- PROBLEM 4
-- Takes two integers, a base and a power, and returns
-- the result using tail recursion.
power_tr :: Int -> Int -> Int
power_tr base pow
    | pow == 0      = 1
    | otherwise     = power_tr_helper base base (pow-1)

power_tr_helper :: Int -> Int -> Int -> Int
power_tr_helper total base pow
    | pow == 0      = total
    | otherwise     = power_tr_helper (multiply_tr total base) base (pow-1)

-- PROBLEM 5
-- Takes an integer n and returns the nth harmonic sum
-- using non tail recursion.
harmonic :: Int -> Float
harmonic n
    | n == 1        = 1
    | otherwise     = (1 / fromIntegral(n)) + harmonic (n-1)

-- PROBLEM 6
-- Takes an integer n and returns teh nth harmonic sum
-- using tail recursion.
harmonic_tr :: Int -> Float
harmonic_tr n
    | n == 1        = 1
    | otherwise     = harmonic_tr_helper 0.0 (n)

harmonic_tr_helper ::  Float -> Int -> Float
harmonic_tr_helper total n
    | n == 1        = 1 + total
    | otherwise     = harmonic_tr_helper (total + (1.0 / fromIntegral(n))) (n - 1)
