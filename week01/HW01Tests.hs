-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF1 "toDigits test" toDigits
             [(1234, [1, 2, 3, 4]), (5, [5]), (0, []), (-17, [])]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [testF1 "doubleEveryOther test" doubleEveryOther
             [([8, 7, 6, 5], [16, 7, 12, 5]), ([1, 2, 3], [1, 4, 3])]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [testF1 "sumDigits test" sumDigits
             [([16, 7, 12, 5], 22)]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [testF1 "validate test" validate
             [(4012888888881881, True), (4012888888881882, False)]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = []

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
