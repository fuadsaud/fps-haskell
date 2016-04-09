import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import LinEqSys

main :: IO ()
main = hspec $ do
    describe "LinEqSys.sub" $ do
        it "subtracs two equations" $ do
            sub [2,2,3] [2,5,12] `shouldBe` [-0,-3,-9]

    describe "LinEqSys.scaleList" $ do
        it "multiplies all elements of a list by the given constant" $ do
            scaleList (1/2) [2,5,12] `shouldBe` [1.0,2.5,6.0]

    describe "LinEqSys.subScale" $ do
        it "scales the first list so that when it is subtracted from the second, the first number in the result is zero. Since it must be zero, return a list without that first constant." $ do
            subScale [2,2,3,10] [2,5,12,31] `shouldBe` [3.0,9.0,21.0]
            subScale [2,3,3,8] [4,-2,2,4] `shouldBe` [-8.0,-4.0,-12.0]

    describe "LinEqSys.nonZeroFirst" $ do
        it "signals error if all the lists start with a zero" $ do
            evaluate (nonZeroFirst [[0, 1, 2],[0, 2, 3]])`shouldThrow` anyException
        it "puts the first list that does not start with a zero at the beginning of the list" $ do
            nonZeroFirst [[0,-5,-5],[-8,-4,-12]] `shouldBe` [[-8,-4,-12],[0,-5,-5]]

    describe "LinEqSys.dot" $ do
        it "computes the dot product of two lists" $ do
            dot [1,2] [3,4] `shouldBe` 11

    describe "LinEqSys.triangulate" $ do
        it "takes a list of lists representing a system of linear equations, and returns a triangular list representing the same system" $ do
            triangulate [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]] `shouldBe` [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]

    describe "LinEqSys.solveLine" $ do
        it "takes a list representing an equation, and a list representing the values for all the variables but the first, and returns the solution for that equation" $ do
            solveLine [2,3,3,8] [1,1] `shouldBe` 1.0

    describe "LinEqSys.solveTriangular" $ do
        it "takes a triangular list of lists and returns the list of solutions for the system" $ do
            solveTriangular [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]] `shouldBe` [1.0,1.0,1.0]

    describe "LinEqSys.solveSystem" $ do
        it " takes a rectangular lists of lists representing a system of equations and returns the list of solutions for the system" $ do
            solveSystem [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]] `shouldBe` [1.0,1.0,1.0]
