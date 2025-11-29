module Homology (
  Vertex, Simplex(..),
  SimplicialComplex,
  Filtration,
  computePBoundaryMatrix
) where

import Data.List
import Data.Matrix (Matrix, toLists, fromList, fromLists, (<|>), (<->),
                   ncols, nrows, zero, getElem, mapCol, mapRow)
import Data.Vector (Vector(..), toList)
import Data.Maybe (Maybe(..), fromJust)
import Test.QuickCheck

type Vertex = Int
type Simplex = [Vertex]

-- Represent the boundary operator as a matrix
type BoundaryMatrix = [[Int]]
type SimplicialComplex = [Simplex]
type Filtration = [SimplicialComplex]
type VertexFunction = [(Vertex, Int)]

-- Determines if a given collection of sets is a simplicial complex
isSimplicialComplex :: SimplicialComplex -> Bool
isSimplicialComplex c = all areAllFacesContained c
  where areAllFacesContained simplex | length simplex == 1  = True  -- Nothing to check for vertices
                                     | otherwise            = all (`elem` c) (facesOfSimplex simplex)
  

-- Given a simplicial complex K and a dimension n, returns all simplices of dimension n in K
nDimSimplices :: SimplicialComplex -> Int -> [Simplex]
nDimSimplices complex n = filter (\simplex -> length simplex == n + 1) complex

-- Given a simplicial complex K, returns the simplex-wise filtration in which all simplices of K
-- are added in increasing order of dimension, and for equal dimensions in lexicographic order
orderedSimplices :: SimplicialComplex -> [Simplex]
orderedSimplices complex = concat (map (nDimSimplices complex) [0 .. dim])
  where dim = dimOfSimplicialComplex complex

-- Finds the dimension of a simplicial complex (i.e. the maximum dimension of a simplex)
dimOfSimplicialComplex :: SimplicialComplex -> Int
dimOfSimplicialComplex complex = (foldr max 0 (map length complex)) - 1

-- Given a simplex, returns a list of its facets (faces of dimension (dim simplex - 1)) 
facesOfSimplex :: Simplex -> [[Int]]
facesOfSimplex []      = []
facesOfSimplex (x:xs) = xs : map (x :) (facesOfSimplex xs)


-- Computes the matrix D associated to the boundary map delta_p : C_p -> C_{p-1},
-- given a simplicial complex and a dimension p
computePBoundaryMatrix :: SimplicialComplex -> Int -> Matrix Int
computePBoundaryMatrix complex 0 = emptyMatrix
computePBoundaryMatrix complex p = fromLists (map (map fromEnum) (computeBoundaryMatrixBool complex p))
  where 
    computeBoundaryMatrixBool complex p = map computeBoundaryMatrixRow pMinusOneDimSimplices
    computeBoundaryMatrixRow pMinusOneDimSimplex = map (elem pMinusOneDimSimplex) facesOfDimPSimplices

    pDimSimplices         = nDimSimplices complex p 
    pMinusOneDimSimplices = nDimSimplices complex (p - 1)
    facesOfDimPSimplices  = map facesOfSimplex pDimSimplices

-- Compute the boundary matrix associated to the boundary operator (the collection of all delta_p)
computeBoundaryMatrix :: SimplicialComplex -> Matrix Int
computeBoundaryMatrix complex = (zeroPaddingLeft <|> (nDirectSum pBdMatrices)) <-> zeroPaddingBottom
  where n = dimOfSimplicialComplex complex
        orderedSimplicesList = orderedSimplices complex
        pBdMatrices = map (computePBoundaryMatrix complex) [0..n]

        totalNrSimplices = Prelude.length orderedSimplicesList
        nrVertices = Prelude.length (nDimSimplices complex 0)
        nrMaxSimplices = Prelude.length (nDimSimplices complex n)

        zeroPaddingLeft = zero (totalNrSimplices-nrMaxSimplices) nrVertices
        zeroPaddingBottom = zero nrMaxSimplices totalNrSimplices

directSum :: Matrix Int -> Matrix Int -> Matrix Int
directSum x y = (x <|> zero (nrows x) (ncols y))
               <-> (zero (nrows y) (ncols x) <|> y)

emptyMatrix :: Matrix Int
emptyMatrix = Data.Matrix.fromList 0 0 []

nDirectSum :: [Matrix Int] -> Matrix Int
nDirectSum xs = Prelude.foldr1 directSum xs
          

lowOfCol :: Int -> Matrix Int -> Int
lowOfCol j matr = maximum ([-1] ++ map snd (filter (\(a, b) -> fst (a, b) /= 0) (zip col [1, 2..])))
  where col = (transpose (toLists matr)) !! (j - 1)

lows :: Matrix Int -> [Int]
lows matr = map (\col -> lowOfCol col matr) [1 .. (length matr')]
  where matr' = toLists matr

-- Add column j' to column j in a given matrix (note: columns are indexed from 1)
addColumn :: Int -> Int -> Matrix Int -> Matrix Int
addColumn j j' matr = mapCol (\r val -> val + getElem r j' matr) j matr


-- Given a simplex and the current list of unpaired positive simplices, find a compatible pair for the
-- negative simple or add it to the list of positive unpaired simplices
pairSimplex :: Int -> [Int] -> Matrix Int -> (Maybe Int, [Int])
pairSimplex s posSimpl matr | candidate `elem` posSimpl = (Just candidate, delete candidate posSimpl)
                            | otherwise  = (Nothing, posSimpl ++ [s])
  where candidate = lowOfCol s matr


makeRowZero :: Int -> Matrix Int -> Matrix Int
makeRowZero r matr = mapRow (\_ r -> 0) r matr

leftSimplify :: Int -> [Int] -> Maybe Int
leftSimplify j lowMatr | lowMatr !! j == -1 = Nothing
                       | null candidates    = Nothing
                       | otherwise          = Just (head candidates)
  where candidates = filter (\j' -> lowMatr !! j' == lowMatr !! j) [1..(j-1)]


pairingAlgorithm :: SimplicialComplex -> [(Int, Int)]
pairingAlgorithm c = pairRec (length c) matr [] []
  where matr = computeBoundaryMatrix c

infinity = 2^30

pairRec :: Int -> Matrix Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
pairRec 0 matr positiveSimplList pairings = pairings ++ map (\s -> (s, infinity)) positiveSimplList 
pairRec j matr positiveSimplList pairings
  | chosenPosSimpl == Nothing  = pairRec (j - 1) matr positiveSimplList' pairings
  | otherwise                  = pairRec (j - 1) (makeRowZero (fromJust chosenPosSimpl) matr)
                                     positiveSimplList' (pairings ++ [(fromJust chosenPosSimpl, i)])
  where (chosenPosSimpl, positiveSimplList') = pairSimplex i positiveSimplList matr
        i = nrows matr - j + 1

-- Tests
c = [[1], [2], [3], [1, 2], [1, 3], [2, 3]]
d = [[1], [2], [3], [4], [1, 2], [1, 3], [2, 3], [2, 4], [3, 4], [1, 2, 3], [2, 3, 4]]


