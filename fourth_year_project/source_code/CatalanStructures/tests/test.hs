{-# LANGUAGE OverloadedStrings #-}

module Tests where

import Test.QuickCheck

import Math.CatalanStructures as CS
import Math.DyckPath as DP
import Math.Triangulations as Tri
import Math.StackSortPerm as SSP
import Math.Av321 as AV321
import Math.Av123 as AV123
import Math.YoungTableaux as YT
import Math.Internal as Inter

check :: Testable prop => prop -> IO ()
check = quickCheck

{---------------------------------
	Dyck Path tests
---------------------------------}

dpEx :: DP.DyckPath
dpEx = [U,U,D,D,U,U,D,D]

dpTests = [test1_dp, test2_dp, test3_dp, test4_dp]

test1_dp = check (DP.mkIndec ([U,D]::DP.DyckPath) == [U,U,D,D]::DP.DyckPath)
test2_dp = check (DP.cons [U,D] [U,U,D,D] == [U,U,D,D,U,U,D,D]::DP.DyckPath)
test3_dp = check (DP.decons [U,U,D,D,U,U,D,D] == ([U,D], [U,U,D,D))
test4_dp = check (DP.heights [U,U,D,D,U,U,D,D] == [1,2,1,0,1,2,1,0])
test5_dp = check (DP.uCnt dpEx == 4)
test6_dp = check (DP.dCnt dpEx == 4)
test7_dp = check (DP.returnsXAxis dpEx == 1)
test8_dp = check (DP.heightStat dpEx == 2)
test9_dp = check (DP.noInitialRises dpEx == 2)
test10_dp = check (DP.noDoubleRises dpEx == 2)
test11_dp = check (DP.majorIndex dpEx == 2)
 
{---------------------------------
	Stack Sortable Permutation tests
---------------------------------}

sspTests = [test1_ssp, test2_ssp]

test1_ssp = check (SSP.decons $ Perm231 ([1,2,3,4,5,6]) == Just (Perm231 [1,2,3,4,5],Perm231 []))
test2_ssp = check (SSP.cons (Perm231 [1,2,3,4,5]) (Perm231 []) == Perm231 [1,2,3,4,5,6])


{---------------------------------
	Young Tableaux tests
---------------------------------}

ytTests = [test1_yt, test3_yt, test4_yt, test5_yt, test6_yt]
test1_yt = check (YT.decompose (Tableaux [[1,3,4,6,7], [2,5,8,10], [9]]) == (Just (Tableaux {tableaux = [[1,2,9],[3,5],[4,8]]},Tableaux {tableaux = [[6,10],[7]]}))

test3_yt = check (YT.hook (Tableaux [[1,3,4,6,7], [2,5,8,10], [9]]) == 3628800)
test4_yt = check (YT.dimension (Tableaux [[1,3,4,6,7], [2,5,8,10], [9]]) == 1)
test5_yt = check (YT.lambda (Tableaux [[1,3,4,6,7], [2,5,8,10], [9]]) == 10)
test6_yt = check (YT.size (Tableaux [[1,3,4,6,7], [2,5,8,10], [9]]) == 10)

{---------------------------------
	Triangulations tests
---------------------------------}

triTests = [test1_tri, test2_tri]

test1_tri = check (Tri.decompose ([(1,4,2), (2,4,3), (5,1,4), (5,1,6), (1,6,8), (8,6,7)]) == Just ([(1,4,2),(2,4,3),(5,1,4),(5,1,6)],[(8,6,7)]))
test2_tri = check(Tri.compose [(1,4,2),(2,4,3),(5,1,4),(5,1,6)] [(8,6,7)] == [(1,4,2), (2,4,3), (5,1,4), (5,1,6), (1,6,8), (8,6,7)])


{---------------------------------
	Bijections tests
---------------------------------}

bijTests = []
test1_bi = check (standard (Perm231 [7,5,6,4,2,1,3]) == [U,D,U,U,D,D,U,D,U,U,D,U,D,D])
test2_bi = check (simionSchmidt (Perm[6,7,4,3,1,2,5]) ==)
test3_bi = check (simionSchmidtInv)
tst4_bi = check (fulmek () )