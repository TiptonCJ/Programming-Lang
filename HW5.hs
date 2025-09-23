data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
    deriving (Eq,Show)

data MTree a = MLeaf a | UNode a (MTree a) | BNode (MTree a) (MTree a)
    deriving (Eq,Show)

getLLeaves :: LTree a -> [a]
getLLeaves (LLeaf x) = [x]
getLLeaves (LNode _ l r) = getLLeaves l ++ getLLeaves r

getMLeaves :: MTree a -> [a]
getMLeaves (MLeaf x) = [x]
getMLeaves (UNode _ t) = getMLeaves t
getMLeaves (BNode l r) = getMLeaves l ++ getMLeaves r

maxLDepth :: LTree a -> Integer
maxLDepth (LLeaf x) = 0
maxLDepth (LNode _ l r) = 1 + max (maxLDepth l) (maxLDepth r)

maxMDepth :: MTree a -> Integer
maxMDepth (MLeaf x) = 0
maxMDepth (UNode _ t) = 1 + maxMDepth t
maxMDepth (BNode l r) = 1 + max (maxMDepth l) (maxMDepth r)

maxLTree :: LTree Integer -> Integer
maxLTree (LLeaf x) = x
maxLTree (LNode _ l r) = max (maxLTree l) (maxLTree r)

maxMTree :: MTree Integer -> Integer
maxMTree (MLeaf x) = x
maxMTree (UNode _ t) = maxMTree t
maxMTree (BNode l r) = max (maxMTree l) (maxMTree r)

uncoveredLeafL :: Integer -> LTree Integer -> Bool
uncoveredLeafL



















exLTree :: LTree Integer
exLTree = LNode 5 (LLeaf 4)
                  (LNode 3 (LNode 2 (LLeaf 5) (LLeaf 1))
                           (LLeaf 7))
exMTree :: MTree Integer
exMTree = BNode (UNode 5 (BNode (MLeaf 1) (MLeaf 10)))
                (BNode (UNode 3 (MLeaf 3)) (UNode 4 (MLeaf 3)))

--testing

-- lTree1 :: LTree Integer
-- lTree1 = LLeaf 0

-- mTree1 :: MTree Integer
-- mTree1 = MLeaf 0

-- lTree2 :: LTree String
-- lTree2 = LNode "discno" (LLeaf "discno") (LLeaf "andfdna")

-- mTree2 :: MTree String
-- mTree2 = UNode "bounce" (BNode (MLeaf "andfdna") (MLeaf "bounce"))

-- lTree3 :: LTree (String, Integer)
-- lTree3 = LNode ("hello", 3) (LLeaf ("jello", 10)) (LLeaf ("hello", 3))

-- mTree3 :: MTree (String, Integer)
-- mTree3 = BNode (MLeaf ("hello", 3)) (MLeaf ("jello", 10))

-- lTree4 :: LTree Integer
-- lTree4 = LNode 1
--                (LNode 2
--                       (LNode 3 (LLeaf 4)
--                                (LLeaf 5))
--                       (LNode 3
--                              (LNode 6
--                                     (LNode 7
--                                            (LLeaf 8)
--                                            (LLeaf 1))
--                                     (LLeaf 7))
--                              (LLeaf 6)))
--                (LLeaf 2)

-- mTree4 :: MTree Integer
-- mTree4 = UNode 1
--                (BNode (UNode 2
--                              (BNode (MLeaf 1)
--                                     (BNode (UNode 2
--                                                   (MLeaf 2))
--                                            (UNode 3
--                                                   (MLeaf 4)))))
--                       (UNode 2
--                              (MLeaf 3)))

-- lTree5 :: LTree String
-- lTree5 = LNode "Djiwj"
--                 (LNode "AbbabbA"
--                        (LLeaf "Djiwj")
--                        (LLeaf "Djiwj"))
--                 (LLeaf "Djiwj")

-- mTree5 :: MTree String
-- mTree5 = UNode "anodwaun"
--                (UNode "Abbba"
--                     (BNode (MLeaf "aad")
--                             (MLeaf "dwaino)")))

-- lTree6 :: LTree String
-- lTree6 = LNode "Djiwj"
--                 (LNode "Djiwjd"
--                        (LLeaf "AbbabbA")
--                        (LLeaf "Djiwj"))
--                 (LLeaf "Djiwj")

-- mTree6 :: MTree String
-- mTree6 = UNode "dwaino"
--                (UNode "dwaino"
--                     (BNode (MLeaf "Abbba")
--                            (MLeaf "dwaino)")))

-- lTree7 :: LTree Integer
-- lTree7 = LNode 10
--                 (LNode  20
--                        (LLeaf 20)
--                        (LLeaf 40))
--                 (LLeaf 20)

-- mTree7 :: MTree Integer
-- mTree7 = BNode (UNode 10
--                       (UNode 20
--                              (BNode (MLeaf 40)
--                                     (MLeaf 20))))
--                (MLeaf 20)

-- lTree8 :: LTree Integer
-- lTree8 = LNode 20
--                 (LNode 40
--                        (LLeaf 12)
--                        (LLeaf 34))
--                 (LLeaf 20)

-- mTree8 :: MTree Integer
-- mTree8 = UNode 10
--                (UNode 40
--                     (BNode (MLeaf 10)
--                            (MLeaf 12)))

-- lTree9 :: LTree Integer
-- lTree9 = LNode 9
--                (LLeaf 10)
--                (LNode 9
--                       (LLeaf 10)
--                       (LNode 9
--                              (LNode 9
--                                     (LLeaf 10)
--                                     (LNode 9
--                                            (LLeaf 9)
--                                            (LLeaf 12)))
--                              (LLeaf 10)))

-- mTree9 :: MTree Integer
-- mTree9 = UNode 9
--                (BNode (MLeaf 10)
--                       (UNode 9
--                              (BNode (MLeaf 10)
--                                     (UNode 9
--                                            (BNode (UNode 9
--                                                          (BNode (MLeaf 10)
--                                                          (UNode 9
--                                                                 (BNode (MLeaf 9)
--                                                                        (MLeaf 12)))))
--                                                   (MLeaf 10))))))

-- lTree10 :: LTree String
-- lTree10 = LNode "badinu"
--                 (LLeaf "skqdko")
--                 (LNode " adnoiw"
--                        (LNode "Daonwi"
--                               (LLeaf "daw")
--                               (LLeaf "DAW"))
--                        (LNode "DAW"
--                               (LLeaf "DWAIO")
--                               (LLeaf "DWOn")))

-- mTree10 :: MTree String
-- mTree10 = UNode "badinu"
--                 (BNode (MLeaf "skqdko")
--                        (UNode " adnoiw"
--                               (BNode (UNode "Daonwi"
--                                             (BNode (MLeaf "daw")
--                                                    (MLeaf "DAW")))
--                                      (UNode "DAW"
--                                             (BNode (MLeaf "DWAIO")
--                                                    (MLeaf "DWOn"))))))

-- lTree11 :: LTree String
-- lTree11 = LLeaf "DmioimD"

-- mTree11 :: MTree String
-- mTree11 = MLeaf "dad"

-- tests =
--   [ ((getLLeaves lTree1) == [0])
--   , ((getLLeaves lTree2) == ["discno","andfdna"])
--   , ((getLLeaves lTree3) == [("jello",10),("hello",3)])
--   , ((getLLeaves lTree4) == [4,5,8,1,7,6,2])
--   , ((getLLeaves lTree5) == ["Djiwj","Djiwj","Djiwj"])
--   , ((getMLeaves mTree1) == [0])
--   , ((getMLeaves mTree2) == ["andfdna","bounce"])
--   , ((getMLeaves mTree3) == [("hello",3),("jello",10)])
--   , ((getMLeaves mTree4) == [1,2,4,3])
--   , ((getMLeaves mTree5) == ["aad","dwaino)"])
--   , ((maxLDepth lTree1) == 0)
--   , ((maxLDepth lTree4) == 5)
--   , ((maxLDepth lTree7) == 2)
--   , ((maxLDepth lTree8) == 2)
--   , ((maxLDepth lTree9) == 5)
--   , ((maxMDepth mTree1) == 0)
--   , ((maxMDepth mTree4) == 6)
--   , ((maxMDepth mTree7) == 4)
--   , ((maxMDepth mTree8) == 3)
--   , ((maxMDepth mTree9) == 10)
--   , ((maxLTree lTree1) == 0)
--   , ((maxLTree lTree4) == 8)
--   , ((maxLTree lTree7) == 40)
--   , ((maxLTree lTree8) == 40)
--   , ((maxLTree lTree9) == 12)
--   , ((maxMTree mTree1) == 0)
--   , ((maxMTree mTree4) == 4)
--   , ((maxMTree mTree7) == 40)
--   , ((maxMTree mTree8) == 40)
--   , ((maxMTree mTree9) == 12)
--   , ((uncoveredLeafL 20 lTree7) == True)
--   , ((uncoveredLeafL 40 lTree7) == True)
--   , ((uncoveredLeafL 20 lTree8) == False)
--   , ((uncoveredLeafL 40 lTree8) == False)
--   , ((uncoveredLeafL 34 lTree8) == True)
--   , ((uncoveredLeafM 20 mTree7) == True)
--   , ((uncoveredLeafM 40 mTree7) == True)
--   , ((uncoveredLeafM 20 mTree8) == False)
--   , ((uncoveredLeafM 40 mTree8) == False)
--   , ((uncoveredLeafM 34 mTree8) == False)
--   , ((mapLTree (\x -> x * x) lTree9) == LNode 81 (LLeaf 100) (LNode 81 (LLeaf 100) (LNode 81 (LNode 81 (LLeaf 100) (LNode 81 (LLeaf 81) (LLeaf 144))) (LLeaf 100))))
--   , ((mapLTree (\x -> x * x) lTree4) == LNode 1 (LNode 4 (LNode 9 (LLeaf 16) (LLeaf 25)) (LNode 9 (LNode 36 (LNode 49 (LLeaf 64) (LLeaf 1)) (LLeaf 49)) (LLeaf 36))) (LLeaf 4))
--   , ((mapLTree (\x -> reverse x == x) lTree5) == LNode False (LNode True (LLeaf False) (LLeaf False)) (LLeaf False))
--   , ((mapLTree (\x -> (fst x == "jello")) lTree3) == LNode False (LLeaf True) (LLeaf False))
--   , ((mapLTree (\x -> x > 20) lTree7 ) == LNode False (LNode False (LLeaf False) (LLeaf True)) (LLeaf False))
--   , ((mapMTree (\x -> x * x) mTree9) == UNode 81 (BNode (MLeaf 100) (UNode 81 (BNode (MLeaf 100) (UNode 81 (BNode (UNode 81 (BNode (MLeaf 100) (UNode 81 (BNode (MLeaf 81) (MLeaf 144))))) (MLeaf 100)))))))
--   , ((mapMTree (\x -> x * x) mTree4) == UNode 1 (BNode (UNode 4 (BNode (MLeaf 1) (BNode (UNode 4 (MLeaf 4)) (UNode 9 (MLeaf 16))))) (UNode 4 (MLeaf 9))))
--   , ((mapMTree (\x -> reverse x == x) mTree5) == UNode False (UNode False (BNode (MLeaf False) (MLeaf False))))
--   , ((mapMTree (\x -> (fst x == "jello")) mTree3) == BNode (MLeaf False) (MLeaf True))
--   , ((mapMTree (\x -> x > 20) mTree7) == BNode (UNode False (UNode False (BNode (MLeaf True) (MLeaf False)))) (MLeaf False))
--   , ((applyLfun lTree1) == LLeaf 1)
--   , ((applyLfun lTree4) == LNode 1 (LNode 14 (LNode 509 (LLeaf 65532) (LLeaf 33554427)) (LNode 509 (LNode 68719476730 (LNode 562949953421305 (LLeaf 18446744073709551608) (LLeaf 1)) (LLeaf 562949953421305)) (LLeaf 68719476730))) (LLeaf 14))
--   , ((applyLfun lTree7) == LNode 1267650600228229401496703205366 (LNode 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356 (LLeaf 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356) (LLeaf 44462416477094044620016814065517364315819234512137839319418223093753683069769152238984782576173969417485953521141049383745107056455283979316385016701612810119562585078620415976730705698345087039035930761275083827265405596065418173652685035788898113991627042329246850314029877161622487411877779578892097029690461532001915311366862468942148892205997883828265721290296220249202674740669814705818564765009960300389641843321936008416473775144511929246788246559538970957296160626364645336)) (LLeaf 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356))
--   , ((applyLfun lTree8) == LNode 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356 (LNode 44462416477094044620016814065517364315819234512137839319418223093753683069769152238984782576173969417485953521141049383745107056455283979316385016701612810119562585078620415976730705698345087039035930761275083827265405596065418173652685035788898113991627042329246850314029877161622487411877779578892097029690461532001915311366862468942148892205997883828265721290296220249202674740669814705818564765009960300389641843321936008416473775144511929246788246559538970957296160626364645336 (LLeaf 22300745198530623141535718272648361505980404) (LLeaf 978757239885553111552518299507291628946173019895052784803126361134715164737471333148589823010527548677053512109976144744711344133680578410405337870243370010977031567543689952849756531152255270393792844994269246753409884449197339177424770114500800365518397471517996221945996841656250431969808731337118835799400739730701352692234849894550672754343902)) (LLeaf 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356))
--   , ((applyLfun lTree9) == LNode 2417851639229258349412343 (LLeaf 1267650600228229401496703205366) (LNode 2417851639229258349412343 (LLeaf 1267650600228229401496703205366) (LNode 2417851639229258349412343 (LNode 2417851639229258349412343 (LLeaf 1267650600228229401496703205366) (LNode 2417851639229258349412343 (LLeaf 2417851639229258349412343) (LLeaf 22300745198530623141535718272648361505980404))) (LLeaf 1267650600228229401496703205366))))
--   , ((applyMfun mTree1) == MLeaf 1)
--   , ((applyMfun mTree4) == UNode 1 (BNode (UNode 14 (BNode (MLeaf 1) (BNode (UNode 14 (MLeaf 14)) (UNode 509 (MLeaf 65532))))) (UNode 14 (MLeaf 509))))
--   , ((applyMfun mTree7) == BNode (UNode 1267650600228229401496703205366 (UNode 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356 (BNode (MLeaf 44462416477094044620016814065517364315819234512137839319418223093753683069769152238984782576173969417485953521141049383745107056455283979316385016701612810119562585078620415976730705698345087039035930761275083827265405596065418173652685035788898113991627042329246850314029877161622487411877779578892097029690461532001915311366862468942148892205997883828265721290296220249202674740669814705818564765009960300389641843321936008416473775144511929246788246559538970957296160626364645336) (MLeaf 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356)))) (MLeaf 2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493356))
--   , ((applyMfun mTree8) == UNode 1267650600228229401496703205366 (UNode 44462416477094044620016814065517364315819234512137839319418223093753683069769152238984782576173969417485953521141049383745107056455283979316385016701612810119562585078620415976730705698345087039035930761275083827265405596065418173652685035788898113991627042329246850314029877161622487411877779578892097029690461532001915311366862468942148892205997883828265721290296220249202674740669814705818564765009960300389641843321936008416473775144511929246788246559538970957296160626364645336 (BNode (MLeaf 1267650600228229401496703205366) (MLeaf 22300745198530623141535718272648361505980404))))
--   , ((applyMfun mTree9) == UNode 2417851639229258349412343 (BNode (MLeaf 1267650600228229401496703205366) (UNode 2417851639229258349412343 (BNode (MLeaf 1267650600228229401496703205366) (UNode 2417851639229258349412343 (BNode (UNode 2417851639229258349412343 (BNode (MLeaf 1267650600228229401496703205366) (UNode 2417851639229258349412343 (BNode (MLeaf 2417851639229258349412343) (MLeaf 22300745198530623141535718272648361505980404))))) (MLeaf 1267650600228229401496703205366)))))))
--   , ((findLTree (\x -> reverse x == x) lTree5) == Just "AbbabbA")
--   , ((findLTree (\x -> (fst x == "jello")) lTree3) == Just ("jello",10))
--   , ((findLTree (\x -> x > 20) lTree7) == Just 40)
--   , ((findLTree (\x -> x == 12) lTree9) == Just 12)
--   , ((findLTree (\x -> x == 3) lTree4) == Just 3)
--   , ((findMTree (\x -> reverse x == x) mTree5) == Nothing)
--   , ((findMTree (\x -> (fst x == "jello")) mTree3) == Just ("jello",10))
--   , ((findMTree (\x -> x > 20) mTree7) == Just 40)
--   , ((findMTree (\x -> x == 12) mTree9) == Just 12)
--   , ((findMTree (\x -> x == 3) mTree4) == Just 3)
--   , ((findLpali lTree2) == Just "andfdna")
--   , ((findLpali lTree5) == Just "AbbabbA")
--   , ((findLpali lTree6) == Just "AbbabbA")
--   , ((findLpali lTree10) == Nothing)
--   , ((findLpali lTree11) == Just "DmioimD")
--   , ((findMpali mTree2) == Just "andfdna")
--   , ((findMpali mTree5) == Nothing)
--   , ((findMpali mTree6) == Nothing)
--   , ((findMpali mTree10) == Nothing)
--   , ((findMpali mTree11) == Just "dad")
--   , ((getLLeaves' lTree1) == [0])
--   , ((getLLeaves' lTree2) == ["discno","andfdna"])
--   , ((getLLeaves' lTree3) == [("jello",10),("hello",3)])
--   , ((getLLeaves' lTree4) == [4,5,8,1,7,6,2])
--   , ((getLLeaves' lTree5) == ["Djiwj","Djiwj","Djiwj"])
--   , ((getMLeaves' mTree1) == [0])
--   , ((getMLeaves' mTree2) == ["andfdna","bounce"])
--   , ((getMLeaves' mTree3) == [("hello",3),("jello",10)])
--   , ((getMLeaves' mTree4) == [1,2,4,3])
--   , ((getMLeaves' mTree5) == ["aad","dwaino)"])
--   , ((uncoveredLeafL' 20 lTree7) == True)
--   , ((uncoveredLeafL' 40 lTree7) == True)
--   , ((uncoveredLeafL' 20 lTree8) == False)
--   , ((uncoveredLeafL' 40 lTree8) == False)
--   , ((uncoveredLeafL' 34 lTree8) == True)
--   , ((uncoveredLeafM' 20 mTree7) == True)
--   , ((uncoveredLeafM' 40 mTree7) == True)
--   , ((uncoveredLeafM' 20 mTree8) == False)
--   , ((uncoveredLeafM' 40 mTree8) == False)
--   , ((uncoveredLeafM' 34 mTree8) == False)
--   ]
-- testLinesString =
--   [ "(getLLeaves lTree1)"
--   , "(getLLeaves lTree2)"
--   , "(getLLeaves lTree3)"
--   , "(getLLeaves lTree4)"
--   , "(getLLeaves lTree5)"
--   , "(getMLeaves mTree1)"
--   , "(getMLeaves mTree2)"
--   , "(getMLeaves mTree3)"
--   , "(getMLeaves mTree4)"
--   , "(getMLeaves mTree5)"
--   , "(maxLDepth lTree1)"
--   , "(maxLDepth lTree4)"
--   , "(maxLDepth lTree7)"
--   , "(maxLDepth lTree8)"
--   , "(maxLDepth lTree9)"
--   , "(maxMDepth mTree1)"
--   , "(maxMDepth mTree4)"
--   , "(maxMDepth mTree7)"
--   , "(maxMDepth mTree8)"
--   , "(maxMDepth mTree9)"
--   , "(maxLTree lTree1)"
--   , "(maxLTree lTree4)"
--   , "(maxLTree lTree7)"
--   , "(maxLTree lTree8)"
--   , "(maxLTree lTree9)"
--   , "(maxMTree mTree1)"
--   , "(maxMTree mTree4)"
--   , "(maxMTree mTree7)"
--   , "(maxMTree mTree8)"
--   , "(maxMTree mTree9)"
--   , "(uncoveredLeafL 20 lTree7)"
--   , "(uncoveredLeafL 40 lTree7)"
--   , "(uncoveredLeafL 20 lTree8)"
--   , "(uncoveredLeafL 40 lTree8)"
--   , "(uncoveredLeafL 34 lTree8)"
--   , "(uncoveredLeafM 20 mTree7)"
--   , "(uncoveredLeafM 40 mTree7)"
--   , "(uncoveredLeafM 20 mTree8)"
--   , "(uncoveredLeafM 40 mTree8)"
--   , "(uncoveredLeafM 34 mTree8)"
--   , "(mapLTree (\\x -> x * x) lTree9)"
--   , "(mapLTree (\\x -> x * x) lTree4)"
--   , "(mapLTree (\\x -> reverse x == x) lTree5)"
--   , "(mapLTree (\\x -> (fst x == \"jello\")) lTree3)"
--   , "(mapLTree (\\x -> x > 20) lTree7 )"
--   , "(mapMTree (\\x -> x * x) mTree9)"
--   , "(mapMTree (\\x -> x * x) mTree4)"
--   , "(mapMTree (\\x -> reverse x == x) mTree5)"
--   , "(mapMTree (\\x -> (fst x == \"jello\")) mTree3)"
--   , "(mapMTree (\\x -> x > 20) mTree7)"
--   , "(applyLfun lTree1)"
--   , "(applyLfun lTree4)"
--   , "(applyLfun lTree7)"
--   , "(applyLfun lTree8)"
--   , "(applyLfun lTree9)"
--   , "(applyMfun mTree1)"
--   , "(applyMfun mTree4)"
--   , "(applyMfun mTree7)"
--   , "(applyMfun mTree8)"
--   , "(applyMfun mTree9)"
--   , "(findLTree (\\x -> reverse x == x) lTree5)"
--   , "(findLTree (\\x -> (fst x == \"jello\")) lTree3)"
--   , "(findLTree (\\x -> x > 20) lTree7)"
--   , "(findLTree (\\x -> x == 12) lTree9)"
--   , "(findLTree (\\x -> x == 3) lTree4)"
--   , "(findMTree (\\x -> reverse x == x) mTree5)"
--   , "(findMTree (\\x -> (fst x == \"jello\")) mTree3)"
--   , "(findMTree (\\x -> x > 20) mTree7)"
--   , "(findMTree (\\x -> x == 12) mTree9)"
--   , "(findMTree (\\x -> x == 3) mTree4)"
--   , "(findLpali lTree2)"
--   , "(findLpali lTree5)"
--   , "(findLpali lTree6)"
--   , "(findLpali lTree10)"
--   , "(findLpali lTree11)"
--   , "(findMpali mTree2)"
--   , "(findMpali mTree5)"
--   , "(findMpali mTree6)"
--   , "(findMpali mTree10)"
--   , "(findMpali mTree11)"
--   , "(getLLeaves' lTree1)"
--   , "(getLLeaves' lTree2)"
--   , "(getLLeaves' lTree3)"
--   , "(getLLeaves' lTree4)"
--   , "(getLLeaves' lTree5)"
--   , "(getMLeaves' mTree1)"
--   , "(getMLeaves' mTree2)"
--   , "(getMLeaves' mTree3)"
--   , "(getMLeaves' mTree4)"
--   , "(getMLeaves' mTree5)"
--   , "(uncoveredLeafL' 20 lTree7)"
--   , "(uncoveredLeafL' 40 lTree7)"
--   , "(uncoveredLeafL' 20 lTree8)"
--   , "(uncoveredLeafL' 40 lTree8)"
--   , "(uncoveredLeafL' 34 lTree8)"
--   , "(uncoveredLeafM' 20 mTree7)"
--   , "(uncoveredLeafM' 40 mTree7)"
--   , "(uncoveredLeafM' 20 mTree8)"
--   , "(uncoveredLeafM' 40 mTree8)"
--   , "(uncoveredLeafM' 34 mTree8)"
--   ]

-- runTests = do
--   putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
--   let zipped = zip tests testLinesString
--   sequence (map (putStrLn . snd) (filter (not . fst) zipped))
--   return ()
