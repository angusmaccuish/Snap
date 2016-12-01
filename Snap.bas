Attribute VB_Name = "Snap"
Option Explicit
Option Base 1

Private GammaLn() As Double
Private MaxPairsAvailable() As Integer

'' ===============================================================================================================
'' Pre-calculate GammaLn and MaxPairsAvailable for all possible values
'' ===============================================================================================================
Private Sub InitCache(ranks As Integer, suits As Integer)
    Dim deck As Integer: deck = ranks * suits
    ReDim GammaLn(deck + 1)
    Dim k As Integer
    For k = 1 To deck + 1
        GammaLn(k) = WorksheetFunction.GammaLn(k)
    Next

    ReDim MaxPairsAvailable(0 To deck, 0 To ranks, 1 To suits)
    Dim r As Integer, s As Integer
    For k = 0 To deck
        For r = 0 To ranks
            MaxPairsAvailable(k, r, 1) = 0
            For s = 2 To suits
                MaxPairsAvailable(k, r, s) = MaxPairs(k, r, s)
            Next s
        Next r
    Next k
End Sub

'' ===============================================================================================================
'' First Pair Mean Location with time taken in seconds
'' ===============================================================================================================
Public Function FirstPairMeanLocationWithTime(ByVal ranks As Integer, ByVal suits As Integer) As Double()
    Dim startTime As Date: startTime = Timer
    Dim result(2) As Double
    result(1) = FirstPairMeanLocation(ranks, suits)
    result(2) = Timer - startTime
    FirstPairMeanLocationWithTime = result
End Function

'' ===============================================================================================================
'' First Pair Mean Location
'' ===============================================================================================================
Public Function FirstPairMeanLocation(ByVal ranks As Integer, ByVal suits As Integer) As Double
    Call InitCache(ranks, suits)
    Dim k As Integer, deck As Integer: deck = ranks * suits
    For k = 2 To deck
        FirstPairMeanLocation = FirstPairMeanLocation + k * FirstPairProbability(k, ranks, suits)
    Next k
End Function

'' ===============================================================================================================
'' First Pair Probability
'' ===============================================================================================================
Private Function FirstPairProbability(ByVal k As Integer, ByVal ranks As Integer, ByVal suits As Integer) As Double
    If k < 2 Then
        FirstPairProbability = 0
    Else
        FirstPairProbability = (1 - ProbabilityPairBefore(k, ranks, suits, 2)) * (suits - 1) / (ranks * suits - 1)
    End If
End Function

'' ===============================================================================================================
'' Probability that a pair occurs before a set of matching cards terminating at k
'' ===============================================================================================================
Private Function ProbabilityPairBefore(ByVal k As Integer, ByVal ranks As Integer, ByVal suits As Integer, ByVal m As Integer) As Double
    If k <= m Then
        ProbabilityPairBefore = 0
    ElseIf m = suits Then
        ProbabilityPairBefore = PairProbability(k - m, ranks - 1, suits, 0)
    Else
        ProbabilityPairBefore = PairProbability(k - m, ranks - 1, suits, suits - m) _
            + (1 - ProbabilityPairBefore(k, ranks, suits, m + 1)) * (suits - m) / (ranks * suits - m)
    End If
End Function

'' ===============================================================================================================
'' Pair Probability
'' ===============================================================================================================
Private Function PairProbability(ByVal k As Integer, ByVal ranks As Integer, ByVal suits As Integer, ByVal jokers As Integer) As Double
    Dim MaxPairs As Integer: MaxPairs = ((suits - 1) * k) \ suits

    If MaxPairs > 0 Then
        Dim p As Integer
        Dim coeff As Double: coeff = 1
        For p = 1 To MaxPairs
            PairProbability = PairProbability + (-1) ^ (p + 1) * AtLeastPPairs(k, ranks, suits, jokers, p)
        Next p
    Else
        PairProbability = 0
    End If
End Function

'' ===============================================================================================================
'' At least p pairs probability
'' ===============================================================================================================
Private Function AtLeastPPairs(ByVal k As Integer, ByVal ranks As Integer, ByVal suits As Integer, ByVal jokers As Integer, ByVal p As Integer) As Double
    Dim deck As Integer: deck = ranks * suits + jokers
    Dim logPerms: logPerms = PermutationsWithJokers(ranks, suits, jokers, p, k)

    Dim m As Integer, count As Integer: count = UBound(logPerms, 2)

    For m = 1 To count
        Dim logBlockPerms As Double: logBlockPerms = logPerms(1, m)
        Dim cardsUsed As Integer: cardsUsed = logPerms(2, m)
        If cardsUsed <> 0 Then
            AtLeastPPairs = AtLeastPPairs + _
                Exp(GammaLn(k - p + 1) - GammaLn(k - cardsUsed + 1) + GammaLn(deck - cardsUsed + 1) - GammaLn(deck + 1) + logBlockPerms)
        End If
    Next m
End Function

'' ===============================================================================================================
'' PermutationsWithJokers
'' ===============================================================================================================
Private Function PermutationsWithJokers(ByVal ranks As Integer, ByVal suits As Integer, ByVal jokers As Integer, ByVal pairs As Integer, ByVal cards As Integer) As Variant
    Dim initialBlock: initialBlock = Array(Min(suits, cards))
    If jokers < 2 Then
        PermutationsWithJokers = Permutations(ranks, suits, pairs, cards, initialBlock)
    Else
        Dim allPerms() As Variant, counter As Integer
        Dim jokerBlocks, jokerBlockConfigs: jokerBlockConfigs = AllJokerBlocks(jokers)
        For Each jokerBlocks In jokerBlockConfigs
            If AreJokerBlocksAllowed(ranks, suits, pairs, cards, jokerBlocks) Then
                Dim jokerCards As Integer: jokerCards = 0
                Dim jokerPairs As Integer: jokerPairs = 0
                Dim block
                For Each block In jokerBlocks
                    jokerCards = jokerCards + block
                    If block <> 0 Then jokerPairs = jokerPairs + (block - 1)
                Next
                Dim logJokerBlockPerms As Double: logJokerBlockPerms = 0
                If jokerCards <> 0 Then logJokerBlockPerms = LogBlockPermutations(1, jokers, 1, jokerBlocks)
                Dim perms: perms = Permutations(ranks, suits, pairs - jokerPairs, cards - jokerCards, initialBlock)
                Dim numberOfPerms As Integer: numberOfPerms = UBound(perms, 2)
                ReDim Preserve allPerms(2, counter + numberOfPerms)
                Dim k As Integer
                For k = 1 To numberOfPerms
                    counter = counter + 1
                    allPerms(1, counter) = perms(1, k) + logJokerBlockPerms
                    allPerms(2, counter) = perms(2, k) + jokerCards
                Next k
            End If
        Next
        PermutationsWithJokers = allPerms
    End If
End Function

'' ===============================================================================================================
'' Permutations of cards which make up a specific number of pairs
'' * ranks, suits, jokers specify the deck make-up
'' * pairs is the target number of pairs we need to construct
'' * cards is the available number of cards in the deck to work with (cf. 'k' in the first k cards of the game)
'' * blocks are card configs (explain later - relates to recursion)
'' ===============================================================================================================
Private Function Permutations(ByVal ranks As Integer, ByVal suits As Integer, ByVal pairs As Integer, ByVal cards As Integer, ByRef blocks As Variant) As Variant
    Dim blockCards As Integer
    Dim blockPairs As Integer
    Dim block
    For Each block In blocks
        blockCards = blockCards + block
        blockPairs = blockPairs + (block - 1) ' a block of size N contributes N-1 pairs!
    Next

    Dim perms() As Double
    ReDim perms(2, 1)

    If blockCards > 1 Then
        Dim n As Integer
        Dim maxBlocks As Integer: maxBlocks = ranks

        Dim pairsCreated As Integer
        Dim cardsUsed As Integer
        Dim counter As Integer

        For n = 0 To maxBlocks
            pairsCreated = n * blockPairs
            cardsUsed = n * blockCards
            If pairs >= pairsCreated And cardsUsed <= cards Then
                ' n sets of the 'blocks' do not break the constraints (required pairs and card count),
                ' however we need to now check that there are enough remaining pairs of lower order to
                ' satisfy the number of pairs, or else we move on
                If MaxPairsAvailable(cards - cardsUsed, ranks - n, blockCards - 1) >= (pairs - pairsCreated) Then
                    Dim logBlockPerms As Double: logBlockPerms = LogBlockPermutations(ranks, suits, n, blocks)
                    Dim others: others = Permutations(ranks - n, suits, pairs - pairsCreated, cards - cardsUsed, NextBlocks(blocks))
                    Dim additional As Long: additional = UBound(others, 2)
                    ReDim Preserve perms(2, counter + additional)
                    Dim k As Integer
                    For k = 1 To additional
                        counter = counter + 1
                        perms(1, counter) = others(1, k) + logBlockPerms
                        perms(2, counter) = others(2, k) + cardsUsed
                    Next k
                End If
            End If
        Next n
    End If

    Permutations = perms
End Function

'' ===============================================================================================================
'' Determine the maximum number of pairs that can be created in a specific number of cards.
'' Consume cards as efficiently as possible to make up the most pairs.
'' Stop if we run out of cards, ranks or suits.
'' ===============================================================================================================
Private Function MaxPairs(ByVal cards As Integer, ByVal ranks As Integer, ByVal suits As Integer) As Integer
    Dim ranksUsed As Integer
    Do Until cards < 2 Or ranks < 1 Or suits < 2
        ranksUsed = Min(ranks, cards \ suits)
        MaxPairs = MaxPairs + ranksUsed * (suits - 1)
        cards = cards - ranksUsed * suits
        ranks = ranks - ranksUsed
        suits = suits - 1
    Loop
End Function

'' ===============================================================================================================
'' Block Permutations
'' ===============================================================================================================
Private Function LogBlockPermutations(ByVal ranks As Integer, ByVal suits As Integer, ByVal ranksUsed As Integer, ByRef blocks As Variant) As Double
    If ranksUsed = 0 Then
        LogBlockPermutations = 0
    Else
        Dim logPerms As Double: logPerms = 0
        Dim suiteCardsRemaining As Integer: suiteCardsRemaining = suits
        Dim bins() As Integer
        ReDim bins(suits)

        ' For each block of cards, update the bins (records the block size frequency)
        ' Update the permutations and then deduct the cards in the block from the available total
        Dim block
        For Each block In blocks
            bins(block) = bins(block) + 1
            logPerms = logPerms + GammaLn(suiteCardsRemaining + 1) - GammaLn(suiteCardsRemaining - block + 1)
            suiteCardsRemaining = suiteCardsRemaining - block
        Next

        Dim logDegeneracy As Double: logDegeneracy = 0
        Dim freq
        For Each freq In bins
            ' TODO Overkill in many scenerios - check if faster using conditional GammaLn logic
            If freq > 1 Then logDegeneracy = logDegeneracy + GammaLn(freq + 1)
        Next

        LogBlockPermutations = GammaLn(ranks + 1) - GammaLn(ranksUsed + 1) - GammaLn(ranks - ranksUsed + 1) + ranksUsed * (logPerms - logDegeneracy)
    End If
End Function

'' ===============================================================================================================
'' Next blocks
'' ===============================================================================================================
Private Function NextBlocks(ByRef blocks As Variant) As Variant
    ' Required blocks metadata
    Dim total As Integer
    Dim max As Integer
    Dim totalGreaterThan2 As Integer

    Dim block
    For Each block In blocks
        total = total + block
        If block > 2 Then totalGreaterThan2 = totalGreaterThan2 + 1
        If block > max Then max = block
    Next

    Dim results() As Integer

    If total < 4 Or totalGreaterThan2 = 0 Or (totalGreaterThan2 = 1 And max = 3) Then
        ' Current level cannot be broken down further - drop down to next level
        ReDim results(1)
        results(1) = total - 1
    Else
        results = Spread(blocks)
    End If

    NextBlocks = results
End Function

'' ===============================================================================================================
'' Spread
'' ===============================================================================================================
Private Function Spread(ByRef blocks As Variant) As Variant
    Dim size As Integer: size = UBound(blocks)
    Dim total As Integer

    Dim block
    For Each block In blocks
        total = total + block
    Next

    Dim k As Integer
    Dim results() As Integer

    If size = 1 Then
        ' split a monolithic block in half
        ReDim results(2)
        results(1) = total \ 2
        results(2) = total - (total \ 2)
    ElseIf blocks(1) = 2 Then
        ' First block is 2 - freeze and split remaining blocks
        Dim remaining As Variant
        ReDim remaining(size - 1)
        For k = 2 To size
            remaining(k - 1) = blocks(k)
        Next k

        Dim tmp() As Integer: tmp = Spread(remaining)

        ReDim results(1 + UBound(tmp))
        results(1) = 2
        For k = 1 To UBound(tmp)
            results(k + 1) = tmp(k)
        Next k
    Else
        ' Clone the blocks, then decrement first while incrementing second
        ' Eventually first will reach 2, where the above condition is satisfied
        ReDim results(UBound(blocks))
        For k = 1 To UBound(blocks)
            results(k) = blocks(k)
        Next k
        results(1) = results(1) - 1
        results(2) = results(2) + 1
    End If

    Spread = results
End Function

'' ===============================================================================================================
'' Calculate all the pair configurations for a given number of cards n (includes no pair, 0)
'' Example 6 : (0) -> (6) -> (3,3) -> (2,4) -> (2,2,2) -> (5) -> (2,3) -> (4) -> (2,2) -> (3) -> (2)
'' Example 3 : (0) -> (3) -> (2)
'' Example 2 : (0) -> (2)
'' Example 1 : (0) -> (1)
'' ===============================================================================================================
Function AllJokerBlocks(ByVal n As Integer) As Variant
    Dim count As Integer: count = 1
    Dim result() As Variant
    ReDim result(count): result(count) = Array(0)
        
    If n > 1 Then
        Dim blocks As Variant: blocks = Array(n)
        Do Until blocks(1) = 1
            count = count + 1
            ReDim Preserve result(count): result(count) = blocks
            blocks = NextBlocks(blocks)
        Loop
    End If
    
    AllJokerBlocks = result
End Function

'' ===============================================================================================================
'' AreJokerBlocksAllowed
'' ===============================================================================================================
Function AreJokerBlocksAllowed(ByVal ranks As Integer, ByVal suits As Integer, ByVal pairs As Integer, ByVal cards As Integer, ByRef blocks As Variant) As Boolean
    Dim jokerCards As Integer, jokerPairs As Integer
    Dim block
    For Each block In blocks
        jokerCards = jokerCards + block
        If block <> 0 Then jokerPairs = jokerPairs + (block - 1)
    Next

    If jokerCards > cards Then
        AreJokerBlocksAllowed = False
    Else
        AreJokerBlocksAllowed = jokerPairs <= pairs And MaxPairsAvailable(cards - jokerCards, ranks, suits) >= (pairs - jokerPairs)
    End If
End Function

'' ===============================================================================================================
'' Optimisation - WorksheetFunction.Min() very slow
'' ===============================================================================================================
Function Min(x As Integer, y As Integer) As Integer
    If x < y Then
        Min = x
    Else
        Min = y
    End If
End Function
