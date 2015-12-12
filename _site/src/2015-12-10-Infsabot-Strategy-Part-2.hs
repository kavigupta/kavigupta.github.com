asses :: [RP] -> RP -> Double
modify :: RP -> StdGen -> (RP, StdGen)

type HistoricalStates = [KnownState]

complexity :: RP -> Int

getDeltas :: Ratio -> RP -> [RP]

applyDeltas :: [Ratio] -> RP -> RP

constantToParameter :: HistoricalStates -> RP -> StdGen -> (RP, StdGen)

simplify :: HistoricalStates -> RP -> StdGen -> (RP, StdGen)

data GeneratorParameters = GeneratorParameters {
    closeEnoughThreshold :: Ratio
}

simplify :: GeneratorParameters -> HistoricalStates -> RP -> StdGen -> (RP, StdGen)

complicate :: Int -> RP -> StdGen -> (RP, StdGen)

randomlyGenerate :: StdGen -> (RP, StdGen)

instance Random RP where
    ...

class (Random a) => Expr a where
    complexity :: a -> Int
    getDeltas :: Ratio -> a -> [a]
    applyDeltas :: [Ratio] -> a -> a
    constantToParameter :: HistoricalStates -> a -> StdGen -> (a, StdGen)
    simplify :: HistoricalStates -> a -> StdGen -> (a, StdGen)
    complicate :: Int -> RP -> StdGen -> (RP, StdGen)

