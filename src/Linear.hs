{-# LANGUAGE DeriveFunctor #-}

module Linear (
    Point,
    Timed (..),
    linearStream,
    linearInterpolate,
)
where

type Point = (Double, Double)

-- Значение, сопровождаемое индексом входной точки, позволившей его вывести
data Timed a = Timed
    { emittedAt :: !Int
    , timedValue :: !a
    }
    deriving (Eq, Show, Functor)

-- Интерполировать отрезками и вернуть только значения
linearInterpolate :: Double -> [Point] -> [(Double, Double)]
linearInterpolate step = map timedValue . linearStream step

-- Потоковая линейная интерполяция: выводит точки по мере поступления входных точек
linearStream :: Double -> [Point] -> [Timed (Double, Double)]
linearStream _ [] = []
linearStream step (p0 : rest) =
    Timed 1 p0 : go 1 p0 (fst p0 + step) (fst p0) rest
  where
    eps = 1e-9

    go :: Int -> Point -> Double -> Double -> [Point] -> [Timed (Double, Double)]
    go idx prev nextX lastEmittedXs points =
        case points of
            [] ->
                [Timed idx prev | lastEmittedXs + eps < fst prev]
            (p : ps) ->
                let idx' = idx + 1
                    prevOut = [Timed idx prev | lastEmittedXs + eps < fst prev]
                    (xs, nextX') = spanToLimit nextX step (fst p)
                    outs = map (\x -> Timed idx' (x, interpolate prev p x)) xs
                    emitted = prevOut ++ outs
                    newLast =
                        case emitted of
                            [] -> lastEmittedXs
                            _ -> fst (timedValue (last emitted))
                 in emitted ++ go idx' p nextX' newLast ps

-- Сгенерировать последовательность x до порога limit, вернуть следующее стартовое значение
spanToLimit :: Double -> Double -> Double -> ([Double], Double)
spanToLimit start step limit =
    let xs = takeWhile (<= limit + 1e-9) (iterate (+ step) start)
     in case xs of
            [] -> ([], start)
            _ -> (xs, last xs + step)

-- Прямая линейная интерполяция между двумя точками
interpolate :: Point -> Point -> Double -> Double
interpolate (x0, y0) (x1, y1) x
    | abs (x1 - x0) < 1e-12 = y0
    | otherwise = y0 + (y1 - y0) * ((x - x0) / (x1 - x0))
