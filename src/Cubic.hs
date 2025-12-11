module Cubic (
    Point,
    SplineSegment (..),
    cubicInterpolate,
    cubicStream,
    buildSpline,
    evalSpline,
)
where

import Data.List (zip4)
import Data.Maybe (fromMaybe)
import Linear (Timed (..))

type Point = (Double, Double)

-- Коэффициенты одного участка кубического полинома
data SplineSegment = SplineSegment
    { segX :: !Double
    , segA :: !Double
    , segB :: !Double
    , segC :: !Double
    , segD :: !Double
    }
    deriving (Eq, Show)

-- Помощник, убирающий оболочку Timed
cubicInterpolate :: Double -> [Point] -> [(Double, Double)]
cubicInterpolate step = map timedValue . cubicStream step

-- Стримим значения сплайна по мере поступления точек
cubicStream :: Double -> [Point] -> [Timed (Double, Double)]
cubicStream _ [] = []
cubicStream step (p0 : ps) = Timed 1 p0 : go 1 [p0] (fst p0 + step) (fst p0) ps
  where
    eps = 1e-9

    go :: Int -> [Point] -> Double -> Double -> [Point] -> [Timed (Double, Double)]
    go idx acc nextX lastEmittedXs remaining =
        case remaining of
            [] ->
                finalize idx acc nextX lastEmittedXs
            (p : rest) ->
                let idx' = idx + 1
                    acc' = acc ++ [p]
                    (outs, nextX', lastOutX) = emit idx' acc' nextX
                    newLast = fromMaybe lastEmittedXs lastOutX
                 in outs ++ go idx' acc' nextX' newLast rest

    finalize idx acc nextX lastEmittedXs =
        let lastX = fst (last acc)
            (xs, nextX') = spanToLimit nextX step lastX
            spline = buildSpline acc
            outs = map (\x -> Timed idx (x, evalSpline spline x)) xs
            lastOutX =
                case outs of
                    [] -> lastEmittedXs
                    _ -> fst (timedValue (last outs))
            lastPoint = [Timed idx (lastX, snd (last acc)) | lastOutX + eps < lastX]
         in outs ++ lastPoint

    emit idx acc nextX
        | length acc < 2 = ([], nextX, Nothing)
        | otherwise =
            let lastX = fst (last acc)
                (xs, nextX') = spanToLimit nextX step lastX
                spline = buildSpline acc
                outs = map (\x -> Timed idx (x, evalSpline spline x)) xs
                lastOut =
                    case outs of
                        [] -> Nothing
                        _ -> Just (fst (timedValue (last outs)))
             in (outs, nextX', lastOut)

-- Генерирует неубывающие x с заданным шагом до верхней границы
spanToLimit :: Double -> Double -> Double -> ([Double], Double)
spanToLimit start step limit =
    let xs = takeWhile (<= limit + 1e-9) (iterate (+ step) start)
     in case xs of
            [] -> ([], start)
            _ -> (xs, last xs + step)

-- Строит коэффициенты натурального кубического сплайна для всех интервалов
buildSpline :: [Point] -> [SplineSegment]
buildSpline pts
    | length pts < 2 = []
    | otherwise = map mkSegment [0 .. n - 1]
  where
    xs = map fst pts
    ys = map snd pts
    n = length pts - 1
    hs = zipWith (-) (drop 1 xs) xs
    ms = secondDerivatives xs ys

    mkSegment i =
        let xk = xs !! i
            yk = ys !! i
            yk1 = ys !! (i + 1)
            mk = ms !! i
            mk1 = ms !! (i + 1)
            h = hs !! i
            b = (yk1 - yk) / h - h * (mk1 + 2 * mk) / 6
            c = mk / 2
            d = (mk1 - mk) / (6 * h)
         in SplineSegment xk yk b c d

-- Ищет вторые производные в узлах сплайна, решая СЛАУ методом прогонки
secondDerivatives :: [Double] -> [Double] -> [Double]
secondDerivatives xs ys
    | length xs < 2 = replicate (length xs) 0
    | length xs == 2 = [0, 0]
    | otherwise = 0 : mVals ++ [0]
  where
    n = length xs - 1
    hs = zipWith (-) (drop 1 xs) xs
    m = n - 1

    a = take m hs
    c = drop 1 hs
    b = zipWith (\l r -> 2 * (l + r)) a c
    d =
        [ 6 * ((ys !! (i + 1) - ys !! i) / (hs !! i) - (ys !! i - ys !! (i - 1)) / (hs !! (i - 1)))
        | i <- [1 .. m]
        ]

    (cPrimes, dPrimes) = forwardSweep b c d a
    mVals = backSubstitution cPrimes dPrimes

-- Прямой ход прогонки для трёхдиагональной матрицы
forwardSweep :: [Double] -> [Double] -> [Double] -> [Double] -> ([Double], [Double])
forwardSweep (b0 : bs) (c0 : cs) (d0 : ds) a =
    let c0' = c0 / b0
        d0' = d0 / b0
        step (prevC, prevD, cAcc, dAcc) (bi, ci, di, ai) =
            let denom = bi - ai * prevC
                ci' = ci / denom
                di' = (di - ai * prevD) / denom
             in (ci', di', cAcc ++ [ci'], dAcc ++ [di'])
        (_, _, cRes, dRes) = foldl step (c0', d0', [c0'], [d0']) (zip4 bs cs ds (drop 1 a))
     in (cRes, dRes)
forwardSweep _ _ _ _ = ([], [])

-- Обратный ход прогонки для трёхдиагональной матрицы
backSubstitution :: [Double] -> [Double] -> [Double]
backSubstitution cPrimes dPrimes =
    case reverse dPrimes of
        [] -> []
        (start : restDRev) ->
            let cRev = reverse cPrimes
                pairs = zip (drop 1 cRev) restDRev
                (_, acc) =
                    foldl
                        ( \(prev, res) (cp, dp) ->
                            let m = dp - cp * prev
                             in (m, m : res)
                        )
                        (start, [start])
                        pairs
             in acc

-- Вычисляет сплайн в точке x, выбирая нужный сегмент
evalSpline :: [SplineSegment] -> Double -> Double
evalSpline [] _ = 0
evalSpline segments x =
    let segment = choose segments
        dx = x - segX segment
     in segA segment + segB segment * dx + segC segment * dx * dx + segD segment * dx * dx * dx
  where
    choose [s] = s
    choose (s : rest@(next : _))
        | x < segX next = s
        | otherwise = choose rest
    choose [] = error "unreachable"
