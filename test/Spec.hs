import Control.Monad (forM_)
import Cubic
import Lab3 (Algorithm (..), Config (..), Interpolated (..), parsePoints, runInterpolations)
import Linear
import Test.Hspec

-- Приближённое сравнение двух чисел с погрешностью
roughly :: Double -> Double -> Double -> Bool
roughly eps a b = abs (a - b) < eps

main :: IO ()
main = hspec $ do
    describe "linear interpolation" $ do
        it "interpolates on simple segments" $ do
            linearInterpolate 0.5 [(0, 0), (1, 1)] `shouldBe` [(0, 0), (0.5, 0.5), (1, 1)]

        it "stitches multiple segments" $ do
            linearInterpolate 1 [(0, 0), (1, 1), (2, 0)] `shouldBe` [(0, 0), (1, 1), (2, 0)]

        it "keeps x in non-decreasing order" $ do
            let xs = map fst (linearInterpolate 0.3 [(0, 0), (1, 1), (2, 0)])
            xs `shouldSatisfy` nonDecreasing

    describe "cubic spline" $ do
        it "passes through source points" $ do
            let pts = [(0, 0), (1, 1), (2, 0)]
                spline = buildSpline pts
            forM_ pts $ \(x, y) -> evalSpline spline x `shouldSatisfy` roughly 1e-6 y

        it "behaves like a line on linear data" $ do
            let pts = [(0, 0), (1, 1), (2, 2)]
                samples = cubicInterpolate 0.5 pts
            samples `shouldSatisfy` all (uncurry (roughly 1e-6))

        it "keeps x in non-decreasing order" $ do
            let xs = map fst (cubicInterpolate 0.4 [(0, 0), (1, 1), (2, 0), (3, 1)])
            xs `shouldSatisfy` nonDecreasing

    describe "Lab3 helpers" $ do
        it "parses different separators in input" $ do
            parsePoints "0;1\n2\t3\n4,5\nbad\n" `shouldBe` [(0, 1), (2, 3), (4, 5)]

        it "runInterpolations with only linear matches linearInterpolate" $ do
            let pts = [(0, 0), (1, 1), (2, 0)]
                cfg = Config True False 0.5
                expected = [Interpolated Linear x y | (x, y) <- linearInterpolate (cfgStep cfg) pts]
            runInterpolations cfg pts `shouldBe` expected

        it "runInterpolations with both algorithms emits both kinds" $ do
            let pts = [(0, 0), (1, 1)]
                cfg = Config True True 1.0
                algos = map interpolatedAlgorithm (runInterpolations cfg pts)
            algos `shouldSatisfy` (\as -> Linear `elem` as && Cubic `elem` as)

-- Проверка, что последовательность не убывает
nonDecreasing :: (Ord a) => [a] -> Bool
nonDecreasing xs = and (zipWith (<=) xs (tail xs))
