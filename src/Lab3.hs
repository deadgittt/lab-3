module Lab3 (
    Config (..),
    Algorithm (..),
    Interpolated (..),
    parsePoints,
    runInterpolations,
    main,
)
where

import qualified Cubic
import Data.Maybe (mapMaybe)
import Linear (Point, Timed (..), linearStream)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, isEOF, stderr)
import Text.Read (readMaybe)

data Algorithm = Linear | Cubic deriving (Eq, Show)

-- Результат интерполяции с указанием алгоритма
data Interpolated = Interpolated
    { interpolatedAlgorithm :: !Algorithm
    , interpolatedX :: !Double
    , interpolatedY :: !Double
    }
    deriving (Eq, Show)

data Config = Config
    { cfgLinear :: !Bool
    , cfgCubic :: !Bool
    , cfgStep :: !Double
    }
    deriving (Eq, Show)

-- Настройки по умолчанию: оба алгоритма, шаг 0.5
defaultConfig :: Config
defaultConfig =
    Config
        { cfgLinear = True
        , cfgCubic = True
        , cfgStep = 0.5
        }

parseConfig :: [String] -> Either String Config
parseConfig args = go args defaultConfig False
  where
    -- Разбираем список аргументов, аккумулируя конфиг
    go [] cfg algorithmsChosen =
        let finalCfg =
                if algorithmsChosen
                    then cfg
                    else cfg{cfgLinear = True, cfgCubic = True}
         in if cfgLinear finalCfg || cfgCubic finalCfg
                then Right finalCfg
                else Left "Не выбраны алгоритмы интерполяции (--linear/--cubic)"
    go ("--linear" : rest) cfg chosen =
        let base = if chosen then cfg else cfg{cfgLinear = False, cfgCubic = False}
         in go rest base{cfgLinear = True} True
    go ("--cubic" : rest) cfg chosen =
        let base = if chosen then cfg else cfg{cfgLinear = False, cfgCubic = False}
         in go rest base{cfgCubic = True} True
    go ("--no-linear" : rest) cfg chosen = go rest cfg{cfgLinear = False} True
    go ("--no-cubic" : rest) cfg chosen = go rest cfg{cfgCubic = False} True
    go ("--step" : v : rest) cfg chosen =
        case readMaybe v :: Maybe Double of
            Just s | s > 0 -> go rest cfg{cfgStep = s} chosen
            Just s | s <= 0 -> Left "Шаг дискретизации (--step) должен быть положительным числом"
            _ -> Left "Некорректное значение для шага дискретизации (--step)"
    go ("--help" : _) _ _ = Left usage
    go (x : xs) _ _ = Left $ "Неизвестный аргумент: " <> x <> rest xs

    rest [] = ""
    rest xs = " " <> unwords xs

usage :: String
usage =
    unlines
        [ "Использование: lab3 [--linear] [--cubic] [--no-linear] [--no-cubic] [--step <double>]"
        , "По умолчанию запускаются оба алгоритма, шаг = 0.5"
        ]

-- Разбор входных данных из строк
parsePoints :: String -> [Point]
parsePoints = mapMaybe parsePointLine . lines

parsePointLine :: String -> Maybe Point
parsePointLine raw =
    case words (map normalize raw) of
        [sx, sy] -> do
            x <- readMaybe sx
            y <- readMaybe sy
            pure (x, y)
        _ -> Nothing
  where
    normalize ch
        | ch == ';' || ch == ',' = ' '
        | otherwise = ch

-- Запустить выбранные алгоритмы и слить их результаты в один поток
runInterpolations :: Config -> [Point] -> [Interpolated]
runInterpolations cfg pts =
    let step = cfgStep cfg
        linearResults =
            if cfgLinear cfg
                then map (fmap toLinear) (linearStream step pts)
                else []
        cubicResults =
            if cfgCubic cfg
                then map (fmap toCubic) (Cubic.cubicStream step pts)
                else []
     in map timedValue (mergeTimed linearResults cubicResults)
  where
    toLinear (x, y) = Interpolated Linear x y
    toCubic (x, y) = Interpolated Cubic x y

mergeTimed :: [Timed a] -> [Timed a] -> [Timed a]
mergeTimed xs [] = xs
mergeTimed [] ys = ys
mergeTimed (x : xs) (y : ys)
    | emittedAt x <= emittedAt y = x : mergeTimed xs (y : ys)
    | otherwise = y : mergeTimed (x : xs) ys

-- Форматировать вывод для пользователя
render :: Interpolated -> String
render i =
    prefix ++ show (interpolatedX i) ++ " " ++ show (interpolatedY i)
  where
    prefix = case interpolatedAlgorithm i of
        Linear -> "linear: "
        Cubic -> "cubic: "

main :: IO ()
main = do
    args <- getArgs
    case parseConfig args of
        Left msg -> hPutStrLn stderr msg >> hPutStrLn stderr usage >> exitFailure
        Right cfg -> loop cfg [] 0

loop :: Config -> [Point] -> Int -> IO ()
loop cfg pts emittedCount = do
    eof <- isEOF
    if eof
        then pure ()
        else do
            line <- getLine
            case parsePointLine line of
                Nothing -> loop cfg pts emittedCount
                Just p ->
                    let pts' = pts ++ [p]
                        allOuts = runInterpolations cfg pts'
                        newOuts = drop emittedCount allOuts
                        emittedCount' = emittedCount + length newOuts
                     in mapM_ (putStrLn . render) newOuts >> loop cfg pts' emittedCount'
