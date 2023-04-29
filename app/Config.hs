module Config (Size, size, refleshInterval, initAliveCellRate) where

type Size = (Int, Int)

size :: Size
size = (50, 30) -- (Width, Height)

refleshInterval :: Int
refleshInterval = 1 * 1000 * 1000 -- (μs)

initAliveCellRate :: Double
initAliveCellRate = 0.2
