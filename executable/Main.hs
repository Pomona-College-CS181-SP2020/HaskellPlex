-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified SimplexStream

main :: IO ()
main = do
    let stream = SimplexStream.initializeStream
    let new_stream = SimplexStream.addVertex stream 5
    putStrLn (show new_stream)