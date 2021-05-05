import qualified Test.Comonad.Stream as Stream
import qualified Test.Comonad.Tape   as Tape

main :: IO ()
main = Stream.test >> Tape.test
