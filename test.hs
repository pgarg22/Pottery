import VerticalRhythm
import Clay
import Control.Monad.State

main =  let k= fst(runState establish_baseline (5,4))
	in putCss k
