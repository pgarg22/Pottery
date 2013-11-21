import VerticalRhythm
import Clay
import Control.Monad.State
import Data.Default
import Data.Monoid
import Prelude hiding ((**),div)
import Data.Text

main =  do  
	putCss $
	   do k
	      p1 ? m
	      p2 ? do 
                     j
                     rb
	      p3 ? do m2
		      tb 
        where
        t  = def
	vr = t { baseFontSize = 20, baseLineHeight = 24}
        k  = establishBaseline vr
	j  = toFontSize vr 28 Nothing Nothing
	m  = leader vr Nothing Nothing (Just "margin")
	m2 = trailer vr Nothing Nothing (Just "padding")
	rb = rhythmBorders vr (Just 5) Nothing Nothing Nothing (Just black)
        tb = trailingBorder vr (Just 4) Nothing Nothing Nothing (Just black)
        p1 = element l1
	l1 = pack "p.p1"
        p2 =  element l2
	l2 = pack "p.p2"
        p3 =  element l3
	l3 = pack "p.p3"
