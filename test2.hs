import GoldenRatio
import Clay
import Control.Monad.State
import Data.Default

main =  do  
	putCss $
         do 
	   html ? 
                  do fs
           h1 ? hs
	   h2 ? shs
     
        where
        ts  = grTitleSize 20
        hs  = grHeadLineSize 20 
        shs = grSubHeadLineSize 20 
        fs  = grFontSize 20 
        st  = grSecondaryText 20 
        lh  = grBaseLineHeight 20
