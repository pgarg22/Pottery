module VerticalRhythm where
import Clay
import Control.Monad.State

type CSSState = (Integer,Integer)

type MyStateMonad  = State CSSState 

establish_baseline :: MyStateMonad Css Css
establish_baseline = state (\st ->
				let (x , y) = st 
				    k=rhythm 1.0 x 0
				    p= (realToFrac (2*100)) / (realToFrac 16)
				    c= ( do 
			     
		 	          		html?
				       			fontSize ( pct p)      
				  		html ?
				    			do
				       				fontSize      (px x)
				       				lineHeight (em k))

		       		in (c, st))
rhythm:: Double->Integer->Integer->Double
rhythm lines font_size offset  =do 	let h = 2
					((lines*realToFrac((h - offset))) / realToFrac (font_size))


