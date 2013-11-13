module VerticalRhythm where
import Clay

data Config a = Co a 


newtype State s a = State (s -> (a,s))

type CSSState = (Integer,Integer)

instance Monad (State s) where

  return x = State ( \st -> (x, st) )
  mv >>= g = State ( \st -> 
                 let (y, st') = runState mv st
                 in runState (g y) st' )



runState :: State s a -> s -> (a, s)
runState (State f) init_st = f init_st

establish_baseline :: State CSSState Css
establish_baseline = State(\st ->
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



