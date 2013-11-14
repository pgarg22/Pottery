{-

A Module to implement Vertical Rhythm in Css generated using clay

-}

{-# LANGUAGE OverloadedStrings #-}

module VerticalRhythm where
import Clay
import Control.Monad.State
import Data.Default

-- | Type CSSState stores base_font_size, base_line_height, browser_default_font_size 

data CSSState = Base Integer Integer Integer Integer Bool

instance Default CSSState where def = (Base 16 24 16 2 False)

-- |  Establishes the baseline for the given CSSState

establish_baseline :: CSSState -> Css
establish_baseline (Base x y z m r) = baseline x y z


-- | returns baseline Css
baseline :: Integer -> Integer ->Integer -> Css
baseline x y z= do 
	--setting base Css
	-- x is the base_font_size and y is the base_line_height and z is the browser_default_font_size     
	html?
	   fontSize ( pct p)      
	html ? do
	   fontSize      (px x)
	   lineHeight (em k)
	where
	k=rhythm 1.0 x y 0
	p= (realToFrac (x*100)) / (realToFrac 16)


--  | Calculates rhythm units
rhythm :: Double -> Integer-> Integer -> Integer -> Double
rhythm l f h o  = do ((l*realToFrac((h - o))) / realToFrac (f))
--l is the number of lines, f is the base_font_size, h is the base_line_height and o is the offset


-- | Changes the font size of an element maintaining vertical rhythm
to_font_size:: CSSState -> Integer -> Maybe Double -> Maybe Integer -> Css
to_font_size (Base x y z m r) t l f = font_size t l1 f1 y
				  where
				  l1 = case l of
					 Just k -> k
					 Nothing -> lines_for_font_size t y m r
				  f1 = case f of
					 Just k-> k
					 Nothing -> x 
				 


-- | returns Css to adjust Fontsize of a element
font_size :: Integer -> Double -> Integer -> Integer -> Css
font_size t l f h = do	
	  -- t is the to_font_size, l is the number of lines, f is the from_font_size, h is the baseline height		
	  fontSize ( pct k)
          lineHeight (em s)
			
	  where 
	  k= (realToFrac (t*100)) / (realToFrac f)
	  s=rhythm l t h 0 




-- | Calculates the lines for the fontsize if not provided by the user
lines_for_font_size:: Integer-> Integer -> Integer -> Bool -> Double 
lines_for_font_size f h m r = 
		-- f is the to_fontsize, h is the base line height, m is the minimum line padding and r is the round_to_nearest_half_line
		if (((l*realToFrac (h))) - realToFrac (f)) < (realToFrac(m*2))     
		   then if r
		          then (l + 0.5)
			  else (l + 1.0)
		   else l

		where
		l = if r 
		      then realToFrac(ceiling((realToFrac (f*2) / realToFrac (h))/2)) 
		      else realToFrac(ceiling(realToFrac (f) / realToFrac(h)))

			


