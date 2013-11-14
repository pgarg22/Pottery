{-

A Module to implement Vertical Rhythm in Css generated using clay

-}

{-# LANGUAGE OverloadedStrings #-}

module VerticalRhythm where
import Clay
import Control.Monad.State
import Data.Default

-- | Type CSSState stores base_font_size, base_line_height, browser_default_font_size 

data CSSState = Base Integer Integer Integer

instance Default CSSState where def = (Base 16 24 16)

-- |  Establishes the baseline for the given CSSState

establish_baseline :: CSSState -> Css
establish_baseline (Base x y z) = baseline x y z


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


-- | Adjust Fontsize of a element


font_size :: Integer -> Double -> Integer -> Integer -> Css
font_size to_size lines from_size base_line_height= do	
			
			fontSize ( pct k)
			lineHeight (em s)
			
			where 
			k= (realToFrac (to_size*100)) / (realToFrac from_size)
			s=rhythm lines to_size base_line_height 0 





