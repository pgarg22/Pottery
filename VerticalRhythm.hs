{-

A Module to implement Vertical Rhythm in Css generated using clay

-}

{-# LANGUAGE OverloadedStrings #-}

module VerticalRhythm where
import Clay
import Control.Monad.State

-- | Type CSSState used to keep track of the base_font_size and the base_line_height
type CSSState = (Integer,Integer)


-- |  Establishes the baseline for the given CSSState

establish_baseline :: CSSState -> Css
establish_baseline (x ,y) = baseline x y


-- | returns baseline Css
baseline :: Integer -> Integer -> Css
baseline x y = do 
	--setting base Css
	-- x is the base_font_size and y is the base_line_height 		     
	html?
	   fontSize ( pct p)      
	html ? do
	   fontSize      (px x)
	   lineHeight (em k)
	where
	k=rhythm 1.0 x y 0
	p= (realToFrac (2*100)) / (realToFrac 16)


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



