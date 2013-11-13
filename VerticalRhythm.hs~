{-

A Module to implement Vertical Rhythm in Css generated using clay

-}

{-# LANGUAGE OverloadedStrings #-}

module VerticalRhythm where
import Clay
import Control.Monad.State

-- | Type CSSState used to keep track of the base_font_size and the base_line_height
type CSSState = (Integer,Integer)
type MyStateMonad  = State CSSState 


-- |  Establishes the baseline for the given CSSState
establish_baseline :: MyStateMonad Css
establish_baseline = state (\st ->
				let (x , y) = st 
				    c= baseline x y
				in (c, st))


-- | returns baseline Css
baseline::Integer->Integer->Css
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
rhythm:: Double -> Integer-> Integer -> Integer -> Double
rhythm l f h o  = do ((l*realToFrac((h - o))) / realToFrac (f))
--l is the number of lines, f is the base_font_size, h is the base_line_height and o is the offset


