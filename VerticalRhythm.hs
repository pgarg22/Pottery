{-

A Module to implement Vertical Rhythm in Css generated using clay

-}

{-# LANGUAGE OverloadedStrings #-}

module VerticalRhythm where
import Clay
import Control.Monad.State
import Data.Default

-- | Type CSSState stores base_font_size, base_line_height, browser_default_font_size 

data CSSState = Base Integer Integer Integer Integer Bool Stroke

instance Default CSSState where def = (Base 16 24 16 2 False solid)

-- |  Establishes the baseline for the given CSSState

establish_baseline :: CSSState -> Css
establish_baseline (Base f h b m r bs) = baseline f h b


-- | returns baseline Css
baseline :: Integer -> Integer ->Integer -> Css
baseline x y z= do 
	--setting base Css
	-- x is the base_font_size and y is the base_line_height and z is the browser_default_font_size     
	-- IE 6 refuses to resize fonts set in pixels and it weirdly resizes fonts 
	--whose root is set in ems. So we set the root font size in percentages of the default font size.
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


{- 

   Adjust a block to have a different font size and line height to maintain the rhythm. 
   l specifies how many multiples of the baseline rhythm each line of this font should use up. It does not have to be an integer, 
   but it defaults to the smallest integer that is large enough to fit the font.
   Use $from-size to adjust from a font-size other than the base font-size.

-}

-- | returns value inside Maybe Monad and if it is Nothing then the default value passed to this function
val::Maybe a -> a-> a
val (Just x) y = x;
val Nothing y = y;


to_font_size:: CSSState -> Integer -> Maybe Double -> Maybe Integer -> Css
to_font_size (Base f h z m r bs) t l f2 = font_size t l1 f1 h
				  where
				  l1 = case l of
					 Just k -> k
					 Nothing -> lines_for_font_size t h m r
				  f1 = case f2 of
					 Just k-> k
					 Nothing -> f 
				 


-- | returns Css to adjust Fontsize of a element
font_size :: Integer -> Double -> Integer -> Integer -> Css
font_size t l f h = do	
	  -- t is the to_font_size, l is the number of lines, f is the from_font_size, h is the baseline height		
	  fontSize ( pct k)
          lineHeight (em s)
			
	  where 
	  k= (realToFrac (t*100)) / (realToFrac f)
	  s=rhythm l t h 0 




-- | Calculate the minimum multiple of rhythm units needed to contain the font-size.
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


-- | Apply leading whitespace. The property can be margin or padding. By default property is margin
leader:: CSSState -> Maybe Double-> Maybe Integer -> Maybe String -> Css			
leader s l f2 property | property == (Just "padding") = padding_leader s l f2
		       | otherwise                    = margin_leader s l f2 


-- |Apply leading whitespace as padding.
padding_leader:: CSSState -> Maybe Double-> Maybe Integer -> Css
padding_leader (Base f h b m r bs) l  f2 =  paddingTop (em r)
					    where
					    l1 = val l 1.0
					    f3 = val f2 f
					    r= rhythm l1 f3 h 0



-- |Apply leading whitespace as margin.
margin_leader:: CSSState -> Maybe Double-> Maybe Integer -> Css
margin_leader (Base f h b m r bs) l  f2 =  marginTop (em r)
					   where
					   l1 = val l 1.0
					   f3 = val f2 f
					   r= rhythm l1 f3 h 0


-- | Apply trailing whitespace. The property can be margin or padding. By default property is margin
trailer:: CSSState -> Maybe Double-> Maybe Integer -> Maybe String -> Css			
trailer s l f2 property | property == (Just "padding") = padding_trailer s l f2
		        | otherwise                    = margin_trailer s l f2 


-- |Apply trailing whitespace as padding.
padding_trailer:: CSSState -> Maybe Double-> Maybe Integer -> Css
padding_trailer (Base f h b m r bs) l  f2 =  paddingBottom (em r)
					     where
					     l1 = val l 1.0
					     f3 = val f2 f
					     r= rhythm l1 f3 h 0



-- |Apply trailing whitespace as margin.
margin_trailer:: CSSState -> Maybe Double-> Maybe Integer -> Css
margin_trailer (Base f h b m r bs) l  f2 =  marginBottom (em r)
					    where
					    l1 = val l 1.0
					    f3 = val f2 f
					    r= rhythm l1 f3 h 0

{-  
	Shorthand function to apply whitespace for top and bottom margins and padding. Takes the number of lines for each property with     		default value 0
-}  
property_rhythm:: CSSState -> Maybe Double-> Maybe Double-> Maybe Double-> Maybe Double-> Maybe Integer -> Css
property_rhythm s ml pl mt pt f= do
			 
			margin_leader s (Just ml1) f
			padding_leader s (Just pl1) f 
			padding_trailer s (Just pt1) f
			margin_trailer s (Just mt1) f
			
			where
			ml1 = val ml 0
			pl1 = val pl 0
			mt1 = val mt 0	
			pt1 = val pt 0


-- |Apply a border & whitespace to any side without destroying the verticalrhythm.The whitespace must be greater than the width of the border.
apply_side_rhythm_border:: CSSState-> String-> Maybe Integer-> Maybe Double-> Maybe Integer -> Maybe Stroke -> Css
apply_side_rhythm_border (Base f h b m r bs ) s w l f1 bs1 | s == "Left"    =   do  borderLeft border_style (em w2) red
							                            paddingLeft (em r) 
						
						           | s == "Top"     =   do  borderTop border_style (em w2) red
							                            paddingTop (em r)  
						
						           | s == "Bottom"  =   do  borderBottom border_style (em w2) red
							                            paddingBottom (em r) 
						
						           | s == "Right"   =   do  borderRight border_style (em w2) red
							                            paddingRight (em r)
						        where
							w1 = val w 1
							l1 = val l 1.0
							fz = val f1 f
						      	border_style= val bs1 bs
							r = rhythm l1 fz h w1
							w2= realToFrac(w1) / realToFrac(fz)
					


-- | Apply borders and whitespace equally to all sides.
rhythm_borders:: CSSState -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Css
rhythm_borders (Base f h b m r bs ) w l f1 bs1 = do border border_style (em w3) red
						    padding (em r) (em r) (em r) (em r)

						   where 
						   l2 = val l 1.0
						   f2 = val f1 f
						   border_style= val bs1 bs
						   w2 = val w 1	 
  					           r  = rhythm l2 f2 h w2 
						   w3 = realToFrac(w2) / realToFrac(f2)
					
				


-- |Apply a leading border.
leading_border :: CSSState -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Css
leading_border s (width) (lines)(font_size) (border_style) =   apply_side_rhythm_border s "top" width lines font_size border_style

-- |Apply a trailing border.
trailing_border :: CSSState -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Css
trailing_border s (width) (lines)(font_size) (border_style) =  apply_side_rhythm_border s "Bottom" width lines font_size border_style

-- |Apply both leading border and trailing border
horizontal_border :: CSSState -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Css
horizontal_border s (width) (lines)(font_size) (border_style) = do leading_border s width lines font_size border_style
								   trailing_border s width lines font_size border_style

-- |Alias for `horizontal_borders
h_border :: CSSState -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Css
h_border s width lines font_size border_style = horizontal_border s width lines font_size border_style
