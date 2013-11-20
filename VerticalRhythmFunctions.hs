{-

A Module to implement Vertical Rhythm in Css generated using clay

-}

{-# LANGUAGE OverloadedStrings #-}

module VerticalRhythm where
import Clay
import Control.Monad.State
import Data.Default
import Data.Maybe

-- | Type CSSState stores base_font_size, base_line_height, browser_default_font_size 

data VerRhythm = VerRhythm 
	{ baseFontSize :: Integer
	, baseLineHeight :: Integer
	, browserDefaultFontSize :: Integer
	, minLinePadding :: Integer
	, roundToHalfLine :: Bool
	, defaultRhythmBorderStyle :: Stroke
	}

instance Default VerRhythm where def = VerRhythm 
	{ baseFontSize = 16
	, baseLineHeight = 24 
	, browserDefaultFontSize = 16
	, minLinePadding = 2
	, roundToHalfLine = False
	, defaultRhythmBorderStyle = solid
	}

-- |  Establishes the baseline for the given CSSState

establishBaseline :: VerRhythm -> Css
establishBaseline vr =  baseline f h b
	where
	f = baseFontSize vr
	h = baseLineHeight vr
	b = browserDefaultFontSize vr


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
		fontSize (px x)
		lineHeight (em k)
			where
			k = rhythm 1.0 x y 0
			p= (realToFrac (x*100)) / (realToFrac 16)


--  | Calculates rhythm units
rhythm :: Double -> Integer-> Integer -> Integer -> Double
rhythm l f h o  = ((l*realToFrac((h - o))) / realToFrac (f))
--l is the number of lines, f is the base_font_size, h is the base_line_height and o is the offset




{- 

   Adjust a block to have a different font size and line height to maintain the rhythm. 
   l specifies how many multiples of the baseline rhythm each line of this font should use up. It does not have to be an integer, 
   but it defaults to the smallest integer that is large enough to fit the font.
   Use $from-size to adjust from a font-size other than the base font-size.

-}
toFontSize:: VerRhythm -> Integer -> Maybe Double -> Maybe Integer -> Css
toFontSize vr toSize lines fromSize   =  adjustFontSize toSize l1 f h
	where
	h  = baseLineHeight vr
	m  = minLinePadding vr
	r  = roundToHalfLine vr
	l1 = case lines of
		Just k -> k
		Nothing -> linesForFontSize toSize h m r
	f = case fromSize of
		Just k -> k
		Nothing -> baseFontSize vr 
						 

-- | returns Css to adjust Fontsize of a element
adjustFontSize :: Integer -> Double -> Integer -> Integer -> Css
adjustFontSize t l f h = do	
-- t is the to_font_size, l is the number of lines, f is the from_font_size, h is the baseline height		
	fontSize ( pct k)
	lineHeight (em s)		
	where 
	k= (realToFrac (t*100)) / (realToFrac f)
	s=rhythm l t h 0 




-- | Calculate the minimum multiple of rhythm units needed to contain the font-size.
linesForFontSize:: Integer-> Integer -> Integer -> Bool -> Double 
linesForFontSize f h m r = 
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
leader:: VerRhythm -> Maybe Double-> Maybe Integer -> Maybe String -> Css			
leader vr lines fSize property | property == (Just "padding") = paddingLeader vr lines fSize
			       | otherwise                    = marginLeader vr lines fSize 


-- |Apply leading whitespace as padding.
paddingLeader:: VerRhythm -> Maybe Double-> Maybe Integer -> Css
paddingLeader vr lines fSize =  paddingTop (em r)
	where
	l = fromMaybe 1.0 lines
	f = fromMaybe (baseFontSize vr) fSize
	r = rhythm l f (baseLineHeight vr) 0



-- |Apply leading whitespace as margin.
marginLeader:: VerRhythm -> Maybe Double-> Maybe Integer -> Css
marginLeader vr lines fSize =   marginTop (em r)
			    	where
			        l = fromMaybe 1.0 lines
			        f = fromMaybe (baseFontSize vr) fSize
			 	r = rhythm l f (baseLineHeight vr) 0

-- | Apply trailing whitespace. The property can be margin or padding. By default property is margin
trailer:: VerRhythm -> Maybe Double-> Maybe Integer -> Maybe String -> Css			
trailer vr lines fSize property | property == (Just "padding") = paddingTrailer vr lines fSize
		       		| otherwise                    = marginTrailer vr lines fSize 


-- |Apply trailing whitespace as padding.
paddingTrailer:: VerRhythm -> Maybe Double-> Maybe Integer -> Css
paddingTrailer vr lines fSize = paddingBottom (em r)
				where
			        l = fromMaybe 1.0 lines
			        f = fromMaybe (baseFontSize vr) fSize
			 	r = rhythm l f (baseLineHeight vr) 0


-- |Apply trailing whitespace as margin.
marginTrailer:: VerRhythm -> Maybe Double-> Maybe Integer -> Css
marginTrailer vr lines fSize =  marginBottom (em r)
				where
			        l = fromMaybe 1.0 lines
			        f = fromMaybe (baseFontSize vr) fSize
			 	r = rhythm l f (baseLineHeight vr) 0 


{-  
	Shorthand function to apply whitespace for top and bottom margins and padding. Takes the number of lines for each property with     		default value 0
-}  
propertyRhythm:: VerRhythm -> Maybe Double-> Maybe Double-> Maybe Double-> Maybe Double-> Maybe Integer -> Css
propertyRhythm vr ml pl mt pt fSize = do
			-- ml is marginTop lines, pl is paddingTop lines,mt is marginBottom lines,pt is  paddingBottom lines 
			marginLeader vr (Just ml1) fSize
			paddingLeader vr (Just pl1) fSize 
			paddingTrailer vr (Just pt1) fSize
			marginTrailer vr (Just mt1) fSize
				where
				ml1 = fromMaybe 0 ml
				pl1 = fromMaybe 0 pl
				mt1 = fromMaybe 0 mt
				pt1 = fromMaybe 0 pt


{- 
	Apply a border & whitespace to any side without destroying the verticalrhythm.
	The whitespace must be greater than the width of the border.
	In the function w is width, l is number of line,fSize is font size, bs is borderstyle, c is the color
-}
applySideRhythmBorder:: VerRhythm-> String-> Maybe Integer-> Maybe Double-> Maybe Integer -> Maybe Stroke ->Maybe Color -> Css
applySideRhythmBorder vr side w l fSize bs c | side == "Left" = do borderLeft borderStyle (em w2) c1
							           paddingLeft (em r) 
						
				      	     | side == "Top" = do borderTop borderStyle (em w2) c1
							          paddingTop (em r)  
						
				      	     | side == "Bottom" = do borderBottom borderStyle (em w2) c1
							             paddingBottom (em r) 
						
				             | side == "Right" = do borderRight borderStyle (em w2) c1
							            paddingRight (em r)
						     where
						     w1 = fromMaybe 1 w
						     l1 = fromMaybe 1.0 l
						     c1 = fromMaybe red c
						     fz = fromMaybe (baseFontSize vr) fSize
						     borderStyle = fromMaybe (defaultRhythmBorderStyle vr) bs
						     r = rhythm l1 fz (baseLineHeight vr) w1
						     w2 = realToFrac(w1) / realToFrac(fz)
					



{-	Apply borders and whitespace equally to all sides.
	In the function w is width, l is number of line,fSize is font size, bs is borderstyle, c is the color
-}
rhythmBorders:: VerRhythm -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Maybe Color -> Css
rhythmBorders vr w l fSize bs c = do border borderStyle (em w3) c1
				     padding (em r) (em r) (em r) (em r)
				      	  where 
				      	  l2 = fromMaybe 1.0 l
				      	  f2 = fromMaybe (baseFontSize vr) fSize
				      	  c1 = fromMaybe red c
				      	  borderStyle = fromMaybe (defaultRhythmBorderStyle vr) bs
				      	  w2 = fromMaybe 1 w	 
				      	  r = rhythm l2 f2 (baseLineHeight vr) w2 
					  w3 = realToFrac(w2) / realToFrac(f2)



-- |Apply a leading border.
leadingBorder :: VerRhythm -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Maybe Color ->Css
leadingBorder vr width lines fSize borderStyle color = applySideRhythmBorder vr "top" width lines fSize borderStyle color

-- |Apply a trailing border.
trailingBorder :: VerRhythm -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Maybe Color ->Css
trailingBorder vr width lines fSize borderStyle color = applySideRhythmBorder vr "Bottom" width lines fSize borderStyle color

-- |Apply both leading border and trailing border
horizontalBorder :: VerRhythm -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Maybe Color ->Css
horizontalBorder vr width lines fSize borderStyle color = do leadingBorder vr width lines fSize borderStyle color
							     trailingBorder vr width lines fSize borderStyle color

-- |Alias for `horizontal_borders
hBorder :: VerRhythm -> Maybe Integer -> Maybe Double -> Maybe Integer -> Maybe Stroke -> Maybe Color ->Css
hBorder vr width lines fSize borderStyle color = horizontalBorder vr width lines fSize borderStyle color
