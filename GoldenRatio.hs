{-

A Module to implement Golden Ratio Typography in Css generated using clay
 
-}

{-# LANGUAGE OverloadedStrings #-}

module GoldenRatio where
import Clay
import Control.Monad.State
import Data.Default
import Data.Maybe

phi = (1.0 + sqrt(5)) / 2.0 ::Double     -- 1.61803398874989 or "The Golden Ratio"
yoo = 1.0 / (2.0 * phi) ::Double

--  | Calculated font size based on contentWidth.
calcFontSize :: Integer -> Integer
calcFontSize contentWidth = Prelude.round k
			    where
			    c =realToFrac(contentWidth)
			    k= sqrt(c)/phi 


-- | returns Css for Calculated font size based on contentWidth.
grFontSize:: Integer -> Css
grFontSize contentWidth = do
	
	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(k))) 
	c = (realToFrac(k)) / yoo




--  | Calculated title size based on contentWidth.
calcTitleSize:: Integer -> Integer
calcTitleSize contentWidth = Prelude.round k
			     where
			     f=calcFontSize contentWidth
			     p= phi ^ 2
			     k= realToFrac(f) * p 

-- | returns Css for Calculated title size based on contentWidth.
grTitleSize:: Integer -> Css
grTitleSize contentWidth = do

	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcTitleSize contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo



--  | Calculated Headline Size based on contentWidth.
calcHeadLineSize:: Integer -> Integer
calcHeadLineSize contentWidth = Prelude.round k
		  		where
		  		f=calcFontSize contentWidth 
		  		p= phi ^ 1
		  		k= realToFrac(f) * p 

 
-- | returns Css for Calculated Headline Size based on contentWidth.
grHeadlineSize:: Integer -> Css
grHeadlineSize contentWidth = do
	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcHeadLineSize contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo



 
--  | Calculated sub-Headline Size based on contentWidth. 
calcSubHeadLineSize:: Integer -> Integer
calcSubHeadLineSize contentWidth =   Prelude.round k
				     where
				     f=calcFontSize contentWidth
				     p= sqrt(phi)
				     k= realToFrac(f) * p 


-- | returns Css for sub-Headline Size based on contentWidth.
grSubHeadLineSize::  Integer -> Css
grSubHeadLineSize contentWidth = do

	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcSubHeadLineSize contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo


--  | Calculated secondary text size based on contentWidth.
calcSecondaryText:: Integer -> Integer
calcSecondaryText contentWidth = Prelude.round k
		    		 where
		    		 f=calcFontSize contentWidth
		    		 p= sqrt(phi)
		    		 k= realToFrac(f) * ( (1.0) / p)


-- | returns Css for Calculated secondary text  size based on contentWidth.
grSecondaryText:: Integer -> Css
grSecondaryText contentWidth = do

	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcSecondaryText contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo



--  | Calculated default line-height based on contentWidth.
calcBaseLineHeight:: Integer -> Double
calcBaseLineHeight contentWidth = k
		     		  where
		     		  f	= calcFontSize contentWidth
				  j     = realToFrac(contentWidth)
		     		  foobar= (sqrt(j)) / phi
		     		  h 	= (realToFrac(f) * phi) ^ 2
		     		  p 	= ((realToFrac(contentWidth)) / h )
				  xoo 	= sqrt(j) / phi
		     		  foo	= realToFrac(f) * (phi - xoo * (1.0 - p))
		     		  k	= foo / foobar


-- | returns Css for Calculated default line-height based on contentWidth.
grBaseLineHeight::  Integer -> Css
grBaseLineHeight contentWidth = do

	lineHeight (em c)

	where
	c = calcBaseLineHeight contentWidth




--  | Calculated a Golden Ratio line-height based on contentWidth.
calcCustomLineHeight:: Integer -> Integer ->Double
calcCustomLineHeight contentWidth target =   foo
					     where
					     j      = realToFrac(contentWidth)
					     foobar = (sqrt(j)) / phi
					     foo    = (realToFrac(target)) / foobar


-- | returns Css for Calculated Golden Ratio line-height based on contentWidth.				
grCustomLineHeight:: Integer -> Integer-> Css
grCustomLineHeight contentWidth target = lineHeight (em c)
		     	    		 where
		     	    		 c = calcCustomLineHeight contentWidth target





