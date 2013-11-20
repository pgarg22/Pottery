{-

A Module to implement Golden Ratio Typography in Css generated using clay
 
-}

{-# LANGUAGE OverloadedStrings #-}

module GoldenRatioFunctions where
import Clay



phi = (1.0 + sqrt(5)) / 2.0 ::Double     -- 1.61803398874989 or "The Golden Ratio"
xoo = 1.0 / (2.0 * phi) ::Double

--  | Calculated font size based on contentWidth.
calcFontSize :: Integer -> Integer
calcFontSize contentWidth = Prelude.round k
	where
	c = realToFrac(contentWidth)
	k = sqrt(c)/phi 

{-  USAGE : grFontSize contentWidth. 
    This returns the css for the calculated font size 
-}

grFontSize:: Integer -> Css
grFontSize contentWidth = do	
			  fontSize(px k)
			  fontSize (em p)
			  lineHeight (px c)
	where
	k = calcFontSize contentWidth
	j = realToFrac(contentWidth)
	yoo = (sqrt(j)) / phi                              -- line Height
	p = (realToFrac((100*k)) / (realToFrac(k))) 
	c = Prelude.round ((realToFrac(k)) / yoo)




--  | Calculated title size based on contentWidth.
calcTitleSize:: Integer -> Integer
calcTitleSize contentWidth = Prelude.round k
	where
	f = calcFontSize contentWidth
	p = phi ^ 2
	k = realToFrac(f) * p 

{-  USAGE : grTitleSize contentWidth. 
    This returns the css for the calculated title size based on contentwidth 
-}

grTitleSize:: Integer -> Css
grTitleSize contentWidth = do
			   fontSize(px k)
			   fontSize (em p)
	                   lineHeight (px c)
	where
	k = calcTitleSize contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	j = realToFrac(contentWidth)
	yoo = (sqrt(j)) / phi      
	c = Prelude.round ((realToFrac(k)) / yoo)


calcHeadLineSize:: Integer -> Integer
calcHeadLineSize contentWidth = Prelude.round k
	where
	f = calcFontSize contentWidth 
	p = phi ^ 1
	k = realToFrac(f) * p 

 
{-  USAGE : grHeadLineSize contentWidth. 
    This returns the css for the calculated Headline Size based on Contentwidth. 
-}
grHeadLineSize:: Integer -> Css
grHeadLineSize contentWidth = do
			      fontSize(px k)
			      fontSize (em p)
			      lineHeight (px c)
	where
	k = calcHeadLineSize contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	j = realToFrac(contentWidth)
	yoo = (sqrt(j)) / phi      
	c = Prelude.round ((realToFrac(k)) / yoo)

 
calcSubHeadLineSize:: Integer -> Integer
calcSubHeadLineSize contentWidth =   Prelude.round k
	where
	f =calcFontSize contentWidth
	p = sqrt(phi)
	k = realToFrac(f) * p 


{-  USAGE : grSubHeadLineSize contentWidth. 
    This returns the css for the calculated Sub Headline Size based on Contentwidth. 
-}
grSubHeadLineSize::  Integer -> Css
grSubHeadLineSize contentWidth = do
				 fontSize(px k)
				 fontSize (em p)
				 lineHeight (px c)

	where
	k = calcSubHeadLineSize contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((k)) / (realToFrac(f)))
	j = realToFrac(contentWidth)
	yoo = (sqrt(j)) / phi       
	c = Prelude.round ((realToFrac(k)) / yoo)


calcSecondaryText:: Integer -> Integer
calcSecondaryText contentWidth = Prelude.round k
	 where
	 f = calcFontSize contentWidth
	 p = sqrt(phi)
	 k = realToFrac(f) * ( (1.0) / p)

{-  USAGE : grSecondaryText contentWidth. 
    This returns the css for the calculated secondary text Size based on Contentwidth. 
-}

grSecondaryText:: Integer -> Css
grSecondaryText contentWidth = do
			       fontSize(px k)
			       fontSize (em p)
			       lineHeight (px c)
	where
	k = calcSecondaryText contentWidth
	f = calcFontSize contentWidth
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	j = realToFrac(contentWidth)
	yoo = (sqrt(j)) / phi      
	c = Prelude.round ((realToFrac(k)) / yoo)


calcBaseLineHeight:: Integer -> Double
calcBaseLineHeight contentWidth = k
	where
	f = calcFontSize contentWidth
	j = realToFrac(contentWidth)
	foobar = (sqrt(j)) / phi
	h = (realToFrac(f) * phi) ^ 2
	p = ((realToFrac(contentWidth)) / h )
	foo = realToFrac(f) * (phi - xoo * (1.0 - p))
	k = foo / foobar

{-  USAGE : grBaseLineHeight contentWidth. 
    This returns the CSS for the calculated default line-height 
    based on Contentwidth. 
-}

grBaseLineHeight::  Integer -> Css
grBaseLineHeight contentWidth = do
				lineHeight (px c)
	where
	c = Prelude.round (calcBaseLineHeight contentWidth)


calcCustomLineHeight:: Integer -> Integer ->Double
calcCustomLineHeight contentWidth target =   foo
	where
	j = realToFrac(contentWidth)
	foobar = (sqrt(j)) / phi
	foo = (realToFrac(target)) / foobar

{-  USAGE : grCustomLineHeight contentWidth target. 
    This returns the css for the calculated Golden ratio line 
    height based on Contentwidth. 
-}

grCustomLineHeight:: Integer -> Integer-> Css
grCustomLineHeight contentWidth target = lineHeight (px c)
	where
	c = Prelude.round (calcCustomLineHeight contentWidth target)





