{-

A Module to implement Golden Ratio Typography in Css generated using clay
 
-}

{-# LANGUAGE OverloadedStrings #-}

module GoldenRatio where
import Clay
import Control.Monad.State
import Data.Default
import Data.Maybe

contentWidth= 2.2 ::Double
phi = (1.0 + sqrt(5)) / 2.0 ::Double     -- 1.61803398874989 or "The Golden Ratio"
yoo = 1.0 / (2.0 * phi) ::Double
zoo = sqrt(contentWidth) / phi ::Double
xoo = phi - xoo * (1.0 - (contentWidth / yoo)) ::Double



--  | Calculated font size based on contentWidth.
calcFontSize:: Integer
calcFontSize = Prelude.round k
		where
		k= sqrt(contentWidth/phi) 


-- | returns Css for Calculated font size based on contentWidth.
grFontSize:: Css
grFontSize = do
	
	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcFontSize
	p = (realToFrac((100*k)) / (realToFrac(k))) 
	c = (realToFrac(k)) / yoo


--  | Calculated title size based on contentWidth.
calcTitleSize:: Integer
calcTitleSize = Prelude.round k
		where
		f=calcFontSize 
		p= phi ^ 2
		k= realToFrac(f) * p 

-- | returns Css for Calculated title size based on contentWidth.
grTitleSize::  Css
grTitleSize = do

	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcTitleSize
	f = calcFontSize
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo

--  | Calculated Headline Size based on contentWidth.
calcHeadLineSize:: Integer
calcHeadLineSize= Prelude.round k
		  where
		  f=calcFontSize 
		  p= phi ^ 1
		  k= realToFrac(f) * p 

 
-- | returns Css for Calculated Headline Size based on contentWidth.
grHeadlineSize:: Css
grHeadlineSize = do
	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcHeadLineSize
	f = calcFontSize
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo

 
--  | Calculated sub-Headline Size based on contentWidth. 
calcSubHeadLineSize:: Integer
calcSubHeadLineSize =   Prelude.round k
			where
			f=calcFontSize 
			p= sqrt(phi)
			k= realToFrac(f) * p 


-- | returns Css for sub-Headline Size based on contentWidth.
grSubHeadLineSize::  Css
grSubHeadLineSize = do

	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcSubHeadLineSize
	f = calcFontSize
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo

--  | Calculated secondary text size based on contentWidth.
calcSecondaryText::Integer
calcSecondaryText = Prelude.round k
		    where
		    f=calcFontSize 
		    p= sqrt(phi)
		    k= realToFrac(f) * ( (1.0) / p)


-- | returns Css for Calculated secondary text  size based on contentWidth.
grSecondaryText::  Css
grSecondaryText = do

	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcSecondaryText
	f = calcFontSize
	p = (realToFrac((100*k)) / (realToFrac(f))) 
	c = (realToFrac(k)) / yoo


--  | Calculated default line-height based on contentWidth.
calcBaseLineHeight::Double
calcBaseLineHeight = k
		     where
		     f=calcFontSize 
		     foobar= (sqrt(contentWidth)) / phi
		     h = (realToFrac(f) * phi) ^ 2
		     p =((realToFrac(contentWidth)) / h )
		     foo= realToFrac(f) * (phi - xoo * (1.0 - p))
		     k= foo / foobar


-- | returns Css for Calculated default line-height based on contentWidth.
grBaseLineHeight::  Css
grBaseLineHeight = do

	lineHeight (em c)

	where
	c = calcBaseLineHeight


--  | Calculated a Golden Ratio line-height based on contentWidth.
calcCustomLineHeight:: Integer ->Double
calcCustomLineHeight target =   foo
				where 
				foobar= (sqrt(contentWidth)) / phi
				foo = (realToFrac(target)) / foobar


-- | returns Css for Calculated Golden Ratio line-height based on contentWidth.				
grCustomLineHeight::Integer-> Css
grCustomLineHeight target = lineHeight (em c)
		     	    where
		     	    c = calcCustomLineHeight(target)
