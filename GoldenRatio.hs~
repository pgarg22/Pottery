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


calcFontSize:: Integer
calcFontSize = Prelude.round k
		where
		k= sqrt(contentWidth/phi) 

grFontSize:: Css
grFontSize = do
	
	fontSize(px k)
	fontSize (em p)
	lineHeight (em c)

	where
	k = calcFontSize
	p = (realToFrac((100*k)) / (realToFrac(k))) 
	c = (realToFrac(k)) / yoo

calcTitleSize:: Integer
calcTitleSize = Prelude.round k
		where
		f=calcFontSize 
		p= phi ^ 2
		k= realToFrac(f) * p 

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


calcHeadLineSize:: Integer
calcHeadLineSize= Prelude.round k
		  where
		  f=calcFontSize 
		  p= phi ^ 1
		  k= realToFrac(f) * p 

 
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

 
 
calcSubHeadLineSize:: Integer
calcSubHeadLineSize =   Prelude.round k
			where
			f=calcFontSize 
			p= sqrt(phi)
			k= realToFrac(f) * p 

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


calcSecondaryText::Integer
calcSecondaryText = Prelude.round k
		    where
		    f=calcFontSize 
		    p= sqrt(phi)
		    k= realToFrac(f) * ( (1.0) / p)

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



calcBaseLineHeight::Double
calcBaseLineHeight = k
		     where
		     f=calcFontSize 
		     foobar= (sqrt(contentWidth)) / phi
		     h = (realToFrac(f) * phi) ^ 2
		     p =((realToFrac(contentWidth)) / h )
		     foo= realToFrac(f) * (phi - xoo * (1.0 - p))
		     k= foo / foobar

grBaseLineHeight::  Css
grBaseLineHeight = do

	lineHeight (em c)

	where
	c = calcBaseLineHeight



calcCustomLineHeight:: Integer ->Double
calcCustomLineHeight target =   foo
				where 
				foobar= (sqrt(contentWidth)) / phi
				foo = (realToFrac(target)) / foobar
				
grCustomLineHeight::Integer-> Css
grCustomLineHeight target = lineHeight (em c)
		     	    where
		     	    c = calcCustomLineHeight(target)
