{-|

This module exposes all the functions provided by the Golden Ratio 
Module. This is the interface that you would normally want to
use. You can also import the module for the individual hash function.
-}


module GoldenRatio
	( 
	  grTitleSize : Returns CSS of title size based on content width. 
	, grHeadlineSize : Returns CSS of headline size based on content width. 
	, grSubHeadlineSize : Returns CSS of subheadline size based on content width. 
	, grFontSize : Returns CSS of Font size based on content width. 
	, grSecondaryText : Returns CSS of Secondary Text based on content width. 
	, grBaseLineheight : Returns CSS of default line-height based on content width. 
	, grCustomLineheight	) where

import GoldenRatioFunctions

