{-|

This module exposes all the functions provided by the Vertical Rhythm 
Module. This is the interface that you would normally want to
use. You can also import the module for the individual hash function.
-}


module verticalRhythmFunctions
	( 
	  establishBaseline
	, baseline
	, rhythm
	, toFontSize
	, adjustFontSize
	, linesForFontSize
	, paddingLeader
	, leader
	, marginLeader
	, trailer
	, paddingtrailer
	, marginTrailer
	, propertyRhythm
	, applySideRhythmBorder
	, rhythmBorders
	, leadingBorder
	, trailingBorder
	, horizontalBorder
	, hBorder	
	) where

import VerticalRhythmFunctions
import Clay
import Control.Monad.State
import Data.Default
import Data.Maybe
