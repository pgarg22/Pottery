module VericalRhythm where
import Control.Monad.Reader
import Clay
import Data.Function
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Data.String
import Data.List
import qualified Data.Text.Lazy.IO as L
import Data.IORef
import Data.Global
import Control.Concurrent
import Data.Maybe


base_font_size = declareIORef "some1" 16 :: IORef Integer
base_line_height = declareIORef "some2" 24 :: IORef Integer
rhythm_border_style = declareIORef "some3" "solid" :: IORef String
browser_default_font_size = declareIORef "some4" 16 :: IORef Integer
relative_font_sizing = declareIORef "some5" True :: IORef Bool
round_to_nearest_half_line = declareIORef "some6" False :: IORef Bool
min_line_padding = declareIORef "some7" 2 :: IORef Integer




establish_baseline :: IO Css
establish_baseline = do
		       k <- readIORef  base_font_size
		       l <- readIORef browser_default_font_size 
		       s<- rhythm 1.0 k 0
		       let p= (realToFrac (k*100)) / (realToFrac l)
		       return (do 
			     
		 	          html?
				       fontSize ( pct p)      
				  html ?
				    do
				       fontSize      (px k)
				       lineHeight (em s))





rhythm:: Double->Integer->Integer->IO Double
rhythm lines font_size offset  =do 	h <- readIORef base_line_height
					return ((lines*realToFrac((h - offset))) / realToFrac (font_size))
