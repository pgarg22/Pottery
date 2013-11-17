module VerticalRhythm2 where
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


--declareChan "ch" [ t| Maybe Char |]

--main = do
  --  writeChan ch (Just 'x')
    --readChan  ch >>= print
--ref = declareIORef "some" 17 :: IORef Integer

base_font_size = declareIORef "some1" 16 :: IORef Integer
base_line_height = declareIORef "some2" 24 :: IORef Integer
rhythm_border_style = declareIORef "some3" "solid" :: IORef String
browser_default_font_size = declareIORef "some4" 16 :: IORef Integer
relative_font_sizing = declareIORef "some5" True :: IORef Bool
round_to_nearest_half_line = declareIORef "some6" False :: IORef Bool
min_line_padding = declareIORef "some7" 2 :: IORef Integer




--initialize :: Integer-> ()
--initialize n = do
		 --writeIORef ref n
		 --let ref = declareIORef "my-global-var" 17 :: IORef Int
--		 return $ Config (n) (24) ("solid") (16) (True) (False) (2)

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

adjust_font_size_to :: Integer-> Maybe Double-> Maybe Integer->IO Css
adjust_font_size_to to_size (Just lines) (Just from_size) =do 
								let k= (realToFrac (to_size*100)) / (realToFrac from_size)
								s<- rhythm lines to_size 0 
								return (do 
									   fontSize ( pct k)
									   lineHeight (em s)) 
 
adjust_font_size_to to_size Nothing  Nothing= do
						lines<- lines_for_font_size (to_size)
						from_size<- readIORef  base_font_size
						let k= (realToFrac (to_size*100)) / (realToFrac from_size)
						s<-rhythm lines to_size 0 
						return (do fontSize ( pct k)
							   lineHeight (em s)) 

adjust_font_size_to to_size (Just lines)  Nothing= do
							from_size<- readIORef  base_font_size
							let k= (realToFrac (to_size*100)) / (realToFrac from_size)
							s<-rhythm lines to_size 0 
							return (do fontSize ( pct k)
								   lineHeight (em s)) 


adjust_font_size_to to_size Nothing  (Just from_size)= do
					 			lines<- lines_for_font_size (to_size)
									
								let k= (realToFrac (to_size*100)) / (realToFrac from_size)
								s<- rhythm lines to_size 0 
								return (do fontSize ( pct k)
									   lineHeight (em s)) 



lines_for_font_size:: Integer->IO Double 
lines_for_font_size font_size= do
				r<- readIORef round_to_nearest_half_line
				h<- readIORef  base_line_height
				m<- readIORef min_line_padding
				let p1= (realToFrac (font_size*2) / realToFrac(h))
				let p2= (realToFrac (font_size) / realToFrac(h))
				let a= ceiling p1
				let b= ceiling p2
				let lines = declareIORef "some7" 1 :: IORef Integer
  				if r
        			then do
					writeIORef lines a 
       			        else do 
					writeIORef lines b
				l<- readIORef lines
				let l2 = realToFrac(l)
				let k2= ((realToFrac (l*h))) - realToFrac (font_size) 		
  				if k2 < (realToFrac(m*2))	
    				then do 
					if r
					then return(l2 + 0.5)
					else return(l2 + 1.0)
				else return l2

adjust_leading_to:: Double-> Maybe Integer-> IO Css
adjust_leading_to lines (Just font_size) = do 	  let k = font_size
					   	  s<-rhythm lines k 0  
		       			   	  return (do 
								lineHeight(em s))


adjust_leading_to lines Nothing = do 	   k <- readIORef  base_font_size
					   s<- rhythm lines k 0 
		       			   return (do 
							lineHeight(em s))

leader:: Maybe Double-> Maybe Integer -> Maybe String -> IO Css
leader (Just lines) (Just font_size) (Just "margin")= do  h<- readIORef  base_line_height
						          r<-rhythm lines font_size 0
							  return( do
									marginTop (em r))


leader (Just lines) (Just font_size) (Just "padding")= do h<- readIORef  base_line_height
						          r<-rhythm lines font_size 0
							  return( do
									paddingTop (em r))
							


leader (Nothing) (Just font_size) (Just "margin")= do  	h<- readIORef  base_line_height
						        r<-rhythm 1.0 font_size 0
							return( do
									marginTop (em r))


leader (Nothing) (Just font_size) (Just "padding")= do  h<- readIORef  base_line_height
						        r<-rhythm 1.0 font_size 0
							return( do
									paddingTop (em r))
							

leader (Just lines) Nothing (Just "margin")= do  	h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm lines font_size 0
							return( do
									marginTop (em r))


leader (Just lines) Nothing (Just "padding")= do 	h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm lines font_size 0
							return( do
									paddingTop (em r))
							

leader (Just lines) (Just font_size) (Nothing)= do  	  h<- readIORef  base_line_height
						          r<-rhythm lines font_size 0
							  return( do
									marginTop (em r))



leader (Nothing) Nothing (Just "margin")= do  		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm 1.0 font_size 0
							return( do
									marginTop (em r))


leader (Nothing) Nothing (Just "padding")= do 		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm 1.0 font_size 0
							return( do
									paddingTop (em r))
							

leader (Just lines) Nothing (Nothing)= do  		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm lines font_size 0
							return( do
									marginTop (em r))




leader (Nothing) (Just font_size) (Nothing)= do  	h<- readIORef  base_line_height
						        r<-rhythm 1.0 font_size 0
							return( do
									marginTop (em r))


leader (Nothing ) Nothing (Nothing)= do  		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm 1.0 font_size 0
							return( do
									marginTop (em r))


padding_leader:: Maybe Double-> Maybe Integer -> IO Css
padding_leader (Just lines) (Just font_size) = do  h<- readIORef  base_line_height
				               	   r<-rhythm lines font_size 0
				       	   	   return( do
								paddingTop (em r))


padding_leader (Nothing) (Just font_size) = do  h<- readIORef  base_line_height
				                r<-rhythm 1.0 font_size 0
				       	   	return( do
								paddingTop (em r))

padding_leader (Just lines) Nothing = do	h<- readIORef  base_line_height
						font_size<- readIORef base_font_size
				    	    	r<-rhythm lines font_size 0
						return( do
								paddingTop (em r))
	


padding_leader (Nothing) Nothing = do 	h<- readIORef  base_line_height
					font_size<- readIORef base_font_size
				        r<-rhythm 1.0 font_size 0
					return( do
							paddingTop (em r))


margin_leader:: Maybe Double-> Maybe Integer -> IO Css
margin_leader (Just lines) (Just font_size) = do  h<- readIORef  base_line_height
				               	  r<-rhythm lines font_size 0
				       	   	  return( do
								marginTop (em r))


margin_leader (Nothing) (Just font_size) = do   h<- readIORef  base_line_height
				                r<-rhythm 1.0 font_size 0
				       	   	return( do
								marginTop (em r))

margin_leader (Just lines) Nothing = do		h<- readIORef  base_line_height
						font_size<- readIORef base_font_size
				    	    	r<-rhythm lines font_size 0
						return( do
								marginTop (em r))
	


margin_leader (Nothing) Nothing = do 	h<- readIORef  base_line_height
					font_size<- readIORef base_font_size
				        r<-rhythm 1.0 font_size 0
					return( do
							marginTop (em r))




trailer:: Maybe Double-> Maybe Integer -> Maybe String -> IO Css
trailer (Just lines) (Just font_size) (Just "margin")= do h<- readIORef  base_line_height
						          r<-rhythm lines font_size 0
							  return( do
									marginBottom (em r))


trailer (Just lines) (Just font_size) (Just "padding")= do h<- readIORef  base_line_height
						           r<-rhythm lines font_size 0
							   return( do
									paddingBottom (em r))
							


trailer (Nothing) (Just font_size) (Just "margin")= do  h<- readIORef  base_line_height
						        r<-rhythm 1.0 font_size 0
							return( do
									marginBottom (em r))


trailer (Nothing) (Just font_size) (Just "padding")= do  h<- readIORef  base_line_height
						         r<-rhythm 1.0 font_size 0
							 return( do
									paddingBottom (em r))
							

trailer (Just lines) Nothing (Just "margin")= do  	h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm lines font_size 0
							return( do
									marginBottom (em r))


trailer (Just lines) Nothing (Just "padding")= do 	h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm lines font_size 0
							return( do
									paddingBottom (em r))
							

trailer (Just lines) (Just font_size) (Nothing)= do  	  h<- readIORef  base_line_height
						          r<-rhythm lines font_size 0
							  return( do
									marginBottom (em r))



trailer (Nothing) Nothing (Just "margin")= do  		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm 1.0 font_size 0
							return( do
									marginBottom (em r))


trailer (Nothing) Nothing (Just "padding")= do 		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm 1.0 font_size 0
							return( do
									paddingBottom (em r))
							

trailer (Just lines) Nothing (Nothing)= do  		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm lines font_size 0
							return( do
									marginBottom (em r))




trailer (Nothing) (Just font_size) (Nothing)= do  	h<- readIORef  base_line_height
						        r<-rhythm 1.0 font_size 0
							return( do
									marginBottom (em r))


trailer (Nothing ) Nothing (Nothing)= do  		h<- readIORef  base_line_height
							font_size<- readIORef base_font_size
						        r<-rhythm 1.0 font_size 0
							return( do
									marginBottom (em r))


padding_trailer:: Maybe Double-> Maybe Integer -> IO Css
padding_trailer (Just lines) (Just font_size) = do  h<- readIORef  base_line_height
				                    r<-rhythm lines font_size 0
				       	   	    return( do
								paddingBottom (em r))


padding_trailer (Nothing) (Just font_size) = do  h<- readIORef  base_line_height
				                 r<-rhythm 1.0 font_size 0
				       	   	 return( do
								paddingBottom (em r))

padding_trailer (Just lines) Nothing = do	h<- readIORef  base_line_height
						font_size<- readIORef base_font_size
				    	    	r<-rhythm lines font_size 0
						return( do
								paddingBottom (em r))
	


padding_trailer (Nothing) Nothing = do 	h<- readIORef  base_line_height
					font_size<- readIORef base_font_size
				        r<-rhythm 1.0 font_size 0
					return( do
							paddingBottom (em r))


margin_trailer:: Maybe Double-> Maybe Integer -> IO Css
margin_trailer (Just lines) (Just font_size) = do h<- readIORef  base_line_height
				               	  r<-rhythm lines font_size 0
				       	   	  return( do
								marginBottom (em r))


margin_trailer (Nothing) (Just font_size) = do  h<- readIORef  base_line_height
				                r<-rhythm 1.0 font_size 0
				       	   	return( do
								marginBottom (em r))

margin_trailer (Just lines) Nothing = do	h<- readIORef  base_line_height
						font_size<- readIORef base_font_size
				    	    	r<-rhythm lines font_size 0
						return( do
								marginBottom (em r))
	


margin_trailer (Nothing) Nothing = do 	h<- readIORef  base_line_height
					font_size<- readIORef base_font_size
				        r<-rhythm 1.0 font_size 0
					return( do
							marginBottom (em r))

propertyrhythm (Just lead) (Just padding_lead) (Just padding_trail) (Just trail) (Just font_size)= do
							l<-leader (Just lead) (Just font_size) (Nothing)
							pl<-padding_leader (Just padding_lead) (Just font_size)
							pt<-padding_trailer (Just padding_trail) (Just font_size)
							t<- trailer (Just trail) (Just font_size) (Nothing)
							return( do
									l
									pl
									pt
									t)


apply_side_rhythm_border:: String-> Maybe Integer-> Maybe Double-> Maybe Integer -> Maybe String ->Css
apply_side_rhythm_border side (width1) (lines1) (font_size1) (border_style1)= do 
					let width= val width1 1
					let lines= val lines1 1.0
					f<- readIORef base_font_size
					let font_size = val font_size1 f 
					b<- readIORef rhythm_border_style
					let b1= val border_style1 b
					let border_style= string_to_stroke b1
					r<- rhythm lines font_size width
					let w= realToFrac(width) / realToFrac(font_size)
					if side == "Left"
					then return(do 	borderLeft border_style (em w) red
							paddingLeft (em r)) 
					else if side == "Top"
					then return(do 	borderTop border_style (em w) red
							paddingTop (em r)) 
					else if side == "Bottom"
					then return(do 	borderBottom border_style (em w) red
							paddingBottom (em r)) 
					else if side == "Right"
					then return(do 	borderRight border_style (em w) red
					 		paddingRight (em r)) 
					else return (do borderRight border_style (em 0) red
							paddingRight (em 0))
val::Maybe a -> a-> a
val (Just x) y = x;
val Nothing y = y;

string_to_stroke:: String->Stroke
string_to_stroke a = solid

rhythm_borders (Just width) (Just lines)(Just font_size) (Just border_style)= do
  					r<- rhythm lines font_size width
					let w= realToFrac(width) / realToFrac(font_size)
					
					return(do 	border border_style (em w) red
							padding (em r) (em r) (em r) (em r)) 



leading_border (width) (lines)(font_size) (border_style)= return( apply_side_rhythm_border "top" width lines font_size border_style)


trailing_border (width) (lines)(font_size) (border_style)= return( apply_side_rhythm_border "Bottom" width lines font_size border_style)

horizontal_border (width) (lines)(font_size) (border_style)= return( do leading_border width lines font_size border_style
									trailing_border width lines font_size border_style)

h_border width lines font_size border_style= return horizontal_border width lines font_size border_style
