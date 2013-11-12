module VericalRhythm where
import Clay


data Config a = Co a 


newtype ST s a = ST (s -> (s,a))


instance Monad Config where
  return x = Co x 
  Co a >>= g = g a

establish_baseline :: Integer-> Config Css
establish_baseline base_font_size = do
		       let k=rhythm 1.0 base_font_size 0
		       let p= (realToFrac (2*100)) / (realToFrac 16)
		       return (do 
			     
		 	          html?
				       fontSize ( pct p)      
				  html ?
				    do
				       fontSize      (px base_font_size)
				       lineHeight (em k))





rhythm:: Double->Integer->Integer->Double
rhythm lines font_size offset  =do 	let h = 2
					((lines*realToFrac((h - offset))) / realToFrac (font_size))

