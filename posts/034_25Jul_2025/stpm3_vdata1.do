// This is to emulate the RP fit in stpm2
 
import delimited "/Users/paulsanfilippo/Dropbox/Alfred Neuroscience/zz_test_site/posts/034_25Jul_2025/vdata1.csv"

// Set up data for survival analysis
stset time, failure(status)

// Model
stpm3 i.trt age, scale(lncumhazard) tvc(i.trt) dftvc(3) df(3) eform 

// Predict
predict h1 h2, hazard ci                 ///
               at1(trt 0 age 20)        ///
               at2(trt 1 age 20)        ///
               timevar(0 400, step(5))  ///
			   contrastvar(hr) ///
               frame(f1, replace)		
frames change f1
gen hr = h2/h1	
		
// Plot
twoway (line hr tt, lcolor(blue)) 
