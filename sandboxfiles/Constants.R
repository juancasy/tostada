# Constants.R
# Contiene constantes que intervienen en el proceso
# ---------------------------------------------------------

Constants = c(

	# Radiation constant
	a_rad = 7.5657e-15,
	
	# Gravity constant defined in ESTA working group
	gk = 6.671682e-8,	
		
	# 4*PI
	pi4 = 4*pi, 
	
	# Solar mass (in cgs)
	sm = 1.98919e33,
	
	# Solar radius (in cgs)
	sr = 6.9599e10,
	
	# Solar luminosity (in cgs)
	slumi = 3.846e33,
	
	# Stefan-Bolzman constant (4*a*c/3)
	stefan = 3.021981333e-4
		

)

# Solar mean density
Constants <- append(Constants, 3*Constants["sm"]/(Constants["pi4"]*(Constants["sr"]^3)))
names(Constants)[length(Constants)] <- "rhosol"

# Solar dynamical time
Constants <- append(Constants, sqrt(Constants["sr"]^3/(Constants["gk"]*Constants["sm"])))
names(Constants)[length(Constants)] <- "t_dyn_sol"
