#' Constants frequently used in asteroseismology
#'
#' A dataset containing the most frequent constant used in asteroseismology
#' and stellar science.
#'
#' @format A data frame containing 9 constants:
#' \describe{
#' \tabular{rl}{
#'    \strong{a_rad} \tab radiation constant \cr
#'    \strong{gk} \tab Gravitational constant as defined in ESTA working group \cr
#'    \strong{pi4} \tab 4 times pi \cr
#'    \strong{smass} \tab solar mass in cgs \cr
#'    \strong{sradius} \tab solar radius in cgs \cr
#'    \strong{slum} \tab solar luminosity (in cgs) \cr
#'    \strong{stefbol} .\tab Stefan-Bolzman constant (4*a*c/3) \cr
#'    \strong{srho} \tab solar meand density (in cgs) \cr
#'    \strong{sdynt} \tab solar dynamical time
#'  }
#' }
#'
#' @source \url{http://www.diamondse.info/}
"constants"

constants = data.frame(

# Radiation constant
	a_rad = 7.5657e-15,

	# Gravity constant defined in ESTA working group
	gk = 6.671682e-8,

	# 4*PI
	pi4 = 4*pi,

	# Solar mass (in cgs)
	smass = 1.98919e33,

	# Solar radius (in cgs)
	sradius = 6.9599e10,

	# Solar luminosity (in cgs)
	slum = 3.846e33,

	# Stefan-Bolzman constant (4*a*c/3)
	stefbol = 3.021981333e-4


)

# Solar mean density
constants$srho <- 3*constants$smass/(constants$pi4*constants$sradius^3)

# Solar dynamical time
constants$sdynt <- sqrt(constants$sradius^3/(constants$gk*constants$smass))
