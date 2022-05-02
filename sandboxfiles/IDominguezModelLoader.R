# Clase CesamModelLoader

IDominguezModelLoader <- R6Class("IDominguezModelLoader",
	private = list(
	
		# Objeto GracoEngine que crea este objeto 
		graco = NULL, 
							
		# Método auxiliar que obtiene un vector de números a partir de los datos de un bloque de líneas de números de CESAM
		readDataBlock = function(fichero, intLineas){
			datos <- integer() # vector vacio
			lineas <- readLines(con = fichero, n = intLineas) 			
			for(linea in lineas){				
				s <- sub("^\\s+", "", linea) # retira el primer espacio en blanco de la línea				
				v <- as.numeric(unlist(strsplit(gsub("([0-9])-([0-9])","\\1 -\\2",s)," "))) # separa los - que juntan a veces los números				
				datos <- append(datos,v)
			}			
			return(datos)
		}		
	), 
	
	public = list(
	
		# Inicializa el cargador
		initialize = function(objGracoEngine){
			private$graco <- objGracoEngine			
		},
		
		# Carga el modelo de estrella de Cesam
		# NOTA: Solo carga los datos necesarios para el problema adiabático
		Load = function(strFile){
		
			e <- StarModel$new(private$graco)			
			fichero <- file(description = strFile,open = "r")	
				
			# cabecera
			linea <- readLines(con = fichero, n = 5)
			noAdiabaticas <- FALSE
			driver <- "IDominguez"
			version <- "V1.0"
			contenido <- NA
			
			indices <- NULL
			if(noAdiabaticas){
				contenido <- "Non Adiabatic"
				# define los índices de las variables en el caso no adiabático
				indices <- c(m=2, r=3, t=4, rho=5, p=6, gamma1=7, cs2=8, bv=9, h=10, he=11, c=12, o=13)
			}else{
				contenido <- "Adiabatic"
				# define los índices de los datos necesarios en el caso adiabático
				indices <- c(m=2, r=3, t=4, rho=5, p=6, gamma1=7, cs2=8, bv=9, h=10, he=11, c=12, o=13)
			}			
						
			numeros <- strsplit(linea[2]," ")
			e$m <- as.integer(numeros[[1]][30])-1 # Quitamos la última porque es el centro de la estrella y da problemas numéricos (no se lee esa capa)				
			numDatosGlobales <- NULL # nº de datos que vienen en la parte de datos globales
			numElemQuim <- 4 # nº de elementos químicos que se incluyen en el modelo
			numDatosCapa <- 8 # nº de datos, no elementos químicos, que se incluyen por capa
			e$rfotom <- as.numeric(numeros[[1]][16]) * Constants["sr"]
			e$rmasa <- as.numeric(numeros[[1]][22]) * Constants["sm"]
			e$rlumi <- as.numeric(numeros[[1]][10]) * Constants["slumi"]
			e$al <- 0
			e$edad <- as.numeric(numeros[[1]][4])
			e$rot <- 0
			e$rlogg <- as.numeric(numeros[[1]][19])
			  
			# notificación de los datos iniciales
			private$graco$SendEvent("StarModelLoader_Init",list(driver=driver, version=version, layers=e$m, content=contenido))			
									
			# Reserva de memoria			
			e$r <- integer(e$m)			
			e$vg <- integer(e$m)
			e$c1 <- integer(e$m)
			e$aast <- integer(e$m)
			e$u <- integer(e$m)
			e$q <- integer(e$m)
			e$cs2 <- integer(e$m)
			e$p <- integer(e$m)
			e$gamma <- integer(e$m)
			e$rho <- integer(e$m)
			e$grav <- integer(e$m)
			temp <- integer(e$m)
			
			# Lee los arrays de datos			
			numLineas <- 1
			avisos <- round((e$m-1)*0.25)
			for(i in 1:(e$m)){			
			  datos <- private$readDataBlock(fichero, numLineas)
			  datos <- datos[!is.na(datos)]
			  e$r[i] <- datos[indices["r"]]
			  e$q[i] <- datos[indices["m"]]
			  temp[i] <- datos[indices["t"]]
			  e$p[i] <- datos[indices["p"]]
			  e$rho[i] <- datos[indices["rho"]]
			  e$gamma[i] <- datos[indices["gamma1"]]
			  e$cs2[i] <- datos[indices["cs2"]]
			  e$aast[i] <- datos[indices["bv"]]				
			  if(i%%avisos==0) private$graco$SendEvent("StarModelLoader_Progress",list(comp=e$m-i, total=e$m, porc=((e$m-i)*100/(e$m-1))))			
			}
			private$graco$SendEvent("StarModelLoader_Progress",list(porc=75, comp=e$m, total=e$m))
			
			# Conversión de los datos a logaritmo
			e$q <- e$q * Constants["sm"]
			e$r <- e$r * Constants["sr"]
			e$grav <- Constants["gk"]*e$q/(e$r^2)
	    e$q <- log(e$q)
			e$r <- log(e$r)
	    e$p <- log(10^e$p)
	    e$rho <- log(10^e$rho)
	    temp <- log(10^temp)
			
			# Completa los datos del modelo	
			e$rr <- exp(e$r)/e$rfotom
			e$c1 <- exp(3*e$r-e$q)*e$rmasa/(e$rfotom^3)
			e$vg <- Constants["gk"]*exp(e$q-e$r)/e$cs2	
			e$u <- 4*pi*exp(e$rho+3*e$r-e$q)
			e$ct1 <- e$c1[1]			
			e$R <- e$r[e$m]
			e$rn2 <- e$grav*e$aast/exp(e$r)
			e$rnn <- e$rn2*(e$rfotom^3)/(Constants["gk"]*e$rmasa)
			
			# fin			
			close(fichero)
			private$graco$SendEvent("StarModelLoader_Progress",list(porc=100, comp=e$m, total=e$m))			
			return(e)
		}
	)
)

# Comprueba si un archivo posee el formato adecuado para ser cargado con este driver
IDominguezModelLoader.Test <- function(path, isUrl){
	r <- FALSE	
	c <- NULL			
	if(!isUrl) c <- file(path,"r")
	else c <- url(path)
	linea <- readLines(c,n=2)
	close(c)
	r <- grepl(" AGE:", linea[2])	
	return(r)
}
