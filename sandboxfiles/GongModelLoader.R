# Clase CesamModelLoader

GongModelLoader <- R6Class("GongModelLoader",
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
		
		# Carga el modelo de estrella en formato Gong
		# NOTA: Solo carga los datos necesarios para el problema adiabático
		Load = function(strFile){
		
			e <- StarModel$new(private$graco)			
			fichero <- file(description = strFile,open = "r")	
				
			# cabecera
			linea <- readLines(con = fichero, n = 5) 
			noAdiabaticas <- FALSE			
			driver <- "GONG"			
			contenido <- "Adiabatic"
			
			# información del modelo
			numeros <- strsplit(linea[5]," {1,20}")[[1]]	
			numeros <- numeros[2:5]
			e$m <- as.integer(numeros[1])-1 # Quitamos la última porque es el centro de la estrella y da problemas numéricos (no se lee esa capa)				
			numDatosGlobales <- as.integer(numeros[2]) # nº de datos que vienen en la parte de datos globales
			numDatosCapa <- as.integer(numeros[3]) # nº de datos que se incluyen por capa
			version <- numeros[4]			
									
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
						
			# define los índices de los datos necesarios en el caso adiabático
			indices <- c(rmasa=1, rfotom=2, rlumi=3, al=6, edad=13,	# datos globales
				r=1, q=2, p=4, rho=5, gamma=10, aast=15)			# datos por capa
			
			# carga los datos globales
			datos <- private$readDataBlock(fichero, ceiling(numDatosGlobales/5)) # el bloque usa líneas de 5 datos cada una y en total hay numDatosGlobales datos
			e$rmasa <- datos[indices["rmasa"]]		
			e$rfotom <- datos[indices["rfotom"]]
			e$rlumi <- log(datos[indices["rlumi"]]/Constants["slumi"])
			e$al <- datos[indices["al"]]
			e$edad <- datos[indices["edad"]]
			e$rlogg <- log((Constants["gk"]*e$rmasa)/(e$rfotom^2))
	
			# Lee los arrays de datos
			numLineas <- ceiling(numDatosCapa/5) # El archivo adiabático tiene numDatosCapa datos por cada capa y hay numDatosCapa datos en cada línea
			avisos <- round((e$m-1)*0.25) # usado para mandar eventos a la interfaz de usuario
			for(i in (e$m):1){			
				datos <- private$readDataBlock(fichero, numLineas)
				e$r[i] <- datos[indices["r"]]
				e$q[i] <- datos[indices["q"]]
				e$p[i] <- datos[indices["p"]]
				e$rho[i] <- datos[indices["rho"]]
				e$gamma[i] <- datos[indices["gamma"]]
				e$aast[i] <- datos[indices["aast"]]				
				if(i%%avisos==0) private$graco$SendEvent("StarModelLoader_Progress",list(comp=e$m-i, total=e$m, porc=((e$m-i)*100/(e$m-1))))			
			}
			private$graco$SendEvent("StarModelLoader_Progress",list(porc=75, comp=e$m, total=e$m))			
				
			# Conversión de los datos a logaritmo
			e$r <- log(e$r)
			e$q <- log(e$rmasa * exp(e$q))		
			e$p <- log(e$p)
			e$rho <- log(e$rho)
					
			# Completa los datos del modelo	
			e$rr <- exp(e$r)/e$rfotom
			e$c1 <- exp(3*e$r-e$q)*e$rmasa/(e$rfotom^3)
			e$cs2 <- e$gamma*exp(e$p-e$rho)
			e$vg <- Constants["gk"]*exp(e$q-e$r)/e$cs2	
			e$u <- 4*pi*exp(e$rho+3*e$r-e$q)
			e$ct1 <- e$c1[1]			
			e$R <- e$r[e$m]
			e$grav <- Constants["gk"]*exp(e$q-2*e$r)
			e$rn2 <- e$grav*e$aast/exp(e$r)
			e$rnn <- e$rn2*(e$rfotom^3)/(Constants["gk"]*e$rmasa)					
			
			# fin			
			close(fichero)
			private$graco$SendEvent("StarModelLoader_Progress",list(porc=100, comp=e$m, total=e$m))			
			return(e)
		}
	)
)

# Comprueba si un archivo posee el formato adecuado para ser cargado con este driver. 
# Solo comprobamos que en la línea 5 hay 4 datos
GongModelLoader.Test <- function(path, isUrl){
	r <- FALSE	
	c <- NULL			
	if(!isUrl) c <- file(path,"r")
	else c <- url(path)
	linea <- readLines(c,n=5)
	close(c)
	numeros <- strsplit(linea[5]," {1,20}")[[1]]		
	if(length(numeros)==5) numeros <- numeros[2:5]
	r <- length(numeros)==4	
	return(r)
}
