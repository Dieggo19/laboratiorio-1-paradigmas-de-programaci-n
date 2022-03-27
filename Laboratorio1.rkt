#lang racket
;inicio de laboratorio 1 de Paradigmas
;Nombre: Nicolas Espina Valenzuela
;RUT : 19.973.961-1
;Fecha inicio : 25/10/2021
#|Funcion auxiliar que añade un elemento a una lista|#

;Defino el primer TDA que contendrá toda la información de paradigmadocs, en si se representará como una lista
;de la forma [String, Date, EncryptFuncion, DecryptFunction]
;donde String = Nombre de la plataforma, Date= Fecha de creacion, Encrypt y Decrypt Funciones que encriptan y desencriptan la informacion
;también, salta a la vista que es necesario un TDA para Date.

#|----------------------------------------TDA Date----------------------------------------------|#

#|Representacion:
      Este TDA se representara como una lista de numeros, correspondiente a la fecha en formato DD:MM:AA|#

#|Capa constructora TDA Date

      DOM---> Int Dia, int mes, int año
      REC---> lista de elementos, list dia mes año|#

(define Date (lambda (dia mes año)
               (list dia mes año)))

#|----------------------------------------TDA paradigmadocs------------------------------------|#

#|Representacion:
     Para la representacion de este TDA se hara uso de listas, de la forma [STRING,DATE,ENCRYPTFUNCION,DECRYPTFUNCTION, LIST elementos ]
La ultima parte correspondiente a "list elementos" es lo que se ira incluyendo en paradigma docs, ya sean usuarios, posts y otros datos|#

#|Capa constructora TDA paradigmadocs

  DOM--->String (nombre) X Date (TDA Date)(fecha de creacion) X EncryptFunction X Decryptfunction.
  REC---> Una lista de las entradas mas listas de otros elementos (usuariosregistrados..etc )|#

(define paradigmadocs(lambda(string date encryptfn decryptfn)
                       (;cuerpo
                        list string date encryptfn decryptfn #|usuarios registrados|#'()  #|post creados|#'() #|posts compartidos|#'())))


#|capa selectora de TDA paradigmadocs--------------------------------------|#
#|Primeramente defino lo mas sencillo, como obtener el nombre de paradigmadocs, la fecha y las funciones Encrypt y Decrypt|#
#|1: Funcion que obtiene el nombre de paradigmadocs
     REC---> String nombre|#
(define obtenernombreP(lambda(X)
                        (car X)))
#|2: Funcion que obtiene la fecha (date) de paradigmadocs
     REC---> Lista date|#
(define obtenerdateP(lambda(X)
                      (car (cdr X))))

#|3: Funcion que obtiene el Encrypt
    REC---> funcion|#
(define obtenerEnP (lambda(X)
                     (car (cddr X))))

#|4: Funcion que obtiene el Decrypt
     REC---> Funcion|#
(define obtenerDeP(lambda(X)
                    (car (cdddr X))))
#|Ahora bien, como la salida de paradigma docs es en escencia una nueva lista con una lista de listas al final, donde cada espacio de esta sera ocupado
tanto por los usuarios, posts, etc, se definen los siguientes selectores|#
(define obtenerUsersRegistrados(lambda (X)
                        (car (cddddr X) )))
#|Funcion que obtiene los docs de paradigmadocs|#
(define obtenerdocsP(lambda(X)
                      (caddr (cddddr X))))
#|funcion que obtiene los docs compartidos|#
(define obtenerDCP(lambda(X)
                    (car (reverse X))))

#|------------------------------------------TDA User---------------------------------------------|#

#|Representacion
      Para la representacion de un usuario, igualmente se usará una lista|#

#|Capa constructora de TDA User
    DOM--> int id, string user, string pass|#

(define user(lambda (id user pass idlog) 

              (;cuerpo
               list id user pass idlog)))

#|Ahora unas cuantas capas selectoras del TDA user, todas tienen como DOM-->Lista que representa al user|#

#|1: Funcion que obtiene el id del usuario
      REC---> num id|#
(define obteneriduser(lambda(lista)
                       (car lista)))

#|2: Funcion que obtiene el username como string
     REC---> String username|#
(define obtenerusername(lambda(lista)
                         (car (cdr lista))))
#|3: Funcion que obtiene el pass del usuario como un string
     REC---> String pass|#
(define obteneruserpass(lambda(lista)
                         (car (cdr (cdr lista)))))
#|4: Funcion que obtiene el estado logueado de user|#
(define obteneridlog(lambda(lista)
                      (car(cdddr lista))))

#|Defino el -------------------------------------------------TDA Documento--------------------------------------------------------------
  Representacion: al igual que los demas TDA, será una lista de elementos

----Capa Constructora TDA Documento
    DOM:--> id,string, DATE, string, string
    REC:--> LISTA|#
(define documento (lambda( id autor date nombre contenido)
                    (list  id autor date nombre contenido )))


#|Capa selectora--------------------------|#
#|1: Funcion que obtiene el id del documento|#
(define obteneriddoc(lambda(documento)
                      (car documento)))
#|2: Funcion que obtiene el autor del documento |#
(define obtenerautordoc(lambda(documento)
                         (car(cdr documento))))
#|3 : Funcion que obtiene el nombre del documento|#
(define obtenernombredoc(lambda(documento)
                          (car(cdddr documento))) )
#|4: Funcion que obtiene el contenido del documento|#
(define obtenercontenido(lambda(documento)
                          (car(cddddr documento))))


#|1: Funciones de la capa selecotra|#
#|Defino el inicio de paradigmadocs para evaluar el funcionamiento de las funciones|#
(define pd (paradigmadocs "Paragidmadocs" (Date 26 10 2021) "Encrypt" "Decrypt"))

#|Funcion Register---------------------------------------------------------------------------------------------------------------------------------
Se usa recursion natural
es necesario funciones filtro para verificar la existencia o no existencia de un usuario
DOM---> paradigmadocs, username, pass, date
REC---> paradigmadocs  actualizado con el usuario registrado |#

#|Funcion que filtra y busca un usuario en la lista de usuarios registrados|#
#|DOM --> Listadeusuariosregistrados
  REC---> un booleano|#

(define EstaUsuario?(lambda(lista username)
                      
                      (if (null? lista) #f   ( if (eq? (obtenerusername(car lista)) username )  #t  (EstaUsuario? (cdr lista) username)))))




(define register (lambda(paradigmadocs date username password)
                   ;cuerpofuncion
                   (if (null? (obtenerUsersRegistrados paradigmadocs))
                        (list (obtenernombreP paradigmadocs) date (obtenerEnP paradigmadocs)(obtenerDeP paradigmadocs) (list(user 0 username password 0)) '() '())
                        ;else
                        (if (EstaUsuario? (obtenerUsersRegistrados paradigmadocs) username) paradigmadocs  (list (obtenernombreP paradigmadocs) date (obtenerEnP paradigmadocs)(obtenerDeP paradigmadocs)(append (obtenerUsersRegistrados paradigmadocs) (list (user (+ (obteneriduser (car (reverse (obtenerUsersRegistrados paradigmadocs) ))) 1) username password 0))) '() '()) ))))


#|Ejemplo de prueba recursivo|#
(define Paradigma3
  (register (register (register pd (Date 26 210 2021) "Nicolas" "Almendra") (Date 26 10 2021) "YOYO" "askj") (Date 26 10 2021) "Almendreei" "nicollsa"))

#|se necesitan funciones auxiliares tales como un filtro que determina si un usuario esta registrado y es validado correctamente|#
;Funcion que comprueba si el password es valido con el usuario
#|DOM---> lista usuarios, password
  REC---> bool|#
(define EsPassword? (lambda(lista username password)
                      #|cuerpo funcion|#(if (null? lista) #f   ( if (and (eq? (obteneruserpass(car lista)) password ) (eq? (obtenerusername(car lista)) username )) #t  (EsPassword? (cdr lista) username password)))))

#|Funcion Login y derivados---------------------------------------------------------------------------
se necesitan funciones auxiliares tales como un filtro que determina si un usuario esta registrado y es validado correctamente
Funcion que cambia el estado de logueo del usuario ingresado
  DOM--->lista usuarios, username
  REC---> lista usuarios|#
(define cambiarlog(lambda(lista username)
                    #|necesito cambiar el estado de user logueado, lo busco|#
                     (if(null? lista) #f
                        (if (eq? username (obtenerusername(car lista))) (user (obteneriduser(car lista))(obtenerusername (car lista)) (obteneruserpass (car lista))(+ (obteneridlog(car lista)) 1)) (cambiarlog (cdr lista)))
                            )) )


#|Funcion que cambia el estado de logueo en la lista de usuarios, reemplazando el elemento (uso general)
  DOM--->listaUsuarios
  REC---> listaUsuarios|#
(define reemplazar(lambda(lista user)
                    (if(null? lista) '()
                       (if ( eq? (obtenerusername  user) (obtenerusername(car lista)) )
                           (cons user (cdr lista))

                           (reemplazar (cdr lista) user)))))



#|Funcion que actualiza la lista de usuarios de paradigmadocs
  DOM--->Paradigmadocs
  REC-->Paradigmadocs

|#

(define actualizarlistausers(lambda(paradigmadocs username)
                              (list (obtenernombreP paradigmadocs) (obtenerdateP paradigmadocs) (obtenerEnP paradigmadocs) (obtenerDeP paradigmadocs) (reemplazar (obtenerUsersRegistrados paradigmadocs) (cambiarlog (obtenerUsersRegistrados paradigmadocs) username)) (obtenerdocsP paradigmadocs) (obtenerDCP paradigmadocs) )))
 #|DOM--> paradigmadocs x string (username) x string (password) x funcion (funcion de salida)
 REC---> dependiendo si el usuario se logueo correctamente retorna la funcion a ejecutar, sino solo se retorna paradigmadocs|#


(define login (lambda( paradigmadocs username password function)
                #|cuerpo funcion|#(if(EsPassword? (obtenerUsersRegistrados paradigmadocs) username password) (function (actualizarlistausers paradigmadocs username)) (function paradigmadocs))))



#|Funcion create----------------------------------------------------------------|#

#|Funciones auxiliares
 Funcion que obtiene el username del usuario logeuado
 DOM-->Listausuarios
 REC-->Username string|#
(define ObtenerLog(lambda(lista)
                    (if (null? lista) #f (if(eq? (obteneridlog(car lista)) 1) (obtenerusername(car lista)) (ObtenerLog (cdr lista))))))
#|funcion que determina s u usuario está correctamente logueado
 DOM--> ListaUsuarios
 REC---> Boolean|#
(define EstaLog?(lambda(lista username)
                  (if (null? lista) #f
                      (if (eq? (obteneridlog(car lista)) 1) #t (EstaLog? (cdr lista) username)))))
#|Recibe paradigmadocs acualizado desde login, la currificaré
Esta funcion crea un documento en paradigma docs
DOM---> Paradigmadocs, id (int),date(list), autor(string), nombre(string), contenido(string)|#
(define create (lambda(paradigmadocs)
                 (lambda(date nombre contenido)
                   #|para llegar aca el usuario se logueo correctamente, sin embargo, necesito buscar quien se logueo.|#
                    (if (and (null? (obtenerdocsP paradigmadocs)) (EstaLog? (obtenerUsersRegistrados paradigmadocs) (ObtenerLog (obtenerUsersRegistrados paradigmadocs)) ))
                       
                           #|creo el post, definiendo nuevamente a paradigmadocs|# (list(list (documento 0 (ObtenerLog (obtenerUsersRegistrados paradigmadocs)) date nombre contenido)))

                                           "else"))))