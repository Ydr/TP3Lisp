;Instituto Tecnologico de Costa Rica
;Lenguajes de programacion
;Tarea programada 3
;Frank Brenes, Mercedes Escalante, Yader Morales


(defparameter *tagsMetadatos* '(AENC APIC COMM COMR ENCR EQUA ETCO GEOB GRID IPLS LINK MCDI MLLT OWNE PRIV PCNT POPM POSS
						   RBUF RVAD RVRB SYLT SYTC TALB TBPM TCOM TCON TCOP TDAT TDLY TENC TEXT TFLT TIME TIT1 TIT2
						   TIT3 TKEY TLAN TLEN TMED TOAL TOFN TOLY TOPE TORY TOWN TPE1 TPE2 TPE3 TPE4 TPOS TPUB TRCK
						   TRDA TRSN TRSO TSIZ TSRC TSSE TYER TXXX UFID USER USLT WCOM WCOP WOAF WOAR WOAS WORS WPAY
						   WPUB WXXX ASPI EQU2 RVA2 SEEK SIGN TDEN TDOR TDRC TDRL TDTG TIPL TMCL TMOO TPRO TSOA TSOP 
						   TSOT TSST))
						   
(defparameter *contador* 0) ;key a utilizar en la tabla hash			   
						   
(defparameter *ht* (make-hash-table)) ;Realiza la tabla hash

(defclass archivo-musica ();Se define la clase archivo-musica que va a contener el titulo, artista y genero del header mp3
	((titulo
		:initarg :titulo
		:initform (error "Debe tener un titulo de cancion"));Al menos un titulo debe existir para crear la instancia
	 (artista
		:initarg :artista
		:initform "")
	 (genero
		:initarg :genero
		:initform "")))

(defmethod getTitulo((archivo-musica archivo-musica))
	(slot-value archivo-musica 'titulo))
	
(defmethod getArtista((archivo-musica archivo-musica))
	(slot-value archivo-musica 'artista))
	
(defmethod getGenero((archivo-musica archivo-musica))
	(slot-value archivo-musica 'genero))
	


(defun component-present-p (value); Verifica si se encuentra el valor enviado como parametro
  (and value
	(not (eql value :unspecific))))

(defun directory-pathname-p  (p);Verifica la ruta de los archivos con ayuda de la funcion component-present-p
  (and(not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))p))

(defun pathname-as-directory (name);Se envia un path y lo convierte a directorio, en caso que las carpetas se encuentren en niveles
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname) pathname)))
      
(defun list-directory (dirname);Verifica las direcciones enviadas por parametro
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

(defun directory-wildcard (dirname);Se envia una direccion y se encarga de crear un pathname
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun walk-directory (dirname fn &key directories (test (constantly t))); Recibe el directorio enviado por cargarMp3 y accesa los archivos especificados, en este caso mp3
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

(defun buscarMP3dir (file) ;Funcion para corroborar que los archivos a cargar sean MP3
  (and (not (directory-pathname-p file))
   (string-equal "mp3" (pathname-type file))))
   

(defun recorreArchivos (file)
	(recorreArchivos-aux file 600))

 
(defun recorreArchivos-aux (file  length);Abre los archivos y recorre los metadatos, cortando la informacion util y guardandola en el hashtable
 (let ((in (open file :direction 
        :input 
        :element-type 'unsigned-byte)))
        
		(let ((string (make-string length)))
      (dotimes (i length)
        (setf (char string i) (code-char (read-byte in))));parte que codifica de bytes a chars
        
        (setf (gethash *contador* *ht*) (make-instance 'archivo-musica 
				:titulo (if (NULL (buscarTagNormal "TIT2" string)) "No hay" 
					(subseq string (+ (buscarTagNormal "TIT2" string) 4) (buscarTag "TIT2" string)))
				:artista (if (NULL (buscarTagNormal "TPE1" string)) "No hay" 
					(subseq string (+ (buscarTagNormal "TPE1" string) 4) (buscarTag "TPE1" string)))					
				:genero (if (NULL (buscarTagNormal "TCON" string)) "No hay" 
					(subseq string (+ (buscarTagNormal "TCON" string) 4) (buscarTag "TCON" string)))
				))
		(setf *contador* (+ *contador* 1) ))))
     
(defun buscarTagNormal(tag string)
	(search tag string))     
     
(defun buscarTag (tag string)
	(buscarTagAux string (search tag string) 600))

(defun buscarTagAux (string posi count)
	(loop for i in *tagsMetadatos* do (setf count (buscarAux i string posi count)))
	count)

(defun buscarAux (tag string posi count)
	(cond ((NULL (search (string tag) string)) count)
			((and (> (search (string tag) string) posi) (< (search (string tag) string) count)) (search (string tag) string))
			(T count))) 

(defun escribe-archivo (nombre-archivo contenido) 
;escribe en un archivo las consultas realizadas
; contenido es el string de la busqueda
  (format t "Datos leidos ~%")(with-open-file (stream nombre-archivo
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create )
  (format stream contenido))
   nombre-archivo )
 

;------------------------------------------------------------------------------------------------------------------
(defun cargarMP3(fichero)  ;Funcion principal, la primera que se debe llamar, carga archivos mp3 de una direccion
	(walk-directory fichero #'recorreArchivos :test #'buscarMP3dir)(guardarTxt ""))

;------------------------------------------------------------------------------------------------------------------

(defun guardarTxt(todo); Imprime resultados de todas las canciones
	(maphash #'(lambda (key value)(persistencia value todo)) *ht*))

(defun persistencia (file todo)
	(escribe-archivo "Persistencia" "")
	(if (search todo (getTitulo file))
	(format t "Titulo: ~s Artista: ~s  Genero: ~s ~%" (getTitulo file)(getArtista file)(getGenero file))))

	
(defun buscar-titulo(titulo); Busca por titulo en la tabla hash e imprime resultados
	(maphash #'(lambda (key value)(printTitulo value titulo)) *ht*))

(defun printTitulo (file titulo)
	(if (search titulo (getTitulo file)) 
	(format t "Titulo: ~s Artista: ~s  Genero: ~s ~%" (getTitulo file)(getArtista file)(getGenero file))))



(defun buscar-artista(artista); Busca por artista en la tabla hash e imprime resultados
	(maphash #'(lambda (key value)(printArtista value artista)) *ht*))

(defun printArtista (file artista)
	(if (search artista (getArtista file)) 
	(format t "Artista: ~s Titulo: ~s  Genero: ~s ~%" (getArtista file)(getTitulo file)(getGenero file))))


(defun buscar-genero(genero) ; Busca por genero en la tabla hash e imprime resultados
	(maphash #'(lambda (key value)(printGenero value genero)) *ht*))

(defun printGenero (file genero)
	(if (search genero (getGenero file)) 
	(format t "Genero: ~s Autor: ~s  Titulo: ~s ~%" (getGenero file)(getArtista file)(getTitulo file))))
