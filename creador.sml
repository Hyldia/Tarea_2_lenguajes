(*
Permite:
1. Agregar libros al archivo CSV
2. Limpiar el catalogo
*)


(* Objetivo: Eliminar el salto de linea final de un texto.
Entrada: Un texto de tipo string.
Salida: El texto sin el salto de linea final.
*)
fun limpiar texto = let
    val longitud = String.size texto
in
    if longitud > 0 andalso (*Valida que el texto no este vacio y qeu ultimo caracter sea un salto de linea. -andalso es para qe compruben mabas condiciones-*)
        String.sub(texto, longitud - 1) = #"\n"
    then
        String.substring(texto, 0, longitud - 1)
    else
        texto
end;

(* Objetivo: Verificar si todos los caracteres de un texto son digitos.
Entrada: Un texto de tipo string.
Salida: true si es numerico; false en caso contrario.
*)
fun esNumerotexto texto =
    List.all Char.isDigit (String.explode texto);

(* Objetivo: Leer el codigo de un libro.
Entrada: El codigo escrito por el usuario.
Salida: El codigo como string.
*)
fun leerCodigo() = let
    val _ = print "\n------------------------------------\n"
    val _ = print "Codigo del libro: "
    val entrada = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
in
    if String.size entrada > 0
    then   
        entrada
    else
        (print "\nError: El codigo del libro no puede estar vacio\n"; leerCodigo())
end;

(* Objetivo: Leer el autor de un libro.
Entrada: El autor escrito por el usuario.
Salida: El autor como string.
*)
fun leerAutor() = let
    val _ = print "\n------------------------------------\n"
    val _ = print "Autor: "
    val entrada = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
in
    if String.size entrada > 0
    then   
        entrada
    else
        (print "\nError: El autor del libro no puede estar vacio\n"; leerAutor())
end;

(* Objetivo: Leer el genero de un libro.
Entrada: El genero escrito por el usuario.
Salida: El genero como string.
*)
fun leerGenero() = let
    val _ = print "\n------------------------------------\n"
    val _ = print "Genero: "
    val entrada = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
in
    if String.size entrada > 0
    then   
        entrada
    else
        (print "\nError: El genero del libro no puede estar vacio\n"; leerGenero())
end;

(* Objetivo: Leer la fecha de publicación de un libro.
Entrada: Una fecha escrita por el usuario.
Salida: La fecha como string.
*)
fun leerFecha() = let
    val _ = print "\n------------------------------------\n"
    val _ = print "Fecha de publicacion (AAAA-MM-DD): "
    val entrada = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
in
    if String.size entrada = 10
        then   
            entrada
        else
            (print "\nError: El formato de la fehca esta invalida debe ser YYYY-MM-DD\n"; leerFecha())
end;

(* Objetivo: Leer la cantidad de copias disponibles.
Entrada: Un numero escrito por el usuario.
Salida: La cantidad de copias como int.
*)
fun leerCopias() = let
    val _ = print "\n------------------------------------\n"
    val _ = print "Copias disponibles: "
    val entrada = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
    val _ = print "\n------------------------------------\n"
in
    case Int.fromString entrada of
                                SOME numero => numero
                                | NONE =>(print "\nError: Formato incorecto, debe ingresar un numero\n";leerCopias())
end;

(* Objetivo: Leer todos los datos de un libro.
Entrada: Codigo, autor, genero, fecha y copias ingresados por el usuario.
Salida: Una tupla con los datos del libro.
*)
fun leerDatosLibro() = let
    val codigo = leerCodigo()
    val autor = leerAutor()
    val genero = leerGenero()
    val fecha = leerFecha()
    val copias = leerCopias()
in
    (codigo, autor, genero, fecha, copias)
end;

(* Objetivo: Agregar un libro al archivo libros.csv.
Entrada: Codigo, autor, genero, fecha y cantidad de copias.
Salida: Guarda el libro en el archivo.
*)
fun agregarLibro(codigo, autor, genero, fecha, copias) = let
    val lineaArchivo = codigo ^ "," ^ autor ^ "," ^ genero ^ "," ^ fecha  ^ "," ^ Int.toString copias ^ "\n" (*almacenar la linea con los datos del libro y el salto de linea*)
    val archivo = TextIO.openAppend "libros.csv"
    val _ = TextIO.output(archivo, lineaArchivo)
    val _ =TextIO.closeOut archivo
    val _ = (print "\n------------------------------------\n";
            print "El libro se agrego  de forma exitosa\n";
            print "------------------------------------\n")
in
    ()
end;

(* Objetivo: Eliminar los libros del catalogo y conservar el encabezado.
Entrada: Ninguna.
Salida: Actualiza el archivo libros.csv.
*)
fun limpiarCatalogo() = let
    val archivo = TextIO.openOut "libros.csv"
    val _ = TextIO.output(archivo, "codigo,autor,genero,fecha_publicacion,copias_disponibles\n")
    val _ = TextIO.closeOut archivo
in
    (print "\n------------------------------------\n";
    print "Catalogo limpiado correctamente\n";
    print "------------------------------------\n")
end;

(* Objetivo: Mostrar el menú para administrar libros.
Entrada: Una opción ingresada por el usuario.
Salida: Ejecuta la operación seleccionada.
*)
fun menuCreador() = let
    val _ = print "\n==========================================\n"
    val _ = print "      GESTION DE BIBLIOTECARIA CREADOR\n"
    val _ = print "============================================\n"
    val _ = print "1. Agregar libro\n"
    val _ = print "2. Limpiar catalogo\n"
    val _ = print "3. Volver\n"
    val _ = print "------------------------------------\n"
    val _ = print "Seleccione una opcion: "

    val opcion = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
in
    case opcion of
        "1" => (agregarLibro(leerDatosLibro()); menuCreador())
        | "2" =>(limpiarCatalogo(); menuCreador())
        | "3" => print "\nVolviendo al menu principal...\n"
        | _ => (print "\nOpcion invalida\n"; menuCreador())
end;
