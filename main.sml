use "creador.sml";
use "analizador.sml";

(* Objetivo: Verificar si libros.csv existe y crearlo con encabezado si no existe.
Entrada: Ninguna.
Salida: Asegura que el archivo exista.
*)
fun comprobarArchivo() = let
    val archivo = TextIO.openIn "libros.csv"
    val _ = TextIO.closeIn archivo
in
    ()
end
    (*Si el archivo no existe, se crea el archivo y escribe el ancabezado en la linea inicial*)
    handle _ => let   
        val archivo = TextIO.openOut "libros.csv"
        val _ = TextIO.output(archivo, "codigo, autor, genero, fecha_publicacio, copias_disponibles\n")
        val _ = TextIO.closeOut archivo
    in
        print "\nEl archivo libros.csv fue creado con exito\n"
    end;

(* Objetivo: Mostrar y controlar el menu principal del sistema.
Entrada: Una opción ingresada por el usuario.
Salida: Ejecuta la opción seleccionada.
*)
fun menuPrincipal() = let
    val _ = print "\n==========================================\n"
    val _ = print "      SISTEMA DE GESTION BIBLIOTECARIA\n"
    val _ = print "=============================================\n"
    val _ = print "1. Creador\n"
    val _ = print "2. Analizador\n"
    val _ = print "3. Salir\n"
    val _ = print "----------------------------------------------\n"
    val _ = print "Selecciones una opcion: "

    val opcion = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
in
    case opcion of
        "1" => (menuCreador(); menuPrincipal())
        |"2" => (analizarArchivo(); menuPrincipal())
        | "3" => print "\nPrograma finalizado"
        | _ => (print "\nOpcion invalida\n"; menuPrincipal())
end;

(* Objetivo: Iniciar el sistema bibliotecario.
Entrada: Ninguna.
Salida: Verifica el archivo y muestra el menú principal.
*)
fun main() = let
    val _ = comprobarArchivo()
in
    menuPrincipal()
end;
main()