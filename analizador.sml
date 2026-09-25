(*
Permite:
1. Mostrar los libros más populares dentro de un rango de copias disponibles
2. Identificar autores con más de 5 libros publicados
3. Buscar libros por código o autor
4. Cantidad de libros por género
5. Resumen general de la biblioteca
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

(* Objetivo: Convertir un texto a minusculas.
Entrada: Un texto de tipo string.
Salida: El texto convertido a minusculas.
*)
fun pasarMinusculas texto =
    String.map Char.toLower texto;

(* Objetivo: Leer una entrada valida.
Entrada: El mensaje que se mostrara al usuario.
Salida: El texto ingresado por el usuario.
*)
fun leerEntrada texto = let
    val _ = print texto
    val entrada = limpiar(valOf(TextIO.inputLine TextIO.stdIn))
in
    (* La entrada se repite hasta que el usuario escriba un valor no vacio. *)
    if String.size entrada > 0
        then 
            entrada
        else
            (print "\nError: El valor no puede quedar vacio\n"; leerEntrada texto)
end;

(* Objetivo: Solicitar una ruta valida.
Entrada: Ninguna.
Salida: La ruta de un archivo existente.
*)
fun rutaValida() = let
    val ruta = leerEntrada "\nIngrese la ruta del archivo: "
in
    (TextIO.closeIn(TextIO.openIn ruta); ruta)
    (*Si el archivo no existe, se crea el archivo y escribe el ancabezado en la linea inicial*)
    handle _ => (print "\nError: no se pudo abrir el archivo\n"; rutaValida())
end;

(* Objetivo: Eliminar saltos de linea de un campo.
Entrada: Un texto de tipo string.
Salida: El texto sin saltos de linea.
*)
fun limpiarCampo texto =
    String.implode(List.filter (fn c => c <> #"\n" andalso c <> #"\r") (String.explode texto));


(* Objetivo: Separar una linea del archivo CSV.
Entrada: Una linea de texto.
Salida: Una lista con los campos separados por comas.
*)
fun dividirLinea linea =
    String.tokens(fn c => c = #",") linea;

(* Objetivo: Convertir una linea del archivo CSV en una tupla.
Entrada: Una linea del archivo.
Salida: Una tupla con los datos del libro o NONE si el formato es invalido.
*)
fun pasarLinea linea = 
    case dividirLinea linea of
        [codigo,autor,genero,fecha,copias] =>
            SOME(
                limpiarCampo codigo,
                limpiarCampo autor,
                limpiarCampo genero,
                limpiarCampo fecha,
                (* Convierte la cantidad de copias de string a int. *)
                valOf(Int.fromString(limpiarCampo copias)))
            (* Si las lineas que no tienen exactamente cinco campos se ignoran. *)
        | _ => NONE;

(* Objetivo: Leer todas las lineas de un archivo.
Entrada: Un archivo abierto para lectura.
Salida: Una lista con todas las lineas del archivo.
*)
fun leerLineas archivo =
    case TextIO.inputLine archivo of
        (* NONE indica que se llego al final del archivo. *)
        NONE => []
    (* Se guarda la linea actual y se continua leyendo recursivamente. *)
    | SOME linea => linea::leerLineas archivo;

(* Objetivo: Cargar el catalogo completo.
Entrada: La ruta del archivo CSV.
Salida: Una lista con los libros del catalogo.
*)
fun leerArchivo ruta =
    let
        val archivo = TextIO.openIn ruta
        val lineas = leerLineas archivo
        val _ = TextIO.closeIn archivo
    in
        (* List.tl elimina el encabezado; mapPartial descarta lineas invalidas. *)
        List.mapPartial pasarLinea(List.tl lineas)
    end;

(* Objetivo: Buscar libros por codigo o autor sin distinguir mayusculas.
Entrada: Una lista de libros y el texto de busqueda.
Salida: La lista de libros relacionados con la busqueda.
*)
fun buscarLibro lista texto =
    let
        (* Se normaliza una sola vez el texto ingresado para comparar sin mayusculas. *)
        val textoNormalizado = pasarMinusculas texto
    in
        List.filter(fn(codigo,autor,_,_,_) =>
            pasarMinusculas codigo = textoNormalizado orelse
            (* isSubstring para buscar el texto aunque sea parte del nombre del autor. *)
            String.isSubstring textoNormalizado (pasarMinusculas autor))
            lista
    end;

(* Objetivo: Imprimir los datos de los libros encontrados.
Entrada: Una lista de libros.
Salida: Los datos de cada libro impresos en pantalla.
*)
fun imprmirLibroBuscado [] = ()
    | imprmirLibroBuscado((codigo,autor,genero,fecha,copias)::listaRestante) =
        (print("Codigo: " ^ codigo ^ " | Autor: " ^ autor ^ " | Genero: " ^ genero ^ " | Fecha: " ^ fecha ^ " | Copias: " ^ Int.toString copias ^ "\n"); imprmirLibroBuscado listaRestante);

(* Objetivo: Buscar libros por codigo o autor desde un archivo.
Entrada: La ruta del archivo CSV.
Salida: Muestra en pantalla los libros encontrados.
*)
fun buscarPorCodigoAutor ruta = 
    let
        val catalogo = leerArchivo ruta
        val texto = leerEntrada "\nIngrese el codigo o autor del libro: "
        val resultados = buscarLibro catalogo texto
    in
        if null resultados
            then
                print("\nNo se encontraron libros relacionados con la busqueda\n")
            else
                imprmirLibroBuscado resultados
    end;

(* Objetivo: Contar los libros de un genero especifico.
Entrada: La ruta del archivo CSV.
Salida: La cantidad de libros del genero indicado.
*)
fun cantidadPorGenero ruta =
    let
        val catalogo = leerArchivo ruta
        val generoBuscado  = leerEntrada "\nIngrese el genero: "
        val cantidad = length(List.filter(fn(_,_,genero,_,_) => pasarMinusculas genero = pasarMinusculas generoBuscado)catalogo)
    in
        print("\nCantidad de libros: " ^ Int.toString cantidad ^ "\n")
    end;

(* Objetivo: Ordenar libros de mayor a menor cantidad de copias.
Entrada: Una lista de libros.
Salida: La lista ordenada por cantidad de copias.
*)
fun ordenarPorCopias lista =
    case lista of
        [] => []
    | libroBase :: listaRestante =>
        let
            val (_,_,_,_,copiasBase) = libroBase
            (* Se separan los libros con mas y con menos copias que el libro base. *)
            val mayores = List.filter(fn (_,_,_,_,copias) => copias > copiasBase) listaRestante
            val menores = List.filter(fn (_,_,_,_,copias) => copias <= copiasBase) listaRestante
        in
            (* Se ordenan ambas partes y se unen al del libro base. *)
            ordenarPorCopias mayores@[libroBase]@ordenarPorCopias menores
        end;

(* Objetivo: Leer un numero entero valido.
Entrada: El mensaje que se mostrara al usuario.
Salida: Un numero entero ingresado por el usuario.
*)
fun leerEntero mensaje =
    let
        val entrada = leerEntrada mensaje
    in
        (* Int.fromString devuelve NONE si la entrada no representa un entero. *)
        case Int.fromString entrada of
            SOME numero => numero
        | NONE =>(print "\nError: Debe ingresar un numero valido\n"; leerEntero mensaje)
    end;

(* Objetivo: Mostrar los libros mas populares dentro de un rango de copias.
Entrada: La ruta del archivo CSV.
Salida: Los libros del rango indicado ordenados por copias.
*)
fun librosMasPopulares ruta =
    let
        val catalogo = leerArchivo ruta
        val minimo = leerEntero "\nIngresar cantidad de copias minimas: "
        val maximo = leerEntero "\nIngresar cantidad de copias maximas: "
        (* andalso exige que cada libro cumpla ambos limites del rango. *)
        val filtrados = List.filter(fn (_,_,_,_,copias) => copias >= minimo andalso copias <= maximo) catalogo
        val ordenados = ordenarPorCopias filtrados
    in
        (
            print "\n------------------------------------------\n";
            print "\n===== LIBROS MAS POPULARES =====\n\n";
            if null ordenados
                then print "\nNo hay libros para ese rango\n"
                else imprmirLibroBuscado ordenados
        )
    end;

(* Objetivo: Agregar un autor a la lista de conteo o aumentar su cantidad.
Entrada: Un autor y una lista de autores contados.
Salida: La lista actualizada de autores y cantidades.
*)
fun acumularAutor (autorNuevo, lista) =
    case lista of
        [] => [(autorNuevo,1)]
    | (autor,cantidad)::listaRestante =>
        if pasarMinusculas autorNuevo = pasarMinusculas autor
            then
                (autor,cantidad + 1)::listaRestante
        else
            (autor,cantidad)::
            acumularAutor(autorNuevo,listaRestante);

(* Objetivo: Contar la cantidad de libros publicados por cada autor.
Entrada: El catalogo de libros.
Salida: Una lista con cada autor y su cantidad de libros.
*)
fun contarAutores catalogo =
    (* foldl recorre el catalogo y conserva el conteo acumulado. *)
    List.foldl(fn((_,autor,_,_,_),acumulado) => acumularAutor(autor,acumulado)) [] catalogo;

(* Objetivo: Imprimir los autores y sus cantidades de libros.
Entrada: Una lista de autores con sus cantidades.
Salida: Los autores y cantidades impresos en pantalla.
*)
fun imprimirAutores [] = ()
    | imprimirAutores ((autor,cantidad)::listaRestante) = (print(autor ^" -> " ^ Int.toString cantidad ^ " libros\n"); imprimirAutores listaRestante);

(* Objetivo: Mostrar los autores que tienen al menos cinco libros.
Entrada: La ruta del archivo CSV.
Salida: Los autores que cumplen la cantidad indicada.
*)
fun autoresMasDe5Libros ruta =
    let
        val catalogo = leerArchivo ruta
        val conteo = contarAutores catalogo
        val filtrados = List.filter(fn(_,cantidad) => cantidad >= 5) conteo
    in
        (
            print "\n------------------------------------------\n";
            print "\n===== AUTORES CON MAS DE 5 LIBROS =====\n\n";
            if null filtrados
                then print "\nNo hay autores con 5 o mas libros\n"
                else imprimirAutores filtrados
        )
    end;

(* Objetivo: Agregar un genero a la lista de conteo o aumentar su cantidad.
Entrada: Un genero y una lista de generos contados.
Salida: La lista actualizada de generos y cantidades.
*)
fun acumularGenero (generoNuevo, lista) =
    case lista of
        [] => [(generoNuevo,1)]
    | (genero,cantidad)::listaRestante =>
        if pasarMinusculas generoNuevo = pasarMinusculas genero
            then 
                (genero,cantidad + 1)::listaRestante
        else
            (genero,cantidad)::acumularGenero(generoNuevo,listaRestante);

(* Objetivo: Contar la cantidad de libros de cada genero.
Entrada: El catalogo de libros.
Salida: Una lista con cada genero y su cantidad de libros.
*)
fun contarGeneros catalogo =
    (* Cada genero se agrupa para obtener su cantidad total. *)
    List.foldl(fn((_,_,genero,_,_),acumulado) => acumularGenero(genero,acumulado)) [] catalogo;

(* Objetivo: Obtener el genero con mayor cantidad de libros.
Entrada: Una lista de generos con sus cantidades.
Salida: El genero con mayor cantidad y su cantidad de libros.
*)
fun generoConMasLibros [] = ("", 0)
    | generoConMasLibros ((genero,cantidad)::listaRestante) =
        (* El primer genero se usa como mejor resultado inicial. *)
        List.foldl(fn((generoActual,cantidadActual),mejor) =>
                    let
                        val (_,cantidadMayor) = mejor
                    in
                        if cantidadActual > cantidadMayor
                            then (generoActual,cantidadActual)
                            else mejor
                    end)
                    (genero,cantidad)
                    listaRestante;

(* Objetivo: Extraer el mes y el anio de una fecha.
Entrada: Una fecha con formato AAAA-MM-DD.
Salida: El anio y mes con formato AAAA-MM.
*)
fun extraerMesAnio fecha =
    (* Los primeros siete caracteres corresponden a AAAA-MM. *)
    String.substring(fecha,0,7);

(* Objetivo: Agregar un mes a la lista de conteo o aumentar su cantidad.
Entrada: Un mes y una lista de meses contados.
Salida: La lista actualizada de meses y cantidades.
*)
fun acumularMes (mesNuevo, lista) =
    case lista of
        [] => [(mesNuevo,1)]
    | (mes,cantidad)::resto =>
        if mesNuevo = mes
            then
                (mes,cantidad + 1)::resto
        else
            (mes,cantidad)::acumularMes(mesNuevo,resto);

(* Objetivo: Contar las publicaciones realizadas en cada mes.
Entrada: El catalogo de libros.
Salida: Una lista con cada mes y su cantidad de publicaciones.
*)
fun contarMeses catalogo =
    (* Se agrupan las fechas por mes para contar publicaciones mensuales. *)
    List.foldl(fn((_,_,_,fecha,_),acumulado) =>
                acumularMes(extraerMesAnio fecha, acumulado)) [] catalogo;
        
(* Objetivo: Obtener el mes con mayor cantidad de publicaciones.
Entrada: Una lista de meses con sus cantidades.
Salida: El mes con mayor cantidad y su cantidad de publicaciones.
*)
fun mesMasPublicaciones [] = ("", 0)
    | mesMasPublicaciones ((mes,cantidad)::listaRestante) =
        (* El primer mes se usa como mejor resultado inicial. *)
        List.foldl(fn((mesActual,cantidadActual),mejor) =>
                    let
                        val (_,cantidadMayor) = mejor
                    in
                        if cantidadActual > cantidadMayor
                            then (mesActual,cantidadActual)
                            else mejor
                    end)
                    (mes,cantidad)
                    listaRestante;

(* Objetivo: Obtener el libro con mayor cantidad de copias.
Entrada: El catalogo de libros no vacio.
Salida: El libro con mayor cantidad de copias.
*)
fun libroMasCopias catalogo =
    (* hd necesita que el catalogo tenga al menos un libro. *)
    List.foldl(fn(libro,mejor) =>
                let
                    val (_,_,_,_,copiasLibro) = libro
                    val (_,_,_,_,copiasMejor) = mejor
                in
                    if copiasLibro > copiasMejor
                        then
                            libro
                    else
                        mejor
                end
            )
            (hd catalogo)
            catalogo;

(* Objetivo: Obtener el autor con mayor cantidad de libros.
Entrada: Una lista de autores con sus cantidades.
Salida: El autor con mayor cantidad y su cantidad de libros.
*)
fun mayorAutor lista =
    (* hd necesita que exista al menos un autor contado. *)
    List.foldl(fn((autor,cantidad),mejor) =>
            let
                val (_,cantidadMayor) = mejor
            in
                if cantidad > cantidadMayor
                    then
                        (autor,cantidad)
                else
                    mejor
            end
        )
        (hd lista)
        lista;

(* Objetivo: Imprimir la cantidad de libros de cada genero.
Entrada: Una lista de generos con sus cantidades.
Salida: Los generos y cantidades impresos en pantalla.
*)
fun imprimirGeneros [] = ()
    | imprimirGeneros ((genero,cantidad)::resto) = (print(genero ^ ": " ^ Int.toString cantidad ^ " libros\n"); imprimirGeneros resto);

(* Objetivo: Mostrar un resumen general del catalogo.
Entrada: La ruta del archivo CSV.
Salida: Las estadisticas principales de la biblioteca.
*)
fun resumenGeneral ruta =
    let
        val catalogo = leerArchivo ruta
        val generosContados = contarGeneros catalogo
        val (codigo, autor, genero, fecha, copias) = libroMasCopias catalogo
        val (autorMayor, cantidadAutor) = mayorAutor(contarAutores catalogo)
        val (generoMayor, cantidadGenero) = generoConMasLibros(contarGeneros catalogo)
        val (mesMayor, cantidadMes) = mesMasPublicaciones(contarMeses catalogo)
    in
        (
            print "\n====================================\n";
            print " RESUMEN GENERAL\n";
            print "====================================\n";
            print "\nCantidad de libros por genero:\n";
            imprimirGeneros generosContados;
            print("-----------------------------------------\n");
            print("Libro con mas copias: " ^ codigo ^ "\n");
            print("Autor del libro: " ^ autor ^ "\n");
            print("Copias disponibles: " ^ Int.toString copias ^ "\n");
            print("-----------------------------------------\n");
            print("Autor con mas libros: " ^ autorMayor ^ "\n");
            print("Cantidad de libros: " ^ Int.toString cantidadAutor ^ "\n");
            print("-----------------------------------------\n");
            print("Genero con mas libros: " ^ generoMayor ^ "\n");
            print("Cantidad de libros: " ^ Int.toString cantidadGenero ^ "\n");
            print("-----------------------------------------\n");
            print("Mes con mas publicaciones: " ^ mesMayor ^ "\n");
            print("Cantidad de publicaciones: " ^ Int.toString cantidadMes ^ "\n")
        )
    end;

(* Objetivo: Solicitar la ruta y abrir el menu del analizador.
Entrada: Ninguna.
Salida: Inicia el menu de opciones del analizador.
*)
fun analizarArchivo () =
    let
        val ruta = rutaValida()
    in
        menuAnalizador ruta
    end

(* Objetivo: Mostrar y ejecutar las opciones del analizador.
Entrada: La ruta del archivo CSV.
Salida: Ejecuta la opcion seleccionada por el usuario.
*)
and menuAnalizador ruta =
    let
        val _ = print "\n====================================\n"
        val _ = print " ANALIZADOR\n"
        val _ = print "====================================\n"
        val _ = print "1. Libros mas populares\n"
        val _ = print "2. Autores con mas de 5 libros\n"
        val _ = print "3. Buscar por codigo o autor\n"
        val _ = print "4. Cantidad de libros por genero\n"
        val _ = print "5. Resumen general\n"
        val _ = print "6. Volver\n"
        val _ = print "====================================\n"
        val opcion = leerEntrada "\nSeleccione una opcion: "
    in
        case opcion of 
            "1" => (librosMasPopulares ruta; menuAnalizador ruta)
            | "2" => (autoresMasDe5Libros ruta; menuAnalizador ruta)
            | "3" => (buscarPorCodigoAutor ruta; menuAnalizador ruta)
            | "4" => (cantidadPorGenero ruta; menuAnalizador ruta)
            | "5" => (resumenGeneral ruta; menuAnalizador ruta)
            | "6" => (print "\nVolviendo al menu principal...\n")
            | _ => (print "\nOpcion invalida\n"; menuAnalizador ruta)
    end;
