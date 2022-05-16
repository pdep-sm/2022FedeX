module Library where
import PdePreludat

{-Los envíos tienen distintas características como 
información básica, éstas son: 
lugar de origen (ciudad y país), 
lugar de destino (idem), 
peso en kilogramos, 
precio base, 
categorías, 
impuestos asociados
-}

{- Punto 1
Crear el modelo necesario que mejor se adapte para la solución.
Además:
Indique el tipo de un cargo.
Indique el tipo de un impuesto.
-}

data Envio = 
    Envio {
        origen  :: Lugar,
        destino :: Lugar,
        peso    :: Number,
        precio  :: Number,
        categorias :: [Categoria],
        impuestos :: [Impuesto]
    }

type Lugar = (String, String)
ciudad = fst
pais = snd

type Categoria = String

type Impuesto = Envio -> Number
type Cargo  = Envio -> Envio 

--Cargo categórico: Si el envío tiene una categoría X,
-- se computa un porcentaje dado del  precio base.
cargoCategorico :: Categoria -> Number -> Cargo
cargoCategorico categoria porcentaje envio  
    | elem categoria $ categorias envio = 
        sumarPrecio (porcentaje * precio envio / 100) envio
    | otherwise = envio
--Cargo por sobrepeso: Si el peso es menor o igual a
-- un peso dado (en Kg.), no se afecta el precio.
-- En el caso de ser mayor se le suma $80 por cada kilo
-- que lo supere.
cargoSobrepeso pesoLimite envio
    | peso envio <= pesoLimite  = envio
    | otherwise                 = envio { precio = precio envio + 80 * (peso envio - pesoLimite)}

cargoSobrepeso' pesoLimite envio = 
    sumarPrecio (max (80 * (peso envio - pesoLimite)) 0) envio

cargoArbitrario = 
    sumarPrecio 50 

sumarPrecio importe envio = 
    envio { precio = precio envio + importe }
--Cargo arbitrario: $50 adicionales. Porque sí.


{- Punto 2
Modelar con funciones constantes:
Un cargo categórico de “tecnología” de 18%.
Envío con origen en Buenos Aires, Argentina y con destino Utrecht, Países Bajos, de 2 kg. de peso, precio base de $220, con las categorías de música y tecnología, sin impuestos.
Envío con origen California, Estados Unidos y con destino Miami, Estado Unidos, de 5 kg. de peso, precio base $1500, con categoría de libros, y con IVA e impuesto extraño.
-}
--2.a
cargoTecnologico = cargoCategorico "Tecno" 18
--2.b
envioLocal = Envio {
    origen = ("California", "USA"),
    destino = ("Miami", "USA"),
    peso = 5,
    precio = 1500,
    categorias = ["Libro"],
    impuestos = []
}
--2.c
envioInternacional = Envio {
    origen = ("BsAS", "Argentina"),
    destino = ("Utrecht", "Países Bajos"),
    peso = 2,
    precio = 220,
    categorias = ["Música", "Tecno"],
    impuestos = []
}

{-
Sobre el precio...
Saber si el precio base de un envío cuesta más que un valor determinado N.
Conocer si un envío es barato. Decimos que es barato si vale $1300 o menos (precio base).
-}
--3.a
cuestaMas valor envio = valor < precio envio
--3.b
esBarato = not . cuestaMas 1300

{-
Sobre los lugares...
Saber si un envío se dirige a un país determinado.
Dado un envío, determinar si es local o es internacional. Es local cuando los países de origen y de destino son iguales.
-}
--4.a
seDirige paisDestino envio = paisDestino == (pais . destino) envio
seDirige' paisDestino = (paisDestino==) . pais . destino

--4.b
esLocal envio =  seDirige (pais . origen $ envio) envio
esInternacional = not . esLocal

{- Punto 5
A partir de un conjunto de envíos, obtener aquellos que tienen ciertas categorías.
-}
enviosDeCategorias envios listaDeCategorias = 
    filter (flip all listaDeCategorias . flip elem . categorias) envios  

{- Punto 6
Obtener el precio total de un envío, en base a los impuestos que tiene asignado y a un conjunto de cargos que se aplican en la sucursal de envío.
-}
precioTotal envio cargos = 
    foldl (\nuevaBase impuesto -> nuevaBase + impuesto envioActualizado) base $ impuestos envio
    where
        base = precioBruto envio cargos
        envioActualizado = envio {precio = base}

precioBruto envio cargos =
    precio $ foldr ($) envio cargos