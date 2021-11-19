module Library where
import PdePreludat


data Pais = Unpais {
    nombre ::  String,
    habitantes :: Number,
    indicadores :: [Indicador]
}deriving (Show, Eq)

type Indicador = (String, Number)

setear :: Number -> Indicador -> Indicador
setear cuanto (descripcion, medida) = (descripcion, cuanto)

reducir :: Number -> Indicador -> Indicador
reducir cuanto (descripcion, medida) = (descripcion, medida - cuanto)

aumentar :: Number -> Indicador -> Indicador
aumentar incremento (descripcion, medida) = (descripcion, medida + incremento)

multiplicarindicepor :: Number -> Indicador -> Indicador
multiplicarindicepor numero (descripcion, medida) = (descripcion, medida * numero)

crecerpoblacionunperiodo :: Pais -> Pais
crecerpoblacionunperiodo pais = pais { habitantes = habitantes pais * 1.05 }


ubicar :: String -> Pais -> Indicador
ubicar nombre pais = head (filter (\(x,_) -> x == nombre) (indicadores pais))


estabien :: Pais ->Number -> Bool
estabien pais numero = snd (ubicar "deuda externa" pais) > snd (ubicar "iva vigente" pais)

tienefuturo :: Pais -> Number -> Bool
tienefuturo pais valor = snd(ubicar "indice educativo" pais) > valor

estacondenado :: Pais -> Number -> Bool
estacondenado pais numero = (foldl1 () snd (todosexcepto "deuda externa" pais )) numero > snd (ubicar "deuda externa" pais)


todosexcepto :: String -> [Indicador] -> [Indicador]
todosexcepto nombre indicadores = filter (\(x,_) -> x /= nombre) indicadores

type Paises = [Pais]

cumbreonu :: Paises ->( Pais -> Number -> Bool ) -> Number -> [String]
cumbreonu paises f numero = nombrepaises ( filter (\x -> f x numero ) paises )

nombrepaises :: Paises -> [String]
nombrepaises paises = map nombre paises

---------------------------------------------------------------------
data Fuerzapolitica = Unafuerzapolitica {
    transformacion :: (Pais -> Pais),
    condicion :: ( Indicador -> Bool )
}deriving (Show, Eq)

transformarhormiga ::  Pais -> Pais
transformarhormiga pais 
    |condicion hormigaignorante (ubicar "desocupacion" pais) = pais { indicadores = multiplicarindicepor 0.9 (ubicar "educacion" pais) : (todosexcepto "educacion" indicadores pais) }
    |otherwise = pais { indicadores = reducir 1 (ubicar "educacion" pais) : (todosexcepto "educacion" indicadores pais) }

desocupacionalta :: Indicador -> Bool
desocupacionalta indicador = snd indicador > 10
        
transformareduqueitor :: Pais -> Pais
transformareduqueitor pais = pais {indicadores = multiplicarindicepor 1.4 (ubicar "educacion" pais) :setear 24 (ubicar "iva" pais) :  (todosexcepto "iva" (todosexcepto "educacion" indicadores pais))  }

filtrarpor :: [indicador] -> ( Indicador -> Bool ) -> [Indicador]
filtrarpor lista f 
    | f == nada = lista
    |otherwise = filter(\x -> f x) lista

transformarduplicador :: Pais -> ( Indicador -> Bool )->Pais 
transformarduplicador pais f = map ( multiplicarindicepor 2) (filtrarpor (indicadores pais) (condicion duplicador) )

empiezacond :: Indicador -> Bool
empiezacond indicador = head fst indicador == 'd'

transformarCazabuitre :: Pais -> Pais 
transformarCazabuitre pais = pais { indicadores = setear 0 (ubicar "deuda externa" pais) :  ( todosexcepto "deuda externa" indicadores pais ) } 

transformarpepe :: Pais -> Pais
transformarpepe pais = pais{indicadores = setear 99 (ubicar "desempleo" pais) : ( todosexcepto "desempleo" indicadores pais ) }

hormigaignorante :: Fuerzapolitica
hormigaignorante = Unafuerzapolitica transformarhormiga desocupacionalta

eduqueitor :: Fuerzapolitica
eduqueitor = Unafuerzapolitica transformareduqueitor desocupacionalta

duplicador :: Fuerzapolitica
duplicador = Unafuerzapolitica transformarduplicador empiezacond

cazabuitre :: Fuerzapolitica
cazabuitre = Unafuerzapolitica transformarCazabuitre empiezacond

pepe :: Fuerzapolitica 
pepe = Unafuerzapolitica transformarpepe empiezacond

# Como hay fuerzas politicas que no tienen condicion, se rellena el campó con una aleatoria, la cual no tendra efecto a la hora de gobernar.
---------------------------------------------
type Fuerzas = [Fuerzapolitica]


gobernar :: Pais -> Fuerzapolitica -> Pais
gobernar pais fuerza = crecerpoblacionunperiodo (transformacion fuerza (pais))

multipartidismo :: Pais -> Fuerzas -> [Pais]
multipartidismo pais fuerzas  = map (gobernar pais) fuerzas

# Dada una lista de fuerzas, las cuales reciben un pais y devuelven un pais modificado, a la hora de aplicarles a todas un pais usando el map, se genera una lista de los paises modificados por cada fuerza luego de un periodo en el poder.

ejercer :: Pais -> Fuerza -> Int -> Pais
ejercer pais fuerza periodos = gobernar pais fuerza && periodos (-1) && reelegir pais fuerza periodos

reelegir :: Pais -> Fuerza -> Int -> Pais   
reelegir fuerza pais numerodeperiodos   
    |estabien (1) pais && periodos /= 0 = gobernar pais fuerza && periodos (-1) && reelegir pais fuerza periodos
    |otherwise = pais






