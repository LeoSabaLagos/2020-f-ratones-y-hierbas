module Lib where
import Text.Show.Functions

laVerdad = True

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Punto 1

{-
De los ratones nos interesa modelar su nombre, su edad (en años), su peso, y las enfermedades que posee.

Por ejemplo:
    Cerebro es un ratón con 9 años, 0.2 kg de peso y tiene brucelosis, sarampión y tuberculosis.
    Bicenterrata es un ratón con 256 años, 0.2kg de peso, y completamente sano.
    Huesudo es un ratón de 4 años con 10kg de peso, y alta obesidad y sinusitis.

Modelar a los ratones mencionados.
-}

type Enfermedad = String

data Raton = UnRaton {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: [Enfermedad]
} deriving(Show,Eq)

cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis","sarampion","tuberculosis"]

bicenterrata = UnRaton "Bicenterrata" 256 0.2 []

huesudo = UnRaton "Huesudo" 4 10.0 ["alta obesidad","sinusitis"] 
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Punto 2

{-
Existen distintos tipos de hierbas que afectan (modifican) de diferentes maneras al ratón. 

Definir dichas hierbas:
    hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
    Por ejemplo, si a cerebro le doy hierbaBuena, se transforma en un ratón de 3 años.
    
    hierbaVerde, elimina las enfermedades que terminen de cierta forma.
    Por ejemplo, si a cerebro le doy la hierbaVerde del tipo “sis”, queda sólo con sarampión.
    
    alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
    Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. 
    
    hierbaZort, hace que el ratón se transforme en Pinky, 
    perdiendo todas sus enfermedades y quedando con 0 años de edad.
    
    hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) 
    y elimina todas las enfermedades con menos de 10 letras. 
-}

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena unRaton = unRaton{edad = sqrt (edad unRaton)}

hierbaVerde :: String -> Hierba 
hierbaVerde cadena unRaton  = unRaton{ enfermedades = reducirEnfermedades cadena (enfermedades unRaton) }            

reducirEnfermedades :: String -> [Enfermedad]  -> [Enfermedad]
--reducirEnfermedades cadena listaEnfermedades  = filter (terminaEn cadena) listaEnfermedades
reducirEnfermedades cadena = filter (terminaEn cadena)

terminaEn :: String -> Enfermedad -> Bool
terminaEn cadena enfermedad = cadena /= (drop (cuantosDescartar enfermedad) enfermedad)
--terminaEn cadena enfermedad = (/=cadena).(drop ((length enfermedad)-3)) enfermedad
cuantosDescartar enfermedad =  length enfermedad - 3

alcachofa :: Hierba
alcachofa unRaton 
    | (peso unRaton) > 2 = unRaton{peso = reducirPeso unRaton 0.1}
    | otherwise = unRaton{peso = reducirPeso unRaton 0.05}

reducirPeso :: Raton -> Float ->  Float
--reducirPeso unRaton porcentaje  =  (peso unRaton) - (peso unRaton) * porcentaje
reducirPeso unRaton =  ((peso unRaton)-).(*peso unRaton)

hierbaZort :: Hierba
hierbaZort unRaton = unRaton{edad = 0, enfermedades=[]}

hierbaDelDiablo :: Hierba
hierbaDelDiablo unRaton = unRaton{peso = max (peso unRaton - 0.1) 0, enfermedades = filter (not.(<10).length) (enfermedades unRaton)}
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Punto 3
{-
Medicamentos: Los medicamentos son la administración sucesiva de un conjunto de hierbas. 

Se pide crear los siguientes medicamentos para luego poder administrarlos en un ratón: 

    Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa.
    Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg 

    Hacer el reduceFatFast, (que viene en distintas potencias)
    y es un medicamento compuesto por una hierbaVerde de “obesidad” y tantas alcachofas como indique su potencia.
    Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo quede con sinusitis. 
    Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y queda también solo con sinusitis.
    
    Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas.
    Las enfermedades infecciosas son aquellas cuyo nombre termina de alguna de estas formas (utilizar esta constante):
    sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

-}

aplicarHierba :: Hierba -> Raton -> Raton 
aplicarHierba unaHierba unRaton 

type Medicamento = [Hierba]

pondsAntiAge :: Medicamento 
pondsAntiAge = [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

------------------------------------------------------------------------------------------------------------------------------------------------------------------------