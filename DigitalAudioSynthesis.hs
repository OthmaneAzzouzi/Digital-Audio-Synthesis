
module Main where
import Data.List (transpose)
import Data.Int
import Data.ByteString.Builder
import qualified GHC.IO.Handle
import qualified GHC.IO.Handle.FD
import qualified GHC.IO.Handle.Types
import qualified GHC.IO.IOMode
import System.Environment

------------------------------------------------------------------------
-- Le code suivant construit le fichier .WAV.                         --
-- Le code que vous devez construire est à la suite.                  --
-- Remarquez les constantes                                           --
--     _FREQUENCE_ECHANTILLONAGE                                      --
--     _AMPLITUDE_MAXIMUM                                             --
-- qui vous seront utile.                                             --
------------------------------------------------------------------------

_FREQUENCE_ECHANTILLONAGE :: Int32
_FREQUENCE_ECHANTILLONAGE = 44100

_FREQUENCE_ECHANTILLONAGE_DOUBLE :: Double
_FREQUENCE_ECHANTILLONAGE_DOUBLE = fromIntegral _FREQUENCE_ECHANTILLONAGE

_FREQUENCE_ECHANTILLONAGE_INT :: Int
_FREQUENCE_ECHANTILLONAGE_INT = fromIntegral _FREQUENCE_ECHANTILLONAGE

_AMPLITUDE_MAXIMUM :: Int
_AMPLITUDE_MAXIMUM = ( 2 ^ ( ( ( fromIntegral _OCTETS_PAR_ECHANTILLON ) :: Int ) * 8 - 1) ) - 1

_AMPLITUDE_MAXIMUM_DOUBLE :: Double
_AMPLITUDE_MAXIMUM_DOUBLE = fromIntegral _AMPLITUDE_MAXIMUM

------------------------------------------------------------------------

_POSITION_ARG_NOM_FICHIER_INSTRUMENTS :: Int
_POSITION_ARG_NOM_FICHIER_INSTRUMENTS = 0

_POSITION_ARG_NOM_FICHIER_TRACKS :: Int
_POSITION_ARG_NOM_FICHIER_TRACKS = 1

_POSITION_ARG_NOM_FICHIER_COMPOSITION :: Int
_POSITION_ARG_NOM_FICHIER_COMPOSITION = 2

_POSITION_ARG_NOM_FICHIER_WAVE :: Int
_POSITION_ARG_NOM_FICHIER_WAVE = 3

_MSSG_ERREUR_NOMBRE_ARGUMENT :: String
_MSSG_ERREUR_NOMBRE_ARGUMENT = "Il doit y avoir quatre arguments sur la ligne de commande."


_OCTETS_PAR_ECHANTILLON :: Int16
_OCTETS_PAR_ECHANTILLON = 2


-- -- entete info
-- RIFF
tailleDonnee :: Int -> Int32
tailleDonnee nbEchantillon = 20 + _SUB_1_TAILLE + ( _SUB_2_TAILLE nbEchantillon )

-- WAVE
chunkBuiler :: [ Int ] -> Builder
chunkBuiler echantillons = mconcat [ string8 "RIFF", 
                                     int32LE ( tailleDonnee nbEchantillon ), 
                                     string8 "WAVE", 
                                     _SUB_1_BUILDER, 
                                     sub2Builder nbEchantillon echantillons
                                   ]
    where nbEchantillon = length echantillons

-- -- Sub 1 info
-- fmt
_SUB_1_TAILLE :: Int32
_SUB_1_TAILLE = 16

-- PCM
_FORMAT_AUDIO :: Int16
_FORMAT_AUDIO = 1

-- Mono
_NB_CANALS :: Int16
_NB_CANALS = 1

_FREQUENCE_OCTETS :: Int32
_FREQUENCE_OCTETS = _FREQUENCE_ECHANTILLONAGE * 
                  ( ( fromIntegral _NB_CANALS ) :: Int32 ) * 
                  ( ( fromIntegral _OCTETS_PAR_ECHANTILLON ) :: Int32 )

_OCTETS_PAR_BLOCK :: Int16
_OCTETS_PAR_BLOCK = _NB_CANALS * _OCTETS_PAR_ECHANTILLON

_BITS_PAR_ECHANTILLON :: Int16
_BITS_PAR_ECHANTILLON = _OCTETS_PAR_ECHANTILLON * 8

_SUB_1_BUILDER :: Builder
_SUB_1_BUILDER = mconcat [ string8 "fmt ",
                        int32LE _SUB_1_TAILLE,
                        int16LE _FORMAT_AUDIO,
                        int16LE _NB_CANALS,
                        int32LE _FREQUENCE_ECHANTILLONAGE,
                        int32LE _FREQUENCE_OCTETS,
                        int16LE _OCTETS_PAR_BLOCK,
                        int16LE _BITS_PAR_ECHANTILLON
                      ]

-- -- Sub 2 info
-- data
_SUB_2_TAILLE :: Int -> Int32
_SUB_2_TAILLE nbEchantillon = ( ( fromIntegral nbEchantillon ) :: Int32 ) * 
                           ( ( fromIntegral _OCTETS_PAR_BLOCK ) :: Int32 )

sub2Builder :: Int -> [ Int ] -> Builder
sub2Builder nbEchantillon echantillons = mconcat ( [ string8 "data",
                                                     int32LE ( _SUB_2_TAILLE nbEchantillon )
                                                   ] ++ [ fTraduction x | x <- echantillons ] )
    where fTraduction = if _OCTETS_PAR_ECHANTILLON == 1 
                        then \x -> int8 ( ( fromIntegral x ) :: Int8 ) 
                        else \x -> int16LE ( ( fromIntegral x ) :: Int16 ) 
        
ecrireWave :: FilePath -> [ Int ] -> IO ()
ecrireWave nomFichier echantillons = 
    do fichier <- GHC.IO.Handle.FD.openBinaryFile nomFichier GHC.IO.IOMode.WriteMode
       GHC.IO.Handle.hSetBuffering fichier ( GHC.IO.Handle.Types.BlockBuffering Nothing )
       hPutBuilder fichier ( chunkBuiler echantillons )
       GHC.IO.Handle.hClose fichier

-------------------------------------------------------------------------------

main = 
    do argv <- getArgs
       argc <- return ( length argv )
       nomFichierInsts <- if _POSITION_ARG_NOM_FICHIER_INSTRUMENTS < argc 
                          then return ( argv !! _POSITION_ARG_NOM_FICHIER_INSTRUMENTS )
                          else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       nomFichierTracks <- if _POSITION_ARG_NOM_FICHIER_TRACKS < argc 
                           then return ( argv !! _POSITION_ARG_NOM_FICHIER_TRACKS )
                           else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       nomFichierComposition <- if _POSITION_ARG_NOM_FICHIER_COMPOSITION < argc 
                                then return ( argv !! _POSITION_ARG_NOM_FICHIER_COMPOSITION )
                                else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       nomFichierWave <- if _POSITION_ARG_NOM_FICHIER_WAVE < argc 
                         then return ( argv !! _POSITION_ARG_NOM_FICHIER_WAVE )
                         else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       instruments <- readFile nomFichierInsts
       tracks <- readFile nomFichierTracks
       composition <- readFile nomFichierComposition
       ecrireWave nomFichierWave ( construireWave instruments tracks composition )

  

----
-- Votre code démarre dans la fonction suivante :
      --let patron = map words (splitChar (=='\n') ordresContents );
      --dc = ((patron !! 0) !! 0)
      --    pistes = map (traiterPiste (read dc) instrumentsContents) (splitString (splitChar (=='+') (replace pistesContents "pistes " "+")))

      

construireWave :: String -> String -> String -> [Int]
construireWave instruments tracks composition = resultat
                      where
                          patron = map words (splitChar (=='\n') composition );
                          dc = ((patron !! 0) !! 0)
                          pistes = map (traiterPiste (read dc) ( replace (replace (replace instruments "\r" "\n") "    " " ") "  " " " )) (splitString (splitChar (=='+') (replace (replace tracks "\r" "\n") "piste " "+")))
                          resultat = filtrerSortie (construirePatronFinal (map construirePatron (map (map (isolerElement (concat pistes)) ) (removeFirst patron))))


contaOcs :: [String] -> [(Int, String)]
contaOcs [] = []
contaOcs [x] = [(1,x)]
contaOcs (x1:xs@(x2:_))
     | x1 == x2, ((yi, yv):ys) <- contaOcs xs = (yi+1, yv) : ys
     | otherwise = (1, x1) : contaOcs xs

zipAllWith :: ([a] -> b) -> [[a]] -> [b]
zipAllWith _ []  = []
zipAllWith f xss = map f . transpose $ xss
zipAll = zipAllWith id

filtrerSortie :: [Double] -> [Int]
filtrerSortie liste = map traiter (map ((*) 32767) liste)
      where 
         traiter n
                | truncate n < -32767 = -32767
                | truncate n > 32767 = 32767
                | otherwise = truncate n

isolerElement ::  [[Double]] -> String -> [Double]
isolerElement liste i = (liste !! (read i))  


construirePatron :: [[Double]] -> [Double]
construirePatron pistes = map sum (zipAll pistes)

construirePatronFinal :: [[Double]] -> [Double]
construirePatronFinal listePatrons = concat listePatrons



toDouble :: String -> Double
toDouble chaine = read chaine :: Double


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ replace (drop (length find) s) find repl
        else head s : replace (tail s) find repl

-- splitChar fonction
splitChar :: (Char -> Bool) -> String -> [String]
splitChar p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : splitChar p s''
                          where (w, s'') = break p s'

-- splitString fonction
splitString :: [String] -> [[String]]
splitString = map (splitChar (=='\n'))

-- extraireElement fonction
extraireElement :: Int -> [[String]] -> [[String]]
extraireElement i liste = map words (liste !! i)

-- extraireDonneeNoPorcess fonction
extraireDonneeNoPorcess :: Int -> [[b]] -> [b]
extraireDonneeNoPorcess i liste = map noPorcess (liste !! i)

-- noPorcess fonction
noPorcess :: p -> p
noPorcess elememt = elememt;

-- Fonction pour remplacer les - 
remplacerTirer :: [String] -> [String]
remplacerTirer liste = map (\i -> nmp i (liste !! i)) [0..length liste -1]
                where
                    nmp i x
                      | x == "-" =
                        if(liste !! (i - 1)) == "-"
                            then if liste !! (i - 2) == "-"
                                      then if liste !! (i - 3) == "-"
                                                then if liste !! (i - 4) == "-"
                                                          then if liste !! (i - 5) == "-"
                                                                    then if liste !! (i - 6) == "-"
                                                                              then if liste !! (i - 7) == "-"
                                                                                        then liste !! (i - 8)
                                                                                   else liste !! (i - 7)
                                                                         else liste !! (i - 6)
                                                               else liste !! (i - 5)
                                                     else liste !! (i - 4)
                                           else liste !! (i - 3)
                                 else liste !! (i - 2)
                        else liste !! (i - 1)
                      | otherwise = x

--map (\ x -> (( \ y -> y * 2) x ) + 1 ) [1..50]

--map f [4,2,5,6]
--    where 
--       f x = x + 1


-- indexOf fonction
indexOf :: (Eq a) => a -> [a] -> Int
indexOf n [] = -1
indexOf n (x:xs)
    | n == x    = 0
    | otherwise = case n `indexOf` xs of
        -1 -> -1
        i  -> i + 1

-- removeFirst fonction
removeFirst :: [a] -> [a]
removeFirst = \myList ->
    case myList of
        [] -> [] -- if the list is empty, return empty list
        x:xs -> xs -- split head and return remaining list

-- append fonction
append :: [Char] -> [Char] -> [Char]
append a xs = a ++ xs

-- countOccurrence fonction
countOccurrence :: Eq a => a -> [a] -> Int
countOccurrence x = length . filter (x==)

-- incrementerVal fonction
incrementerVal :: Integer -> Integer
incrementerVal i = i + 1

extraireInstrument :: Int -> String -> [[String]]
extraireInstrument i contents = extraireElement i (splitString (splitChar (=='+') (replace contents "instrument\n" "+")))

extrairePiste :: Int -> String -> [String]
extrairePiste i contents = extraireDonneeNoPorcess i (splitString (splitChar (=='+') (replace contents "pistes " "+")))




traiterPiste :: Double -> String -> [String] -> [[Double]]
traiterPiste dc instrumentsContents piste = map (\x -> (postTraitmentNote instrument (fst x) (snd x))) nvPiste 
    where
      instrumentId = piste !! 0
      instrument = extraireInstrument ((read instrumentId)) instrumentsContents
      nvPiste = contaOcs (remplacerTirer (removeFirst piste ))
      postTraitmentNote instrument n note = (map (\i -> (traiterNote i instrument note )) [0.0..(((fromIntegral n :: Double)*dc*44100) -1)])
      traiterNote i [] note = 0
      traiterNote i (x:xs) note = (runFunc (x !! 0) i x  (words note)) + traiterNote i xs note


runFunc :: String -> Double -> [String] -> [String] -> Double
runFunc "sin" i a b = Main.sin i a b
runFunc "pulse" i a b = pulse i a b
runFunc "triangle" i a b = triangle i a b

decouperNotes :: [String] -> [[String]]
decouperNotes liste = map words liste

-- FONCTIONS WAV
--sin :: [String]-> [String]-> Double
--sin liste1 liste2 = 5.0
--pulse :: [String]-> [String]-> Double
--pulse liste1 liste2 = 6.0
--triangle :: [String]-> [String]-> Double
--triangle liste1 liste2 = 7.0

_Z :: Double
_Z = 2**(1/12)

_Pi :: Double
_Pi = 3.14

listeDemiTons :: [String]
listeDemiTons = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

-- [ 0   1    2  ]
-- [ 0   c    v  ] u
-- calculer l'amplitude d'une note (la hauteur)
calculerAmplitude :: Double -> Double -> Double
calculerAmplitude u c  = ( 12 * c ) + u - 57

-- calculer la fréquence d'une note (la hauteur)
-- h = calculerAmplitude u c  
-- [ 0   c    v  ] u
calculerFrequence :: [String] -> Double
calculerFrequence paramNote | paramNote !! 0 == "silence" = 0.0
                            | otherwise = 440 * (_Z)**h 
                             where 
                                h = calculerAmplitude (fromIntegral ( indexOf ( paramNote !! 0 ) listeDemiTons ) :: Double) ( toDouble ( paramNote !! 1 ) )

-- 2. calculer phase
calculerPhase :: Double -> Double -> Double -> Double
calculerPhase tk phase frequence = (tk  + phase) * frequence - (fromIntegral y :: Double) where y = floor (x) where x = (tk + phase) * frequence  
						         

-- 1. calculerEbl
calculerEbl :: Double -> Double -> Double -> Double -> Double
calculerEbl tk ebl a1bl a2bl 
    | tk <= ebl = a1bl
    | otherwise = a2bl



-- ["sin","ebl","a1bl","a2bl","qbl"]["A","1","2.0"]
-- [  0     1     2      3       4 ][ 0   1    2  ]
--									[ 0   c    v  ] u
sin :: Double -> [String] -> [String] -> Double
sin k paramSin paramNote | paramNote !! 0 == "silence" = 0.0
                                  | otherwise = v * ebltk * Prelude.sin ( x ) 
                                    where 
                                        x = 2 * _Pi * f * (k/44100) + toDouble ( paramSin !! 4 )
                                        ebltk = calculerEbl (k/44100) ( toDouble ( paramSin !! 1 ) ) ( toDouble ( paramSin !! 2 )) (toDouble ( paramSin !! 3 ) )
                                        f = calculerFrequence paramNote
                                        v = toDouble ( paramNote !! 2 )

--              0     1   2   3     4   5
-- 1. calculer pulse ebl a1bl a2bl qbl phase
pulse :: Double -> [String] -> [String] -> Double
pulse k paramPulse paramNote | paramNote !! 0 == "silence" = 0.0
                              | ph < (toDouble ( paramPulse !! 4 )) = v * ebltk
                              | otherwise = v * negate ( ebltk ) 
                                where 
                                    ph = calculerPhase (k/44100) (toDouble ( paramPulse !! 5 )) f
                                    f = calculerFrequence paramNote 
                                    ebltk = calculerEbl (k/44100) ( toDouble ( paramPulse !! 1 ) ) ( toDouble ( paramPulse !! 2 )) (toDouble ( paramPulse !! 3 ) )
                                    v = toDouble ( paramNote !! 2 )


--                 0     1   2   3     4   5
-- 1. calculer Triangle ebl a1bl a2bl qbl phase
-- 1. calculer Triangle ebl a1bl a2bl qbl phase
triangle :: Double -> [String] -> [String] -> Double
triangle k paramTriangle paramNote | paramNote !! 0 == "silence" = 0.0
                                   | ph < qbl = v * ebltk * ( ( ( 2 * ph ) / qbl ) - 1.0 )
                                   | otherwise = v* ebltk * ( 1.0 - ( 2 * ( ph - qbl ) / ( 1.0 - qbl ) ) )
                                        where 
                                            ph = calculerPhase (k/44100) (toDouble ( paramTriangle !! 5 )) f
                                            f = calculerFrequence paramNote 
                                            qbl = toDouble ( paramTriangle !! 4 )
                                            v = toDouble ( paramNote !! 2 )
                                            ebltk = calculerEbl (k/44100) ( toDouble ( paramTriangle !! 1 ) ) ( toDouble ( paramTriangle !! 2 )) (toDouble ( paramTriangle !! 3 ) )
 
 
calculerWk :: Double -> [Double] -> Double
calculerWk volume valeurs = volume * sum (valeurs)


------ patron 
-- piste 1 [calculerWk, calculerWk ...]
-- patron1 [ piste 1 , piste 2 ] , patron2 [ piste 2 , piste 3 ] 
--                             =
-- patron1 [ sum (piste 1 [calculerWk, calculerWk ...] + piste 2 [calculerWk, calculerWk ...])]
-- patron2 [ sum (piste 2 [calculerWk, calculerWk ...] + piste 3 [calculerWk, calculerWk ...])]
-- patron finale patron1 ++ patron2



           

