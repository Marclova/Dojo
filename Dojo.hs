{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Use >=" #-}
{-# HLINT ignore "Use >" #-}
import Data.Maybe (fromJust)

limiteScacchiera :: Coordinata
limiteScacchiera = (10,10)

dentroILimiti :: Coordinata -> Bool
dentroILimiti coordinata = x <= limiteX && y <= limiteY && x >= 1 && y >= 1
       where
              x = fst coordinata
              y = snd coordinata
              limiteX = fst limiteScacchiera
              limiteY = snd limiteScacchiera

type Coordinata = (Int,Int)

data Oggetto
    = Vaso Coordinata
    | Spada Coordinata
    | Fazzoletto Coordinata
    | Scopa Coordinata

data Virtu
    = Umilta Int
    | Coraggio Int
    | Gentilezza Int
    | Rispetto Int

type Senpai = (Coordinata,Virtu,Virtu,Virtu,Virtu)

type ConfigurazioneDojo = ([Senpai],[Oggetto])



instance Eq Oggetto where
      Vaso val1 == Vaso val2              = val1 == val2
      Spada val1 == Spada val2            = val1 == val2
      Fazzoletto val1 == Fazzoletto val2  = val1 == val2
      Scopa val1 == Scopa val2            = val1 == val2
      _ == _                              = False

      o1 /= o2 = not (o1 == o2)


instance Eq Virtu where
       Umilta val1 == Umilta val2         = val1 == val2
       Coraggio val1 == Coraggio val2     = val1 == val2
       Gentilezza val1 == Gentilezza val2 = val1 == val2
       Rispetto val1 == Rispetto val2     = val1 == val2
       _ == _                             = False
       v1 /= v2 = not (v1 == v2)


instance Ord Virtu where
       Umilta val1 < Umilta val2 = val1 < val2
       Coraggio val1 < Coraggio val2 = val1 < val2
       Gentilezza val1 < Gentilezza val2 = val1 < val2
       Rispetto val1 < Rispetto val2 = val1 < val2
       v1 <= v2 = v1 < v2 || v1 == v2
       v1 > v2 = not (v1 < v2)
       v1 >= v2 = not (v1 <= v2)

       max v1 v2 | v1 > v2 = v1
              | otherwise = v2
       min v1 v2 | v1 > v2 = v2
              | otherwise = v1


instance Show Oggetto where
       show (Vaso c) = "Vaso " ++ show c
       show (Spada c) = "Spada " ++ show c
       show (Fazzoletto c) = "Fazzoletto " ++ show c
       show (Scopa c) = "Scopa " ++ show c


instance Show Virtu where
       show (Umilta v) = "Umilta " ++ show v
       show (Coraggio v) = "Coraggio " ++ show v
       show (Gentilezza v) = "Gentilezza " ++ show v
       show (Rispetto v) = "Rispetto " ++ show v



checkConfigurazioneInizialeDojo :: ConfigurazioneDojo -> Bool
checkConfigurazioneInizialeDojo ([],_) = False
checkConfigurazioneInizialeDojo (listaSenpai,listaOggetti) = checkListaSenpai listaSenpai && checkListaOggetti listaOggetti

checkListaSenpai :: [Senpai] -> Bool
checkListaSenpai [] = True
checkListaSenpai ((coordinata,Umilta u1,Coraggio c1,Gentilezza g1,Rispetto r1) : lista) | checkSenpai = checkListaSenpai lista
                                                                                        | otherwise = False
       where
              checkSenpai = dentroILimiti coordinata && u1 == 0 && c1 == 0 && g1 == 0 && r1 == 0

checkListaOggetti :: [Oggetto] -> Bool
checkListaOggetti [] = True
checkListaOggetti (oggetto : lista) | dentroILimiti (getCoordinataFromOggetto oggetto) = checkListaOggetti lista
                                    | otherwise = False


stampaSimulazione :: [ConfigurazioneDojo] -> IO ()
stampaSimulazione (dojo : listaDojo) = do
                                          putStrLn (show dojo ++ "\n")
                                          stampaSimulazione listaDojo
stampaSimulazione [] = print ()



calcolaSimulazione :: ConfigurazioneDojo -> [ConfigurazioneDojo]
calcolaSimulazione ([s],listaOggetti) = [([s],listaOggetti)]
calcolaSimulazione dojo = dojo : calcolaSimulazione (eseguiUnTurno dojo)


eseguiUnTurno :: ConfigurazioneDojo -> ConfigurazioneDojo
eseguiUnTurno ([], listaOggetti) = ([], listaOggetti)
eseguiUnTurno (senpaiDiTurno : listaSenpai, listaOggetti) = (listasenpaiDopoSecondoScontro ++ [senpaiDopoSecondoScontro], listaOggettiDopoRaccolta)
       where
              (senpaiDopoPrimoScontro,listasenpaiDopoPrimoScontro,vittoriaOttenuta) = sfidaUnSenpaiVicino (senpaiDiTurno,listaSenpai)
              senpaiMinorePiuVicino = trovaSenpaiMinorePiuVicino senpaiDopoPrimoScontro listasenpaiDopoPrimoScontro
              oggettoPiuVicino = trovaOggettoPiuVicino senpaiDopoPrimoScontro listaOggetti
              cst = getCoordinataFromSenpai senpaiDopoPrimoScontro
              csv = getCoordinataFromSenpai senpaiMinorePiuVicino
              cov | oggettoPiuVicino == Nothing = (-1,-1)  -- -1 coincide con coordinata inesistente
                  | otherwise = getCoordinataFromOggetto (fromJust oggettoPiuVicino)
              senpaiSpostato | senpaiDopoPrimoScontro /= senpaiMinorePiuVicino && distanza cst csv < distanza cst cov  -- Controllo se conviene di più avvicinarsi ad un senpai più debole o ad un'oggetto (escludendo se stesso)
                                   = muoviSenpai senpaiDopoPrimoScontro csv listasenpaiDopoPrimoScontro
                             | otherwise = muoviSenpai senpaiDopoPrimoScontro cov listasenpaiDopoPrimoScontro
              (senpaiDopoAverRaccoltoOggetto,listaOggettiDopoRaccolta) = raccoltaOggetto senpaiSpostato listaOggetti
              (senpaiDopoSecondoScontro,listasenpaiDopoSecondoScontro,_) | not vittoriaOttenuta = sfidaUnSenpaiVicino (senpaiDopoAverRaccoltoOggetto, listasenpaiDopoPrimoScontro)
                                                                         | otherwise = (senpaiDopoAverRaccoltoOggetto, listasenpaiDopoPrimoScontro,False)


muoviSenpai :: Senpai -> Coordinata -> [Senpai] -> Senpai
muoviSenpai (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1) (x2,y2) listaSenpai
       | x1 < x2 && check (x1 + 1,y1) = ((x1 + 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento a destra
       | y1 < y2 && check (x1,y1 + 1) = ((x1,y1 + 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento in alto
       | y1 > y2 && check (x1,y1 - 1) = ((x1,y1 - 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento in basso
       | x1 > x2 && check (x1 - 1,y1) = ((x1 - 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento a sinistra
              -- nessun movimento sicuro per avvicinarsi all'obbiettivo oppure nessun obbiettivo fissato, quindi controllo se il senpai è al sicuro...
       | senpaiAlSicuro = (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1)  -- Nessun movimento elusivo necessario
       | check (x1 + 1,y1) = ((x1 + 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo a destra
       | check (x1,y1 + 1) = ((x1,y1 + 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo in alto
       | check (x1,y1 - 1) = ((x1,y1 - 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo in basso
       | check (x1 - 1,y1) = ((x1 - 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo a sinistra
       | otherwise = (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1)  -- il senpai non ha via di scampo e verrà presto eliminato
       where
              (x1,y1) = coordinata1
              check c = dentroILimiti c && not (casellaOccupata listaSenpai c)
                            && controllaSicurezzaCasella (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1) listaSenpai c
              senpaiAlSicuro = controllaSicurezzaCasella (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1) listaSenpai (x1,y1)


sfidaUnSenpaiVicino :: (Senpai,[Senpai]) -> (Senpai,[Senpai],Bool)
sfidaUnSenpaiVicino (senpai,[]) = (senpai,[],False)  -- non c'è nessuno rimasto da sfidare
sfidaUnSenpaiVicino (senpai,listaSenpai) | distanza cs1 cs2 == 1 = (senpaiDopoScontro,listaSenpaiDopoScontro,True)  -- sconfigge un senpai vicino e riceve i benefici
                                       | otherwise = (senpai,listaSenpai,False) -- non ci sono senpai da sfidare
       where
              senpaiDaSconfiggere = trovaSenpaiMinorePiuVicino senpai listaSenpai  --senpaiDaSconfiggere == senpai se e solo se non vi sono senpai da sconfiggere
              cs1 = getCoordinataFromSenpai senpai
              cs2 = getCoordinataFromSenpai senpaiDaSconfiggere
              senpaiDopoScontro = applicaBeneficioScontro senpai senpaiDaSconfiggere
              listaSenpaiDopoScontro = rimuoviUnElementoDallaLista listaSenpai senpaiDaSconfiggere


raccoltaOggetto :: Senpai -> [Oggetto] -> (Senpai,[Oggetto])
raccoltaOggetto senpai [] = (senpai,[])  -- nessun oggetto da raccogliere oppure nessun oggetto raggiunto
raccoltaOggetto senpai listaOggetti | distanza cs co == 0 = (senpaiPotenziato, nuovaLista)
                                    | otherwise = (senpai,listaOggetti)  -- non faccio nulla
       where
              Just oggettoDaRaccogliere = trovaOggettoPiuVicino senpai listaOggetti  -- rimuovo subito il Just senza controllare che non sia Nothing
              cs = getCoordinataFromSenpai senpai
              co = getCoordinataFromOggetto oggettoDaRaccogliere
              senpaiPotenziato = applicaBeneficioOggetto oggettoDaRaccogliere senpai
              nuovaLista = rimuoviUnElementoDallaLista listaOggetti oggettoDaRaccogliere


applicaBeneficioScontro :: Senpai -> Senpai -> Senpai
applicaBeneficioScontro (coordinata,Umilta u1,Coraggio c1,Gentilezza g1,Rispetto r1) (_,Umilta u2,Coraggio c2,Gentilezza g2,Rispetto r2) =
       (coordinata,Umilta u3,Coraggio c3,Gentilezza g3,Rispetto r3)
       where
              u3 | u1 > u2 = u1 + 1
                 | otherwise = u1
              c3 | c1 > c2 = c1 + 1
                 | otherwise = c1
              g3 | g1 > g2 = g1 + 1
                 | otherwise = g1
              r3 | r1 > r2 = r1 + 1
                 | otherwise = r1


applicaBeneficioOggetto :: Oggetto -> Senpai -> Senpai
applicaBeneficioOggetto (Vaso _) (c,Umilta n,coraggio,gentilezza,rispetto) = (c,Umilta (n+1),coraggio,gentilezza,rispetto)
applicaBeneficioOggetto (Spada _) (c,umilta,Coraggio n,gentilezza,rispetto) = (c,umilta,Coraggio (n+1),gentilezza,rispetto)
applicaBeneficioOggetto (Fazzoletto _) (c,umilta,coraggio,Gentilezza n,rispetto) = (c,umilta,coraggio,Gentilezza (n+1),rispetto)
applicaBeneficioOggetto (Scopa _) (c,umilta,coraggio,gentilezza,Rispetto n) = (c,umilta,coraggio,gentilezza,Rispetto (n+1))


casellaOccupata :: [Senpai] -> Coordinata -> Bool
casellaOccupata [] _ = False
casellaOccupata (s : listaSenpai) c | distanza (getCoordinataFromSenpai s) c == 0 = True
                                    | otherwise = casellaOccupata listaSenpai c


controllaSicurezzaCasella :: Senpai -> [Senpai] -> Coordinata -> Bool
controllaSicurezzaCasella s1 [] (x1,y1) = True -- tutti gli avversari sono stati analizzati
controllaSicurezzaCasella s1 (s2 : listaSenpai) (newX,newY) | abs (newX-x2) + abs (newY-y2) == 1 && maggioreDi s2 s1 = False  -- minaccia rilevata nella coordinata indicata
                                                            | otherwise = controllaSicurezzaCasella s1 listaSenpai (newX,newY)  -- esaminazione prossimo senpai avversario
       where
              x2 = fst (getCoordinataFromSenpai s2)
              y2 = snd (getCoordinataFromSenpai s2)


rimuoviUnElementoDallaLista :: Eq a => [a] -> a -> [a]
rimuoviUnElementoDallaLista [] _ = []
rimuoviUnElementoDallaLista (y : ys) x | x == y     = ys       -- trovo la ricorrenza, quindi rimuovo l'elemento e mi fermo
                                       | otherwise  = y : rimuoviUnElementoDallaLista ys x


trovaSenpaiMinorePiuVicino :: Senpai -> [Senpai] -> Senpai
trovaSenpaiMinorePiuVicino senpaiTarget [] = senpaiTarget
trovaSenpaiMinorePiuVicino senpaiTarget [s] | maggioreDi senpaiTarget s = s -- ultima iterazione
                                            | otherwise = senpaiTarget
trovaSenpaiMinorePiuVicino senpaiTarget (s : listaSenpai) | not (maggioreDi senpaiTarget s) = trovaSenpaiMinorePiuVicino senpaiTarget listaSenpai -- rimozione primo elemento
trovaSenpaiMinorePiuVicino senpaiTarget (s1 : s2 : listaSenpai) | not (maggioreDi senpaiTarget s2) = trovaSenpaiMinorePiuVicino senpaiTarget (s1 : listaSenpai) -- rimozione secondo elemento
                                                                -- sia s1 che s2 sono minori di senpaiTarget
                                                                | distanza cst cs1 < distanza cst cs2 =
                                                                      trovaSenpaiMinorePiuVicino senpaiTarget (s1 : listaSenpai) -- mantengo s1 perché piu' vicino
                                                                | otherwise = trovaSenpaiMinorePiuVicino senpaiTarget (s2 : listaSenpai) -- mantengo s2 perché piu' vicino
       where
              cst = getCoordinataFromSenpai senpaiTarget
              cs1 = getCoordinataFromSenpai s1
              cs2 = getCoordinataFromSenpai s2


trovaOggettoPiuVicino :: Senpai -> [Oggetto] -> Maybe Oggetto
trovaOggettoPiuVicino _ [] = Nothing
trovaOggettoPiuVicino senpai [o] = Just o
trovaOggettoPiuVicino senpai (oggetto1 : oggetto2 : listaOggetti)     | distanza cs co1 < distanza cs co2
                                                                             = trovaOggettoPiuVicino senpai (oggetto1 : listaOggetti)
                                                                      | otherwise = trovaOggettoPiuVicino senpai (oggetto2 : listaOggetti)
       where
              cs = getCoordinataFromSenpai senpai
              co1 = getCoordinataFromOggetto oggetto1
              co2 = getCoordinataFromOggetto oggetto2


distanza :: Coordinata -> Coordinata -> Int
distanza (x1,y1) (x2,y2) | x1 < 0 || y1 < 0 || x2 < 0 || y2 < 0 = maxBound :: Int  -- in caso di coordinate anomale, la distanza è massima
                         | otherwise = abs (x1-x2) + abs (y1-y2) -- non calcolo l'ipotenusa nella distanza perché non esistono movimenti diagonali



getCoordinataFromSenpai :: Senpai -> Coordinata
getCoordinataFromSenpai (c,_,_,_,_) = c


getCoordinataFromOggetto :: Oggetto -> Coordinata
getCoordinataFromOggetto (Vaso c) = c
getCoordinataFromOggetto (Spada c) = c
getCoordinataFromOggetto (Fazzoletto c) = c
getCoordinataFromOggetto (Scopa c) = c


maggioreDi :: Senpai -> Senpai -> Bool
maggioreDi ((x1,y1),umilta1,coraggio1,gentilezza1,rispetto1) ((x2,y2),umilta2,coraggio2,gentilezza2,rispetto2)
              | sommaValoriVirtu umilta1 coraggio1 gentilezza1 rispetto1 /= sommaValoriVirtu umilta2 coraggio2 gentilezza2 rispetto2 =
                            sommaValoriVirtu umilta1 coraggio1 gentilezza1 rispetto1 > sommaValoriVirtu umilta2 coraggio2 gentilezza2 rispetto2
              |otherwise = (div ((x1+y1)*(x1+y1-1)) 2 +x1-y1) > (div ((x2+y2)*(x2+y2-1)) 2 +x2-y2)


sommaValoriVirtu :: Virtu -> Virtu -> Virtu -> Virtu -> Int
sommaValoriVirtu (Umilta v1) (Coraggio v2) (Gentilezza v3) (Rispetto v4) = v1 + v2 + v3 + v4





dojo1 :: ConfigurazioneDojo
dojoTest :: ConfigurazioneDojo
arrayOggettos :: [Oggetto]
arrayOggettos = [Vaso (1,1), Scopa (10,10)]
{-   --scontri
arraySenapais :: [Senpai]
arraySenapais = [    ((6,5),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
                     ((4,5),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
                     ((5,5),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
                     ((5,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
                     ((5,6),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)
                ]
-}
{-   ---scelta obbiettivo
arraySenapais = [    ((1,5),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
                     ((1,10),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)
                ]
-}
--{-   --preservazione
arraySenapais = [((2,4),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
              ((3,5),Umilta 2,Coraggio 2,Gentilezza 2,Rispetto 2),
              ((4,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)]
---}
dojoTest = (arraySenapais,arrayOggettos)




dojo1 = (
  [((2,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((3,8),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((4,1),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((9,6),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)],
  [Vaso (7,7),Vaso (1,1),Spada (5,1),Spada (9,9),Fazzoletto (4,3),Scopa (8,7),Scopa (6,1),Scopa (4,5)]
  )
