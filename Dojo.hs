{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=" #-}
{-# HLINT ignore "Use >" #-}

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



-- da fare (metodo per simulare l'intera partita)


eseguiUnTurno :: ConfigurazioneDojo -> ConfigurazioneDojo  -- da fare (Sposta lo scontro da prima a dopo il movimento [attento ai controlli delle distanze])
eseguiUnTurno ([], listaOggetti) = ([], listaOggetti)
eseguiUnTurno (senpaiDiTurno : listaSenpai, []) = (listaSenpaiDopoScontro ++ [senpaiSpostato],[])
       where
              (senpaiDopoScontro,listaSenpaiDopoScontro) = sfidaSenpaiVicini (senpaiDiTurno,listaSenpai)
              senpaiMinorePiuVicino = trovaSenpaiMinorePiuVicino senpaiDopoScontro listaSenpaiDopoScontro
              senpaiSpostato = muoviSenpai senpaiDopoScontro (getCoordinataFromSenpai senpaiMinorePiuVicino) listaSenpaiDopoScontro
eseguiUnTurno (senpaiDiTurno : listaSenpai, listaOggetti) = (listaSenpaiDopoScontro ++ [senpaiDopoAverRaccoltoOggetto], listaOggettiDopoRaccolta)
       where
              (senpaiDopoScontro,listaSenpaiDopoScontro) = sfidaSenpaiVicini (senpaiDiTurno,listaSenpai)  -- in listaSenpaiDopoScontro non c'è il senpaiDiturno
              senpaiMinorePiuVicino = trovaSenpaiMinorePiuVicino senpaiDopoScontro listaSenpaiDopoScontro
              oggettoPiuVicino = trovaOggettoPiuVicino senpaiDopoScontro listaOggetti
              cst = getCoordinataFromSenpai senpaiDopoScontro
              csv = getCoordinataFromSenpai senpaiMinorePiuVicino
              cov = getCoordinataFromOggetto oggettoPiuVicino
              senpaiSpostato | senpaiDopoScontro /= senpaiMinorePiuVicino && distanza cst csv < distanza cst cov  -- Controllo se conviene di più avvicinarsi ad un senpai più debole o ad un'oggetto (escludendo se stesso)
                                   = muoviSenpai senpaiDopoScontro csv listaSenpaiDopoScontro
                             | otherwise = muoviSenpai senpaiDopoScontro cov listaSenpaiDopoScontro
              senpaiDopoAverRaccoltoOggetto = fst (raccoltaOggetto senpaiSpostato listaOggetti)
              listaOggettiDopoRaccolta = snd (raccoltaOggetto senpaiSpostato listaOggetti)


muoviSenpai :: Senpai -> Coordinata -> [Senpai] -> Senpai
muoviSenpai (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1) (x2,y2) listaSenpai
       | x1 == x2 && y1 == y2 && not senpaiInPericolo = (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1)  -- il senpai non deve spostarsi
       | x1 < x2 && check (x1 + 1,y1) = ((x1 + 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento a destra
       | y1 < y2 && check (x1,y1 + 1) = ((x1,y1 + 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento in alto
       | y1 > y2 && check (x1,y1 - 1) = ((x1,y1 - 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento in basso
       | x1 > x2 && check (x1 - 1,y1) = ((x1 - 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento a sinistra
                                                                             -- nessun movimento sicuro per avvicinarsi all'obbiettivo
                                                                             -- quindi un altro senpai piu' forte lo sta ostacolando
       | senpaiInPericolo && check (x1 + 1,y1) = ((x1 + 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo a destra
       | senpaiInPericolo && check (x1,y1 + 1) = ((x1,y1 + 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo in alto
       | senpaiInPericolo && check (x1,y1 - 1) = ((x1,y1 - 1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo in basso
       | senpaiInPericolo && check (x1 - 1,y1) = ((x1 - 1,y1),umilta1,coraggio1,gentilezza1,rispetto1)  -- movimento elusivo a sinistra
       | otherwise = (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1)  -- il senpai non ha via di scampo
       where
              x1 = fst (getCoordinataFromSenpai (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1))
              y1 = snd (getCoordinataFromSenpai (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1))
              check c = dentroILimiti c && controllaSicurezzaCasella (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1) listaSenpai c
              senpaiInPericolo = not (check (x1,y1))


sfidaSenpaiVicini :: (Senpai,[Senpai]) -> (Senpai,[Senpai])  -- da fare (incrementa valore senpai dopo la vittoria && rimuovi la ridondanza dell'input)
sfidaSenpaiVicini (senpai,[]) = (senpai,[])  -- non c'è nessuno rimasto da sfidare
sfidaSenpaiVicini (senpai,listaSenpai) | distanza cs1 cs2 == 1 && maggioreDi senpai senpaiDaSconfiggere
                                                 = sfidaSenpaiVicini (senpaiDopoScontro,listaSenpaiDopoScontro)  -- sconfigge un senpai vicino e controlla se ce n'è un'altro
                                       | otherwise = (senpai,listaSenpai) -- non ci sono senpai da sfidare
       where
              senpaiDaSconfiggere = trovaSenpaiMinorePiuVicino senpai listaSenpai
              cs1 = getCoordinataFromSenpai senpai
              cs2 = getCoordinataFromSenpai senpaiDaSconfiggere
              senpaiDopoScontro = applicaBeneficioScontro senpai senpaiDaSconfiggere
              listaSenpaiDopoScontro = rimuoviUnElementoDallaLista listaSenpai senpaiDaSconfiggere


raccoltaOggetto :: Senpai -> [Oggetto] -> (Senpai,[Oggetto])
raccoltaOggetto senpai [] = (senpai,[])  -- nessun oggetto da raccogliere oppure nessun oggetto raggiunto
raccoltaOggetto senpai listaOggetti | distanza cs co == 0 = (senpaiPotenziato, nuovaLista)
                                    | otherwise = (senpai,listaOggetti)  -- non faccio nulla
       where
              oggettoDaRaccogliere = trovaOggettoPiuVicino senpai listaOggetti
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


controllaSicurezzaCasella :: Senpai -> [Senpai] -> Coordinata -> Bool
controllaSicurezzaCasella s1 [] (x1,y1) = True -- tutti gli avversari sono stati analizzati
controllaSicurezzaCasella s1 (s2 : listaSenpai) (x1,y1) | abs (x1-x2) + abs (y1-y2) <= 1 && maggioreDi s2 s1 = False  -- minaccia rilevata nella coordinata indicata
                                                        | otherwise = controllaSicurezzaCasella s1 listaSenpai (x1,x2)  -- esaminazione prossimo senpai avversario
       where
              x2 = fst (getCoordinataFromSenpai s2)
              y2 = snd (getCoordinataFromSenpai s2)


rimuoviUnElementoDallaLista :: Eq a => [a] -> a -> [a]
rimuoviUnElementoDallaLista [] _ = []
rimuoviUnElementoDallaLista (y : ys) x | x == y     = ys       -- trovo la ricorrenza, quindi rimuovo l'elemento e mi fermo
                                       | otherwise  = y : rimuoviUnElementoDallaLista ys x


distanza :: Coordinata -> Coordinata -> Int
distanza (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2) -- non calcolo l'ipotenusa nella distanza perché non esistono movimenti diagonali


trovaSenpaiMinorePiuVicino :: Senpai -> [Senpai] -> Senpai
trovaSenpaiMinorePiuVicino senpaiTarget (s : listaSenpai) | senpaiTarget == s = trovaSenpaiMinorePiuVicino senpaiTarget listaSenpai -- escudere se stesso (primo elemento)
trovaSenpaiMinorePiuVicino senpaiTarget [] = senpaiTarget  -- non esiste nessun senpai che possa esser battuto
trovaSenpaiMinorePiuVicino senpaiTarget [s]      | maggioreDi senpaiTarget s = s -- ultima ricorsione; rimane solo il sempai piu' vicino
                                                 | otherwise = senpaiTarget  -- non esiste nessun senpai che possa esser battuto
trovaSenpaiMinorePiuVicino senpaiTarget (s1 : s2 : listaSenpai) | senpaiTarget == s2 = trovaSenpaiMinorePiuVicino senpaiTarget (s1 : listaSenpai)  -- escudere se stesso (secondo elemento)
trovaSenpaiMinorePiuVicino senpaiTarget (s1 : s2 : listaSenpai) | maggioreDi senpaiTarget s1 && distanza cst cs1 < distanza cst cs2 =
                                                                      trovaSenpaiMinorePiuVicino senpaiTarget (s1 : listaSenpai) --mantengo s1 perché piu' vicino
                                                                | otherwise = trovaSenpaiMinorePiuVicino senpaiTarget (s2 : listaSenpai) --mantengo s2 perché piu' vicino
       where
              cst = getCoordinataFromSenpai senpaiTarget
              cs1 = getCoordinataFromSenpai s1
              cs2 = getCoordinataFromSenpai s2


trovaOggettoPiuVicino :: Senpai -> [Oggetto] -> Oggetto
trovaOggettoPiuVicino senpai [o] = o
trovaOggettoPiuVicino senpai (oggetto1 : oggetto2 : listaOggetti)     | distanza cs co1 < distanza cs co2
                                                                             = trovaOggettoPiuVicino senpai (oggetto1 : listaOggetti)
                                                                      | otherwise = trovaOggettoPiuVicino senpai (oggetto2 : listaOggetti)
       where
              cs = getCoordinataFromSenpai senpai
              co1 = getCoordinataFromOggetto oggetto1
              co2 = getCoordinataFromOggetto oggetto2



getCoordinataFromSenpai :: Senpai -> Coordinata
getCoordinataFromSenpai (c,_,_,_,_) = c


getCoordinataFromOggetto :: Oggetto -> Coordinata
getCoordinataFromOggetto (Vaso c) = c
getCoordinataFromOggetto (Spada c) = c
getCoordinataFromOggetto (Fazzoletto c) = c
getCoordinataFromOggetto (Scopa c) = c


maggioreDi :: Senpai -> Senpai -> Bool
maggioreDi ((x1,y1),umilta1,coraggio1,gentilezza1,rispetto1) ((x2,y2),umilta2,coraggio2,gentilezza2,rispetto2)
              | calcolaValoreSenpai umilta1 coraggio1 gentilezza1 rispetto1 /= calcolaValoreSenpai umilta2 coraggio2 gentilezza2 rispetto2 =
                            calcolaValoreSenpai umilta1 coraggio1 gentilezza1 rispetto1 > calcolaValoreSenpai umilta2 coraggio2 gentilezza2 rispetto2
              |otherwise = (div ((x1+y1)*(x1+y1-1)) 2 +x1-y1) > (div ((x2+y2)*(x2+y2-1)) 2 +x2-y2)


calcolaValoreSenpai :: Virtu -> Virtu -> Virtu -> Virtu -> Int
calcolaValoreSenpai (Umilta v1) (Coraggio v2) (Gentilezza v3) (Rispetto v4) = v1 + v2 + v3 + v4







arraySenapais :: [Senpai]
arraySenapais = [    ((5,5),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
                     ((6,5),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
                     ((4,5),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
                     ((5,6),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
                     ((5,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0) ]
arrayOggettos :: [Oggetto]
arrayOggettos = [Vaso (1,1), Scopa (10,10)]
{-
arraySenapais = [((2,4),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
              ((3,4),Umilta 2,Coraggio 2,Gentilezza 2,Rispetto 2),
              ((4,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)]
-}
dojo1 :: ConfigurazioneDojo
dojo1 = (arraySenapais,arrayOggettos)

{- test
dojo :: ConfigurazioneDojo
dojo = (
  [((2,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((3,8),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((4,1),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((9,6),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)],
  [Vaso (7,7),Vaso (1,1),Spada (5,1),Spada (9,9),Fazzoletto (4,3),Scopa (8,7),Scopa (6,1),Scopa (4,5)]
  )

dojo1 = (
       [((2,4),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
              ((3,4),Umilta 2,Coraggio 2,Gentilezza 2,Rispetto 2),
              ((4,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)],
       []
       )


senpai1 :: Senpai
senpai2 :: Senpai
senpai3 :: Senpai
senpai4 :: Senpai
senpai1 = ((2,4),Umilta 0,Umilta 0,Umilta 0,Umilta 0)
senpai2 = ((3,4),Umilta 0,Umilta 0,Umilta 0,Umilta 0)
senpai3 = ((2,6),Umilta 0,Umilta 0,Gentilezza 0,Umilta 0)
senpai4 = ((5,7),Umilta 0,Umilta 0,Umilta 0,Umilta 0)

oggetto1 :: Oggetto
oggetto2 :: Oggetto
oggetto3 :: Oggetto
oggetto4 :: Oggetto
oggetto1 = Vaso (1,2)
oggetto2 = Vaso (1,2)
oggetto3 = Scopa (1,2)
oggetto4 = Vaso (3,5)
-}
