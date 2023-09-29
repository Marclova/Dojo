{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=" #-}
{-# HLINT ignore "Use >" #-}

limiteScacchiera :: Coordinata
limiteScacchiera = (10,10)

dentroILimiti :: Coordinata -> Bool
dentroILimiti coordinata = fst coordinata <= fst limiteScacchiera && snd coordinata <= snd limiteScacchiera

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
       show (Vaso c) = "Vaso" ++ show c
       show (Spada c) = "Spada" ++ show c
       show (Fazzoletto c) = "Fazzoletto" ++ show c
       show (Scopa c) = "Scopa" ++ show c


instance Show Virtu where
       show (Umilta v) = "umilta" ++ show v
       show (Coraggio v) = "coraggio" ++ show v
       show (Gentilezza v) = "gentilezza" ++ show v
       show (Rispetto v) = "rispetto" ++ show v



eseguiUnTurno :: ConfigurazioneDojo -> ConfigurazioneDojo  -- da fare (logica spostamenti con gli oggetti && raccolta oggetti)
eseguiUnTurno ([], listaOggetti) = ([], listaOggetti)
eseguiUnTurno (senpaiDiTurno : listaSenpai, []) = (listaSenpaiDopoScontro ++ [senpaiSpostato],[])
       where
              listaSenpaiDopoScontro = sfidaSenpaiVicini senpaiDiTurno listaSenpai listaSenpai
              senpaiMinorePiuVicino = trovaSenpaiMinorePiuVicino senpaiDiTurno listaSenpaiDopoScontro
              senpaiSpostato = muoviSenpai senpaiDiTurno (getCoordinataFromSenpai senpaiMinorePiuVicino) listaSenpaiDopoScontro
eseguiUnTurno (senpaiDiTurno : listaSenpai, listaOggetti) = (listaSenpaiDopoScontro ++ [senpaiSpostato], listaOggetti)
       where
              listaSenpaiDopoScontro = sfidaSenpaiVicini senpaiDiTurno listaSenpai listaSenpai
              senpaiMinorePiuVicino = trovaSenpaiMinorePiuVicino senpaiDiTurno listaSenpaiDopoScontro
              oggettoPiuVicino = trovaOggettoPiuVicino senpaiDiTurno listaOggetti
              senpaiSpostato = muoviSenpai senpaiDiTurno (getCoordinataFromSenpai senpaiMinorePiuVicino) listaSenpaiDopoScontro



sfidaSenpaiVicini :: Senpai -> [Senpai] -> [Senpai] -> [Senpai]  -- da fare (incrementa valore senpai dopo la vittoria)
sfidaSenpaiVicini _ _ [s] = [s]  -- non c'è nessuno rimasto da sfidare
sfidaSenpaiVicini _ [] effettivaListaSenpai = effettivaListaSenpai  -- fine iterazione
sfidaSenpaiVicini senpai (s : listaSenpai) effettivaListaSenpai       | distanza cs1 cs2 == 1 && maggioreDi senpai s
                                                                             = sfidaSenpaiVicini senpai listaSenpai (rimuoviUnElementoDallaLista effettivaListaSenpai s)  -- sconfigge un senpai vicino e controlla se ce n'è un'altro
                                                                      | otherwise = sfidaSenpaiVicini senpai listaSenpai effettivaListaSenpai  -- nessuno scontro e si continua con l'iterazione
       where
              cs1 = getCoordinataFromSenpai senpai
              cs2 = getCoordinataFromSenpai s


muoviSenpai :: Senpai -> Coordinata -> [Senpai] -> Senpai
muoviSenpai (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1) (x2,y2) listaSenpai
       | x1 == x2 && y1 == y2 = (coordinata1,umilta1,coraggio1,gentilezza1,rispetto1)  -- il senpai non deve spostarsi
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


controllaSicurezzaCasella :: Senpai -> [Senpai] -> Coordinata -> Bool
controllaSicurezzaCasella s1 [] (x1,y1) = True -- tutti gli avversari sono stati analizzati
controllaSicurezzaCasella s1 (s2 : listaSenpai) (x1,y1) | abs (x1-x2) + abs (y1-y2) <= 1 && maggioreDi s2 s1 = False  -- minaccia rilevata nella coordinata indicata
                                                    | otherwise = controllaSicurezzaCasella s1 listaSenpai (x1,x2)  -- esaminazione prossimo senpai avversario
       where
              x2 = fst (getCoordinataFromSenpai s2)
              y2 = snd (getCoordinataFromSenpai s2)

{-
eseguiScontri :: [Senpai] -> [Senpai] -> [Senpai]  -- iterazione per far sfidare tutti i senpai tra loro
eseguiScontri [s] actualSenpais = actualSenpais  -- fine iterazione
eseguiScontri (senpai : listaSenpai) actualSenpais      | distanza senpai senpaiDasconfiggere == 1 && maggioreDi senpai senpaiDasconfiggere
                                                               = eseguiScontri listaSenpaiFiltrato actualSenpaisFiltrato  --un senpai viene sconfitto
                                                        |otherwise = eseguiScontri listaSenpai actualSenpais  --il senpai di turno non può sconfiggere nessuno
       where
              senpaiDasconfiggere = trovaSenpaiMinorePiuVicino senpai actualSenpais  --trovo il senpai da sconfiggere dalla lista vera, non l'iterazione
              listaSenpaiFiltrato = rimuoviUnElementoDallaLista listaSenpai senpaiDasconfiggere  --rimozione dall'iterazione
              actualSenpaisFiltrato = rimuoviUnElementoDallaLista actualSenpais senpaiDasconfiggere  --effettiva rimozione dal gioco
-}



rimuoviUnElementoDallaLista :: Eq a => [a] -> a -> [a]
rimuoviUnElementoDallaLista [] _ = []
rimuoviUnElementoDallaLista (y : ys) x | x == y     = ys       -- trovo la ricorrenza, quindi rimuovo l'elemento e mi fermo
                                       | otherwise  = y : rimuoviUnElementoDallaLista ys x


distanza :: Coordinata -> Coordinata -> Int
distanza (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2) -- non calcolo l'ipotenusa nella distanza perché non esistono movimenti diagonali


trovaSenpaiMinorePiuVicino :: Senpai -> [Senpai] -> Senpai
trovaSenpaiMinorePiuVicino senpaiTarget (s : listaSenpai) | senpaiTarget == s = trovaSenpaiMinorePiuVicino senpaiTarget listaSenpai --escudere se stesso
trovaSenpaiMinorePiuVicino senpaiTarget [s]      | maggioreDi senpaiTarget s = s -- ultima ricorsione; rimane solo il sempai piu' vicino
                                                 | otherwise = senpaiTarget  -- non esiste nessun senpai che possa esser battuto
trovaSenpaiMinorePiuVicino senpaiTarget (s1 : s2 : listaSenpai)       | maggioreDi senpaiTarget s1 && distanza cst cs1 < distanza cst cs2 =
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






dojo1 :: ConfigurazioneDojo
dojo1 = (
       [((2,4),Umilta 1,Coraggio 1,Gentilezza 1,Rispetto 1),
              ((3,4),Umilta 2,Coraggio 2,Gentilezza 2,Rispetto 2),
              ((4,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)],
       []
       )

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
