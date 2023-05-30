{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=" #-}
{-# HLINT ignore "Use >" #-}

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


{-instance Eq Oggetto where
       Vaso c1 == Vaso c2                 = c1 == c2
       Spada c1 == Scopa c2               = c1 == c2
       Fazzoletto c1 == Fazzoletto c2     = c1 == c2
       Scopa c1 == Scopa c2               = c1 == c2
       _ == _                             = False
       o1 /= o2 = not (o1 == o2)-}

instance Eq Virtu where
       Umilta val1 == Umilta val2         = val1 == val2
       Coraggio val1 == Coraggio val2     = val1 == val2
       Gentilezza val1 == Gentilezza val2 = val1 == val2
       Rispetto val1 == Rispetto val2     = val1 == val2
       _ == _                             = False
       v1 /= v2 = not (v1 == v2)

{-instance Ord Virtu where
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
            | otherwise = v1-}



generaNuovaConfigurazioneDojo :: ConfigurazioneDojo -> ConfigurazioneDojo
generaNuovaConfigurazioneDojo ([], oggettos) = ([], oggettos)
generaNuovaConfigurazioneDojo (senpai : senpais, []) = ([],[])
       where
              senpaiPiuVicino = trovaSenpaiPiuVicino senpai senpais
generaNuovaConfigurazioneDojo (sens, os) = ([],[])


rimuoviUnElementoDallaLista :: Eq a => a -> [a] -> [a]
rimuoviUnElementoDallaLista _ [] = []
rimuoviUnElementoDallaLista x (y : ys) | x == y     = ys       --trovo la ricorrenza, quindi rimuovo l'elemento e mi fermo
                                       | otherwise  = y : rimuoviUnElementoDallaLista x ys


distanza :: Senpai -> Senpai -> Int
distanza ((x1,y1),_,_,_,_) ((x2,y2),_,_,_,_) = abs (x1-x2) + abs (y1-y2) -- non calcolo l'ipotenusa nella distanza perché non esistono movimenti diagonali


--funzioni parziali

--Lancia errore se la lista di senpai ha 0 elementi. Questo perché tale situazione sarebbe anomala e causerebbe una soluzione finale errata.
trovaSenpaiPiuVicino :: Senpai -> [Senpai] -> Senpai
trovaSenpaiPiuVicino senpai (s : senpais) | distanza senpai s == 0 = trovaSenpaiPiuVicino senpai senpais --escudere se stesso
trovaSenpaiPiuVicino senpai [s] = s --ultima ricorsione; rimane solo il sempai più vicino
trovaSenpaiPiuVicino senpai (s1 : s2 : senpais) | distanza senpai s1 < distanza senpai s2 = trovaSenpaiPiuVicino senpai (s1 : senpais) --mantengo il più vicino
                                                | otherwise = trovaSenpaiPiuVicino senpai (s2 : senpais) --mantengo il più vicino













-- test
dojo :: ConfigurazioneDojo
dojo = (
  [((2,4),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((3,8),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((4,1),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0),
       ((9,6),Umilta 0,Coraggio 0,Gentilezza 0,Rispetto 0)],
  [Vaso (7,7),Vaso (1,1),Spada (5,1),Spada (9,9),Fazzoletto (4,3),Scopa (8,7),Scopa (6,1),Scopa (4,5)]
  )


senpai1 :: Senpai
senpai2 :: Senpai
senpai3 :: Senpai
senpai4 :: Senpai
senpai1 = ((2,4),Umilta 0,Umilta 0,Umilta 0,Umilta 0)
senpai2 = ((2,4),Umilta 0,Umilta 0,Umilta 0,Umilta 0)
senpai3 = ((2,4),Umilta 0,Umilta 0,Gentilezza 0,Umilta 0)
senpai4 = ((5,7),Umilta 0,Umilta 0,Umilta 0,Umilta 0)

oggetto1 :: Oggetto
oggetto2 :: Oggetto
oggetto3 :: Oggetto
oggetto4 :: Oggetto
oggetto1 = Vaso (1,2)
oggetto2 = Vaso (1,2)
oggetto3 = Scopa (1,2)
oggetto4 = Vaso (3,5)
