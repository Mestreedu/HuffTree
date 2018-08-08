module Projeto_Paradigmas.Huffman where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Function

class Eq a => Bits a where
    zer :: a
    one :: a
 

type Codemap a = M.Map Char [a]
 

instance Bits Bool where
     zer = False
     one = True

instance Bits Int where
     zer = 0
     one = 1

--Monta a tabela de uma determinada string
montaTabela:: String->[([Char],Int)]
montaTabela a = zip (map (goList)(tiraRep a)) (contaRep a)

--Converte um Char pra uma String. Para manipulação.
goList::Char->[Char]
goList a = [a]
 --Tira Repetição
tiraRep:: String->[Char]
tiraRep [] = []    
tiraRep(x:xs) = x:(tiraRep(filter(/=x) xs))
 --Conta Repetição
contaRep:: String->[Int]
contaRep [] = []
contaRep (x:xs) = 1+(length (filter(== x) xs)):(contaRep[y|y<-xs,y/=x])

--Ordena em ordem crescente (QuickSort)
ordena::[([Char],Int)]->[([Char],Int)]
ordena [] = []
ordena (x:xs) = ordena[y|y<- xs,(snd y)<(snd x)] ++ [x]++ ordena[y|y<- xs,snd y>=(snd x)]

--Estrutura de árvore
data Arvore a = No a (Arvore a) (Arvore a) |Folha a |Null deriving (Show)

--Soma dois elementos
somaFilhos:: Arvore ([Char],Int)-> Arvore ([Char],Int)->Arvore ([Char],Int)
somaFilhos a b = No (aglutina (transform a) (transform b)) (a) (b)

--Transforma uma tupla numa folha
tuplaFolha:: (String,Int)->Arvore (String,Int)
tuplaFolha a = Folha a

--Monta uma lista com os elementos que serão as folhas da árvore
montaFolhas::String->[Arvore ([Char],Int)]
montaFolhas [] = []
montaFolhas x = (tuplaFolha (head (ordena(montaTabela x)))):(montaFolhas  (tail x))

--Soma duas tuplas.Usar pros valores dos nós
aglutina::([Char],Int)->([Char],Int)->([Char],Int)
aglutina a b = (fst a ++ fst b, (snd a)+(snd b))

--Ordena elementos da lista de árvores. Quicksort
ordenaArv:: [Arvore ([Char],Int)]->[Arvore ([Char],Int)]
ordenaArv [] = []
ordenaArv (x:xs) =ordenaArv[y|y<-xs,(snd $ getTupla y)<(snd $getTupla x)]++[x] ++ ordenaArv[y|y<-xs,(snd $ getTupla y)>= (snd $getTupla x)]

--Função para auxiliar a ordenação da lista de árvores, utilizada para acessar os valores das tuplas.
getTupla (No a  ) = a
--Serializa o valor da árvore. Para manipular as tuplas
transform:: Arvore ([Char],Int)-> ([Char],Int)
transform (Folha a) = a
transform (No a f1 f2) = a  

fazArvore:: [Arvore ([Char],Int)]-> [Arvore ([Char],Int)]
fazArvore ([]) = []
fazArvore (x:[]) = [x]
fazArvore (x:xs:xxs) =  fazArvore $ ordenaArv[y| y<-(((somaFilhos x xs):(fazArvore xxs)))]


--montaPai::[Arvore ([Char],Int)]->[Arvore ([Char],Int)]->Arvore ([Char],Int)
--montaPai a b = somaFilhos a b

--Monta a Arvore
--montaArvore::[Arvore ([Char],Int)]->[Arvore([Char],Int)]
--montaArvore [] = []
--montaArvore (x:xs:xxs) = No (montaPai x xs)
---Decodificando uma string binária
--decodifica::Arvore->String->String
--decodifica raiz string = aux raiz string where
--aux(Folha c) string = c:(aux raiz string)
--aux arv "" = ""
--aux(No esquerda direita) ('0':string) = aux esquerda string
--aux(No esquerda direita) ('1':string) = aux direita string


data Arvore2  = Leaf Char Int
            | Fork Arvore2 Arvore2 Int
            deriving (Show)

peso :: Arvore2 -> Int
peso (Leaf _ w)    = w
peso (Fork _ _ w) = w

intercalar t1 t2 = Fork t1 t2 (peso t1 + peso t2)

freqLista :: String -> [(Char, Int)]
freqLista = M.toList . M.fromListWith (+) . map (flip (,) 1)

montarArvore :: [(Char, Int)] -> Arvore2
montarArvore = construa . map (uncurry Leaf) . sortBy (compare `on` snd)
    where  construa (x:[])    = x
           construa (a:b:xs) = construa $ insertBy (compare `on` peso) (intercalar a b) xs


buildCodemap :: Bits a => Arvore2 -> Codemap a
buildCodemap = M.fromList . buildCodelist
     where  buildCodelist (Leaf c w)    = [(c, [])]
            buildCodelist (Fork l r w)  = map (addBit zer) (buildCodelist l) ++ map (addBit one) (buildCodelist r)
              where addBit b = second (b :)

stringTree :: String -> Arvore2
stringTree = montarArvore . freqLista


stringCodemap :: Bits a => String -> Codemap a
stringCodemap = buildCodemap . stringTree

encode :: Bits a => Codemap a -> String -> [a]
encode m = concat . map (m M.!)
 
encode' :: Bits a => Arvore2 -> String -> [a]
encode' t = encode $ buildCodemap t