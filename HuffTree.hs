module Projeto_Paradigmas.Huffman where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Function


--Monta a tabela de uma determinada string
montaTabela:: String->[(Char,Int)]
montaTabela a = zip (tiraRep a) (contaRep a)

 --Tira Repetição
tiraRep:: String->[Char]
tiraRep [] = []    
tiraRep(x:xs) = x:(tiraRep(filter(/=x) xs)) 
 --Conta Repetição
contaRep:: String->[Int]
contaRep [] = []
contaRep (x:xs) = 1+(length (filter(== x) xs)):(contaRep[y|y<-xs,y/=x])

--Ordena em ordem crescente (QuickSort)
ordena::[(Char,Int)]->[(Char,Int)]
ordena [] = []
ordena (x:xs) = ordena[y|y<- xs,(snd y)<(snd x)] ++ [x]++ ordena[y|y<- xs,snd y>=(snd x)]

--Estrutura de árvore
data Arvore = No ([Char],Int) Arvore Arvore|Nulo

--Auxiliar
aux a =((fst a):[],snd a) 

--Transforma uma tupla numa folha
tuplaFolha:: (Char,Int)->Arvore
tuplaFolha a = No (aux a) (Nulo) (Nulo)

--Monta uma lista com os elementos que serão as folhas da árvore
montaFolhas::String->[Arvore]
montaFolhas [] = []
montaFolhas x = (tuplaFolha (head (ordena(montaTabela x)))):(montaFolhas  (tail x))

--Muda o tipo do identificador Char pra uma String e soma os inteiros
--Usar pros valores dos nós
aglutina::(Char,Int)->(Char,Int)->([Char],Int)
aglutina a b = ((fst a):[fst b], (snd a)+(snd b))


----Decodificando uma string binária
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


