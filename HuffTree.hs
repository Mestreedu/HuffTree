module Projeto_Paradigmas.Huffman where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Function
import Data.List
import Data.Maybe
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
data Arvore a = No a (Arvore a) (Arvore a)|Folha a deriving (Show)
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
getTupla:: Arvore ([Char],Int)->([Char],Int)
getTupla (Folha a) = a
getTupla (No a _ _) = a
--Serializa o valor da árvore. Para manipular as tuplas
transform:: Arvore ([Char],Int)-> ([Char],Int)
transform (Folha a) = a
transform (No a f1 f2) = a  
--Monta a Arvore
fazArvore:: [Arvore ([Char],Int)]-> [Arvore ([Char],Int)]
fazArvore [] = []
fazArvore (x:[]) = [x]
fazArvore (x:xs:xxs) =  fazArvore $ ordenaArv[y| y<-(((somaFilhos x xs):(fazArvore xxs)))]
--Monta lista de char e suas representações binárias
montaTable::[Arvore ([Char],Int)]->String->[(Char,String)]
montaTable _ [] = []
montaTable t (x:xs) = (x,(percorre (head t) x)):montaTable t xs
--Percorre a arvore
percorre:: Arvore ([Char],Int) ->Char->String
percorre (Folha a) c = []
percorre (No n esq dir) c 
        |elem c $ fst(transform esq) = '0':(percorre esq c)
        |otherwise = '1':(percorre dir c)
montaString:: [(Char,String)]->String
montaString [] = []
montaString (x:xs) = (snd x)++(montaString xs)
--Decodificando uma string binária

decodifica::[Arvore ([Char],Int)]->String->String 
decodifica (x:xs) s = percorreAux x s where
--percorreaux::Arvore ([Char],Int)->String->String
 percorreAux (Folha a) [] = [head $ fst a]
 percorreAux (Folha a) string = (head $ fst a):(percorreAux x string)
 percorreAux (No n esq dir) ('0':string) = percorreAux esq string
 percorreAux (No n esq dir) ('1':string) = percorreAux dir string

maxOrd x = (maximum x, fromJust $ elemIndex (maximum x) x)


--variaveis de teste
--let nome = "Otorrinolaringologista"
--let arvore = fazArvore $ montaFolhas nome
--let tabela = montaTable arvore nome
--let string = montaString tabela