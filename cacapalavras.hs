--Pedro Paiva Ladeira --

import System.IO     
import Data.Char
-- CONSTANTES --

cpalavras1= ['E','N','G','E','N','H','E','I','R','O',
			 'Q','P','L','I','S','A','D','F','O','B',
			 'F','K','W','X','E','S','T','U','D','O',
			 'S','L','E','T','D','K','J','R','A','L',
			 'D','D','U','R','B','E','I','O','T','T',
			 'A','H','Q','Y','A','L','H','D','U','Y',
			 'Z','O','S','O','E','L','I','U','P','J',
			 'P','C','A','G','S','O','V','P','M','K',
			 'F','H','E','C','Y','I','V','G','O','X',
			 'T','R','S','T','I','B','U','U','C','D']


cpalavras2= ['S','A','B','O','N','E','T','E','J','P',
			 'A','M','H','G','J','L','P','J','A','I',
			 'L','I','M','A','O','L','V','W','C','C',
			 'S','A','B','O','N','E','T','E','A','L',
			 'I','Q','F','C','T','M','U','H','T','E',
			 'C','W','Z','S','E','Y','K','J','I','S',
			 'H','G','D','N','X','C','Q','J','A','L',
			 'A','R','U','I','N','U','S','A','C','O',
			 'K','F','P','O','W','Q','T','K','A','D',
			 'M','A','N','T','E','I','G','A','J','D']
			 
cpalavras3= ['F','L','O','R','E','S','T','A','R','S',
             'K','J','G','D','F','T','J','J','U','V',
             'C','A','P','E','L','A','P','I','S','C',
             'D','T','C','A','D','W','Y','E','R','T',
             'F','D','C','C','S','A','L','U','S','E',
             'Q','L','K','C','A','U','E','A','I','O',
             'A','A','S','A','R','P','R','D','K','A',
             'N','E','N','E','F','T','E','B','A','U',
             'J','K','D','D','S','H','W','L','F','L',
             'I','O','T','U','G','J','J','V','A','A']
			  


cpalavras5= ['L','T','S','O','R',
			 'O','K','E','I','U',
			 'D','R','D','P','T',
			 'I','Y','A','J','Y',
			 'N','T','H','O','R']
			 
cpalavras6 = ['X','T','S','O','R','Q',
			  'O','V','E','W','Q','F',
			  'D','I','D','Z','T','W',
			  'I','L','A','K','Y','U',
			  'N','T','H','O','R','G',
			  'Q','J','S','E','I','D']

			  
			 
lcustos1=   [('A',1),('B',10),('C',18),('D',38),('E',70),('F',10),('G',43),('H',15),('I',54),
			('J',10),('K',44),('L',81),('M',19),('N',39),('O',54),('P',91),('Q',72),('R',30),
			('S',84),('T',75),('U',22),('V',47),('X',66),('Y',59),('Z',32)]
			
lcustos2=   [('A',10),('B',1),('C',1),('D',1),('E',10),('F',1),('G',1),('H',1),('I',10),
			('J',1),('K',1),('L',1),('M',1),('N',1),('O',10),('P',1),('Q',1),('R',1),
			('S',1),('T',1),('U',10),('V',1),('X',1),('Y',1),('Z',1)]

palavras1= ["ENGENHEIRO", "HASKELL", "BITS", "ENGRENAGEM", "COMPUTADOR", "MAQUINA", "ESTUDO"]
			
palavras2= ["SABONETE", "SALSICHA", "MANTEIGA", "LIMAO", "PICLES", "JACA", "COCACOLA", "FANTA"]

palavras3= ["AULA", "ALEGRIA", "CAPELA", "FLORESTA", "LAPIS"]

palavras4= ["JACA","JAMBO"]

palavras5= ["PATO", "JOTA", "PACA", "TYX"]

palavras6= ["THOR", "ODIN", "TYR", "HADES", "VIDAR"]




---------------------------------------------------- CUSTOS -----------------------------------------------
posicaoLet b cl= (length (takeWhile (/= b) letras))
				where 
				letras = [ x | (x,y) <- cl]
				
custoPal bs cl= if null bs
				then 0
				else (custos!!(posicaoLet b cl)) + (custoPal (tail bs) cl)
				where
				b= (head bs)
				letras = [ x | (x,y) <- cl]
				custos = [ y | (x,y) <- cl]


listCustoPal pal cl= if null pal
					then []
					else [((head pal),custoPal (head pal) cl)] ++ listCustoPal (tail pal) cl
		

menorCust pal cpal n cl= [ (x,y) | (x,y)<- (listCustoPal (varredura pal cpal n) cl), y== minimum custinhos]
						where
						custinhos= [ y | (x,y) <- listCustoPal pal cl]			
			   
-------------------------------------------------------------- OCORRENCIAS --------------------------------				
sublista bs ls=  if null ls
				then 0
				else if take(length bs) ls == bs
					 then 1 + sublista bs (tail ls)
					 else sublista bs (tail ls)


analiseLins bs cpal n= if  null cpal
					   then []
					   else [(sublista bs linha, bs),(sublista bs (reverse linha), bs)] ++ analiseLins bs (drop(n)cpal) n
					   
					   where
					   linha = take(n)cpal
					   
listaCols cpal n= if (length cpal) == (n^2-n)
					then []	
					else [[ cpal!!i | i<-[n*0,n*1..(n*(n-1))]]] ++ listaCols (tail cpal) n	
					
analiseCols bs cpal n= if null cpal
					   then 0
					   else sublista bs (head cpal) + sublista bs (reverse (head cpal)) + (analiseCols bs (tail cpal) n)
					   
analiseColsDet bs cpal n= if null cpal                 --Nesta função cpal deve ser aplicado a listaCol--
					      then []
					      else anal1 ++ analiseColsDet bs (tail cpal) n
					   
					   where
					   anal1=[(bs,(sublista bs (head cpal))), (bs,(sublista bs (reverse (head cpal))))] 
					   
				   
varredura pal cpal n= if null pal
					  then[]
					  else if ocorrencialinha >0 || ocorrenciacoluna>0
						  then [(head pal)]++ varredura (tail pal) cpal n
						  else []++varredura (tail pal) cpal n
					  
					  where
					  ocorrencialinha= sum [ x | (x,y)<-analiseLins	 (head pal) cpal n]
					  ocorrenciacoluna= analiseCols (head pal) (listaCols cpal n) n
					  
varreduraOcorr pal cpal n= if null pal
					       then[]
					       else ocorrencialinha+ocorrenciacoluna : varreduraOcorr (tail pal) cpal n
					  
					        where
					        ocorrencialinha= sum [ x | (x,y)<-analiseLins	 (head pal) cpal n]
					        ocorrenciacoluna= analiseCols (head pal) (listaCols cpal n) n
 
---------------------------------------------- ORDENAÇÃO ---------------------------------------------
           
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
		
invertTup list= [(x,y) | (y,x)<-list]

colunasMark cpal n= zip[ x | x<-listaCols cpal n][ y | y<-[1..n]]




----------------------------------------------- LOCALIZAÇÃO ------------------------------------------
varreduraHor pal cpal n= if null pal
					       then 0
					       else ocorrencialinha + varreduraHor (tail pal) cpal n
					  
					  where
					  ocorrencialinha= sum [ x | (x,y)<-analiseLins	 (head pal) cpal n]

varreduraVer pal cpal n= if null pal
					     then 0
					     else ocorrenciacoluna + varreduraVer (tail pal) cpal n
					  
					  where
					  ocorrenciacoluna= analiseCols (head pal) (listaCols cpal n) n	

varreduraDirecaoSentido pal cpal n= if null pal
					               then[]
					               else sentidolinha++sentidocoluna++(varreduraDirecaoSentido (tail pal) cpal n)
						               
					  
					  where

					  spl=(concat[(zip[(analiseLins(head pal) cpal n)!!i | i<-[0,2..(n*2-1)]] (cycle["esquerda-direita"])),(zip[(analiseLins(head pal) cpal n)!!i | i<-[1,3..(n*2-1)]] (cycle["direita-esquerda"]))])
					  sentidolinha= [(y,k)|((x,y),k)<-spl, x>0]
					  spc=(concat[(zip[(analiseColsDet(head pal) (listaCols cpal n) n)!!i | i<-[0,2..(n*2-1)]] (cycle["topo-baixo"])),(zip[(analiseColsDet (head pal) (listaCols cpal n) n)!!i | i<-[1,3..(n*2-1)]] (cycle["baixo-topo"]))])
					  sentidocoluna=[(x,k)|((x,y),k)<-spc, y>0]
					  
sublistaPosi bs ls=  if null ls
				      then 0
				      else if bs /= take(length bs) ls
					       then 1 + sublistaPosi bs (tail ls)
					       else 0 

varreduraPosiLin bs cpal n= if null cpal
						  then []
						  else [(sublistaPosi bs linha) , (sublistaPosi (reverse bs)  linha)] ++ varreduraPosiLin bs (drop(n)cpal) n
						  
						  
						  where
						  linha = take(n)cpal

varreduraPosiCol bs cpal n= if null cpal --usar (listaCols cpal n) no lugar de cpal nessa função --
							then []
							else [(sublistaPosi bs (head cpal)),(sublistaPosi (reverse bs) (head cpal)) ] ++ (varreduraPosiCol bs (tail cpal) n)
							

interpretador pal cpal n= if null pal
						  then[]
						  else intlinha ++ intcoluna++ interpretador (tail pal) cpal n
						  
						  where
						  intlinha= [(x,(head pal)) | x<-(varreduraPosiLin (head pal) cpal n), x/=n]
						  intcoluna=[(x,(head pal)) | x<-(varreduraPosiCol (head pal) (listaCols cpal n) n), x/=n]
						  
---------------------------------- CONSERTO -------------------------------

quase pal cpal n =[ (q,w,e) | ((q,e),w)<- (zip (varreduraDirecaoSentido pal cpal n) [ x | (x,y)<-(interpretador pal cpal n)])]

consertar pal cpal n= [if (z=="baixo-topo"||z=="direita-esquerda") then y+(length x)-1 else y+0 | (x,y,z)<-(quase pal cpal n)]
						
						
						
------------------------------- PARA INPUT OUTPUT ---------------------------

cacapalavras cpal n= if null cpal
					then []
					else take(n)cpal++"\n"++cacapalavras (drop(n)cpal) n
					
					
					
					
					
					

					


------------------------------------------- Exercicio 1 ---------------------------------------------------



ocorrenciamc pal cpal n cl=  if varredura pal cpal n == []
							then error "Nenhuma palavra encontrada na matriz!"
							else  menorCust pal cpal n cl
						
						
					
--------------------------------------------- Exercicio 2 --------------------------------------------------


 
noocorrencias pal cpal n= [ (x,y)| (x,y)<-(quicksort palocor), y>0]
 
                      where
					palocor= zip (pal) (varreduraOcorr pal cpal n)
					
					
		
---------------------------------------- Exercicio 3 ----------------------------------------------------


palavrasmaiscaras pal cpal n cl= invertTup (reverse (quicksort (invertTup (listCustoPal (varredura pal cpal n) cl))))


-----------------------------------------  Exercicio 4 -------------------------------------------------------

dadospalavra pal cpal n= [(x,y,z) | ((x,z),y)<-wip]
						where
						
						wip = zip[(x,z)|(x,c,z)<-(quase pal cpal n)] (consertar pal cpal n)
						
--Condição especial: o programa não reconhece 2 palavras iguais na mesma linha ou coluna e no mesmo sentido, por exemplo a seguinte linha "JACAGHJACA" só reconhece o primeiro
--"JACA" e sua posição, porém na linha "JACAHUACAJ" ele reconhece as duas "JACA"s pois elas estão em sentidos diferentes,
--mas é uma prática muito incomum um caça palavras com 2 palavras iguais numa mesma linha e sentido.

------------------------------------ Exercicio 5 ----------------------------------------------------

direcao pal cpal n=  if  (varreduraHor pal cpal n) == (varreduraVer pal cpal n)
					then [("ambas as direcoes",(varreduraVer pal cpal n))]
					else if (varreduraHor pal cpal n) > (varreduraVer pal cpal n)
						then [("horizontal", (varreduraHor pal cpal n))]
						else [("vertical", (varreduraVer pal cpal n))]
					
				   where
				   listOcorrencia= [varreduraHor pal cpal n,varreduraVer pal cpal n]
				   
				   
				   
--------------------------------------------------Execicio 6 --------------------------------------------
  
    
relatorio = do
putStr "Analisando caca palavras..."     
arquivo <- readFile "saida.txt"
putStr "\n"
writeFile "saida.txt" (arquivo ++ 
						"Conjunto de palavras:"++ show(palavras1)++ "\n" ++
						"\n" ++
						"Matriz:" ++ "n=10"++"\n"++
						"\n" ++
						(cacapalavras cpalavras1 10) ++ "\n" ++ "\n" ++ "\n" ++
						show(ocorrenciamc palavras1 cpalavras1 10 lcustos2) ++ "\n"++ "\n"++ 
						show(noocorrencias palavras1 cpalavras1 10) ++ "\n" ++ "\n"++
						show(palavrasmaiscaras palavras1 cpalavras1 10 lcustos2) ++ "\n" ++ "\n"++
						show(dadospalavra palavras1 cpalavras1 10) ++ "\n" ++ "\n"++
						show(direcao palavras1 cpalavras1 10))
putStr "Analise completa!"









