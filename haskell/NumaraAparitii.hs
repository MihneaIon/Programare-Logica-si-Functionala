import System.Console.ANSI
import System.IO
import System.Exit
import System.Process
import System.Directory (doesFileExist, doesDirectoryExist)

--Eliminarea spatiilor redundante, dintre 2 cuvinte
norm::[Char]->[Char]
norm []=[]
norm (' ':(' ':rl))=norm(' ':rl)
norm(' ':rl)=' ':(norm rl)
norm(car:rl)=car:(norm rl)

--eliminarea spatiilor de la finalul sirului
elsr::[Char]->[Char]
elsr []=[]
elsr (' ':[])=[]
elsr (car:rl)=car:(elsr rl)

--obtinerea primului cuvant dintr-un sir de cuvinte normalizat
getW::[Char]->[Char]
getW []=[]
getW (' ':_)=[]
getW (car:rl)=car:(getW rl)

--stergerea primului cuvant dintr-un si de caractere normalizat
delW::[Char]->[Char]
delW []=[]
delW (' ':rl)=rl
delW (_:rl)=(delW rl) --recursivitate

--inlocuieste sfarsitul de linie cu un spatiu
inlocuire::[Char]->[Char]
inlocuire []=[]
inlocuire ('\n':rl)=' ':(inlocuire rl)
inlocuire (car:rl)=car:(inlocuire rl)

-- Contorizarea aparitile unui cuvant intr-o lista de cuvinte
contor::String->[String]->Int
contor _ []=0
contor el (cuv:rl) |(el==cuv) =1+contor el rl
                 | otherwise =0+contor el rl 

-- Contorizare unui cuvant specificat intr-o lista


--Stergerea aparitiilor unui cuvant intr-o lista de cuvinte
stergeaparitie::String->[String]->[String]
stergeaparitie _ []=[]
stergeaparitie el (cuv:rs) |(el==cuv)=stergeaparitie el rs
                           |otherwise=cuv:(stergeaparitie el rs)


--retine frecventa aparitiilor fiecarui cuvant
numaraCuvinte::[String]->[(String,Int)]
numaraCuvinte []=[]
numaraCuvinte (cuv:rs)=(cuv,contor cuv(cuv:rs)):numaraCuvinte (stergeaparitie cuv (cuv:rs))

--caut un primul cuvant in sir ir dupa ce l-am gasit il voi sterge
tok::[Char]->[[Char]]-- lista de lista de Char sau [String]
tok []=[]
tok sir=(getW sir):tok(delW(sir))

-- Afisarea cuvintelotr, cu numarul lor de aparitii
disp::[(String,Int)]->IO()
disp []=putStrLn "Terminare"
disp ((cuv,frecv):rs)=do{
                putStrLn("("++cuv++","++(show frecv)++")");
                disp rs
}

-- Verific daca fisierul din care doresc sa citesc exista sau nu
check :: (FilePath -> IO Bool) -> FilePath -> IO(String)
check p s = do{
  result <- p s;
    if (result == True) then return  "De" else return "Dne"
}

-- execut optiunile pe care le primesc de la utilizator
execopt::String->String->IO()
-- Dupa ce voi selecta una din optiuni care reprezinta un string voi intoarce o monada de tip IO()
opt1::String->IO()
-- Afisare tabelara a cuvintelor cu numarul de aparitii
opt1 sir=do{
        setCursorPosition 5 30;
        clearFromCursorToScreenEnd;
        sirnou <- return(reverse(elsr(reverse(inlocuire(elsr(norm sir))))));
        frecventacuvinte<- return (reverse(elsr(reverse(elsr(norm sirnou)))));
        listaDeFrecventa<- return (numaraCuvinte(tok(frecventacuvinte)));
        putStrLn "";
        disp listaDeFrecventa;
        temp<-getLine;
        putStrLn temp;
        clearScreen;
        setCursorPosition 5 30;
             
}
-- Dupa ce voi selecta una din optiuni care reprezinta un string voi intoarce o monada de tip IO()
opt2::String->IO()
-- Caut numarul de aparitii al unui cuvant, pe care utilizatorul il introduce
opt2 sir=do{
        setCursorPosition 5 30;
        clearFromCursorToScreenEnd;
        putStr "Introduceti cuvantul pe care doriti sa il cautati: ";
        hFlush stdout;
        cuvant <- getLine;
        setCursorPosition 7 30;
        putStr "Cuvantul: "; putStr cuvant; putStr " apare de: ";
        -- in aux imi voi retine cuvantul pe care dorsc sa il caut 
        let aux = tok(norm sir) in 
                putStrLn $ show( contor cuvant aux);
        hFlush stdout;
        temp<-getLine;
        putStrLn temp;
        setCursorPosition 5 30;
        clearFromCursorToScreenEnd; 
        
}

-- Daca se selecteaza optiunea 1 se executa opt1 iar apoi se revine la meniul initial
execopt opt sir| (opt=="1")=do{
                                    opt1 sir;
                                    mainLoop sir
                              }
-- Daca se selecteaza optiunea 2 se executa opt2 iar apoi se revine la meniul initial
execopt opt sir| (opt=="2")=do {
                                    opt2 sir;
                                    mainLoop sir
                               }
-- Daca se selecteaza optiunea 3 se executa opt3 iar apoi se revine la meniul initial
execopt opt sir| (opt=="3")=do{
                                  setCursorPosition 12 30;
                                  putStr "La revedere...";
                                  hFlush stdout;
                                  setCursorPosition 12 49;
                                  exitSuccess
                           }
-- In caz ca nu se selecteaza se va afisa mesajul "Optiune eronata" iar utilizatorul poate schimba alegerea facuta
            | otherwise = do{
                                setCursorPosition 12 30;
                                putStr "Optiune eronata...";
                                hFlush stdout;
                                temp<-getLine;
                                putStrLn temp;
                            }

-- Functia meniu  in care prezint meniul initial, dupa fiecare optiune consola va fi curatata de gunoaiele existente
mainLoop::String->IO()
mainLoop sir= do{
    setCursorPosition 5 30;
    clearFromCursorToScreenEnd;
    setTitle "Aplicatie finala";
    setCursorPosition 5 30;
    putStrLn "Optiuni program...";
    setCursorPosition 6 30;
    putStrLn "1-Afisare numar de aparitii ale cuvintelor din text ";
    setCursorPosition 7 30;
    putStrLn "2-Selecteaza un cuvant pe care doresti sa il cauti ";
    setCursorPosition 8 30;
    putStrLn "3-Terminare";
    setCursorPosition 10 30;
    putStr "Optiunea Dvs.: ";
    hFlush stdout;
    setCursorPosition 10 44;  
    opt<-getLine;
    execopt opt sir;
    clearScreen;
    mainLoop sir;
}

main::IO()
main = do{
        setCursorPosition 5 30;
        clearFromCursorToScreenEnd;
        putStr "Introduceti numele fisierului pe care vreti sa il folositi: ";
        hFlush stdout;
        --Citesc fisierul de la tastatura
        numefisier<-getLine;
        rv <- check doesFileExist numefisier;
        --Verific daca fisierul exista, iar daca acesta exista voi aplica functia de meniu mainLoop
        if ( rv=="De" ) then do{
            setCursorPosition 8 30;
            sir <- readFile numefisier;
            mainLoop sir;
        }    
        -- Daca fisierul nu exista va apara mesajul File doesn't exist si se va curata consola
        else do{
            setCursorPosition 7 30;
            putStrLn "File doesn't exist";
            hFlush stdout;
            temp<-getLine;
            putStrLn temp;
            setCursorPosition 5 30;
            clearFromCursorToScreenEnd;
            main;
        }
}