%trace

domains
    nume=string
    nrTelefon=string
    numeUtilizator=string
    parola=string
    data=integer
    ora=integer
    text=string
    titlu=string
  
database-agenda
    agenda(nume,parola,numeUtilizator,nrTelefon)
  
database-calendar
    eveniment(nume,titlu,data,data,data,ora,ora,text)
 

predicates

    run
    meniu1
    meniu2(nume)
    citparola(string,string)
    repeatMeniu2(Nume)
    citint(integer,real,real)
    optiune(integer) %fereastra principala
    opt(integer,nume) %fereastra secundara
    addcont
    addevent(nume)
    validareAgenda(nume,parola,numeUtilizator,nrTelefon)
    logare(nume,parola)
    citzi(integer)
    citluna(integer)
    citan(integer)
    citora(integer)
    citminut(integer)
    cautaredupanume(nume,titlu)
    cautaredupadata(nume,data,data,data)
    dateeveniment(titlu,data,data,data,ora,ora)
    modificatitlu(nume,titlu,titlu,data,data,data,ora,ora)
    modificadata(nume,titlu,data,data,data,data,data,data,ora,ora)
    modificaora(nume,titlu,data,data,data,ora,ora,ora,ora)
    stergeev(nume,titlu,data,data,data,ora,ora)
    afisev(nume)
    cautarepersoana(Nume,NumeUtilizator)
    stergerelista()
  
clauses

run:-
    clearwindow,
    makewindow(1,9,9,"**Meniu Principal**",4,35,9,40),
	meniu1.
run:-
    cursor(6,2),
	write("Optiune eronata!"),
	run.
	
repeatMeniu2(Nume):-
    meniu2(Nume).
 
citparola(Start,Parola):-
    readchar(Caracter),
    Caracter<>13,
    Caracter<>32,
    Caracter<>27,
    Caracter<>8,
    %in C copiez fiecare caractercitit de la tastatura
    str_char(C,Caracter),
    %In Start voi concatena C si Conc
    concat(Start,C,Conc),
    %Voi adauga cate o steluta dupa fiecare caracter scris
    write("*"),
    citparola(Conc,Parola).
    citparola(Start,Parola):-
    %Parola devine cate un caracter citit de la tastatura
    %in final devenind ce vom scrie
    Parola=Start.
 
citzi(Zi):-
    cursor(Lin,Col),
    write(""),
    cursor(Lin,Col),
    readint(Zi),
    %Verifica daca ziua este declarata corecta
    Zi>0,
    Zi<32.
    citzi(Zi):-
    %daca introduc o data gresita imi va aparea mesajul de 
    %mai jos pe urmatorul rand si voi putea sa citesc 
    %pana aceasta va fi corecta
    cursor(Lin,_),
    write("Ziua este intre 1 si 31!!"),
    Lx=Lin-1,
    cursor(Lx,0),
    citzi(Zi).
 
citluna(Luna):-
    cursor(Lin,Col),
    write(""),
    cursor(Lin,Col),
    readint(Luna),
    %Verific corectitudinea lunii
    Luna>0,
    Luna<13.
    citluna(Luna):-
    cursor(Lin,_),
    %La fel ca la data
    write("Luna este intre 0 si 12!!"),
    Lx=Lin-1,
    cursor(Lx,0),
    citluna(Luna).
  
citan(An):-
    cursor(Lin,Col),
    write("  "),
    cursor(Lin,Col),
    readint(An),
    %Verific daca am introdus un an valid
    %anul trebuie sa fie mai mare de 2000
    An>0,
    Aux=An/1000,Aux>1.
citan(An):- 
    cursor(Lin,_),
    write("Anul trebuie sa fie mai mare de 2000!"),
    Lx=Lin-1,
    cursor(Lx,0),
    citan(An).
  
citora(Ora):-
    cursor(Lin,Col),
    write(" "),
    cursor(Lin,Col),
    readint(Ora),
    %Verific corectitudinea orei
    Ora>-1,
    Ora<25.
citora(Ora):-
    cursor(Lin,_),
    write("Ora incorecta!"),
    Lx=Lin-1,
    cursor(Lx,0),
    citora(Ora).  
 
citminut(Min):-
    cursor(Lin,Col),
    write(" "),
    cursor(Lin,Col),
    readint(Min),
    %Verific minutul sa fie corect
    Min>-1,
    Min<60.
    citminut(Min):-
    cursor(Lin,_),
    write("Minut incorect!!"),
    Lx=Lin-1,
    cursor(Lx,0),
    citminut(Min). 
 
   %Functai de logare imi cauta printre agendele existente logandu-am cu numele agendei si parola 
logare(Nume,Parola):-
    %folosesc retract pentru a curata data-base-ul agenda
    retractall(agenda(_,_,_,_)),
    %Cu ajutorul predicatului consult imi deschid
    %fisierul text unde am salvate date de logare
        %ale unui utilizatori
    consult("Log1.txt",agenda),
    %verific daca utilizatorul exista
    agenda(Nume,Parola,NumeUtilizator,NrTelefon),
    removewindow,
    %elimin fereastar curenta
    makewindow(1,5,5,Nume,5,17,16,40),
    %imi creez o noua fereastra
    meniu2(Nume).
    %in caz ca logarea va esua 
    logare(_,_):-
    makewindow(1,4,4,"Eroare  la logare",5,15,4,55),
    write("Autentificare esuata!! Verifica userul sau parola!"),
    readchar(_),
    removewindow,
    clearwindow,
    meniu1.
  
%-verific daca agenda exista
validareAgenda(Nume,_,_,_):-
    %Validarea user-ului in caz ca vrem sa creem un user care
    %deja are numele selectat
    retractall(agenda(_,_,_,_)),
    consult("Log1.txt",agenda),
    agenda(Nume,_,_,_),
    makewindow(1,4,4,"Eroare",5,15,4,55),
    write("Contul exista deja!"),
    readchar(_),
    removewindow,
    clearwindow,
    meniu1.
    validareAgenda(Nume,Pas,NumeUtilizator,NrTelefon):-
    %In caz de reusita salvez datele in memorie iar apoi
    %le adug in fisierul meu
    assert(agenda(Nume,Pas,NumeUtilizator,NrTelefon)),
    save("Log1.txt",agenda),
    makewindow(1,3,3,"Succes ",7,15,4,55),
    write("Agenda creat cu succes!"),
    readchar(_),
    removewindow,
    clearwindow,
    meniu1.
  
% creez agenda noua  
addcont:-
    makewindow(1,6,6,"Creare cont",6,40,9,40),
    write("Introdu nume agenda:"),readln(Nume),
    write("Parola: "),citparola("",Pas),nl,
    write("Nume utilizator: "),readln(NumeUtilizator),nl,
    write("Nr de telefon:"), readln(NrTelefon),
    removewindow,
    validareAgenda(Nume,Pas,NumeUtilizator,NrTelefon),
    clearwindow.
  
addevent(Nume):-
    write("Titlu eveniment:"),readln(Titlu),
    write("Data evenimentului"),nl,
    write("Ziua:"),nl,citzi(Zi),
    write("Luna:"),nl,citluna(Luna),
    write("An:"),nl,citan(An),
    write("Momentul din zi"),nl,
    write("Ora:"),nl,citora(Ora),
    write("Minutul:"),nl,citminut(Min),
    write("Descriere:"),nl,
    readln(Text),
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    %Creez fisierul unde imi voi retine evenimentele create
    consult("Eve.txt",calendar),
    %voi retine in memorie evenimentul creat
    assert(eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,Text)),
    %Voi salva evenimentele in fisierul text
    save("Eve.txt",calendar),
    makewindow(1,3,3,"Succes",9,12,3,50),
    write("Eveniment adaugat cu succes!"),
    readchar(_),
    removewindow.

% Citesc optiunea de la tastaura
citint(Int,Lin,Col):-
    write(""),
    cursor(Lin,Col),
    readint(Int).
citint(Int,Lin,Col):-
    cursor(Lin,Col),
    write(""),
    Lx=Lin+1,
    cursor(Lx,0),
    write("Numar incorect!"),
    citint(Int,Lin,Col).
 
cautaredupanume(Nume,Titlu):-
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    consult("Eve.txt",calendar),
    eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,Text),
    write("In data de: ",Zi,".",Luna,".",An),nl,
    write(Titlu,"Ora:",Ora,":",Min),nl,
    write(Text),nl,
    write("------------------------------"),nl,
    readchar(_),
    %Faild, forteaza esecul, si intra pe false
    fail.
    cautaredupanume(_,_):-
    write("Acestea sunt toate."),nl,
    write("------------------------------"),
    readchar(_).

cautaredupadata(Nume,Zi,Luna,An):-
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    consult("Eve.txt",calendar),
    eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,Text),
    write("In data de: ",Zi,".",Luna,".",An),nl,
    write(Titlu,"Ora:",Ora,":",Min),nl,
    write(Text),nl,
    write("------------------------------"),nl,
    readchar(_),
    fail.
cautaredupadata(_,_,_,_):-
    write("Acestea sunt toate."),nl,
    write("------------------------------"),nl,
    readchar(_).
 
dateeveniment(Titlu,Zi,Luna,An,Ora,Min):-
    write("Introdu datele pentru\nevenimentul care vrei sa-l\nmodifici:"),nl,
    write("Titlul:"),readln(Titlu),
    write("---Data---"),nl,
    write("Ziua:"),nl,citzi(Zi),
    write("Luna:"),nl,citluna(Luna),
    write("Anul:"),nl,citan(An),
    write("Ora:"),nl,citora(Ora),
    write("Minutul:"),nl,citminut(Min). 
 
modificatitlu(Nume,Titlu,Titlu2,Zi,Luna,An,Ora,Min):-
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    consult("Eve.txt",calendar),
    eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,Text),
    write("Eveniment modificat"),nl,
    retract(eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,_)),
    assert(eveniment(Nume,Titlu2,Zi,Luna,An,Ora,Min,Text)),
    save("Eve.txt",calendar),
    fail.
    modificatitlu(_,_,_,_,_,_,_,_):-
    write("------------------------------"),nl,
    readchar(_).
 
modificadata(Nume,Titlu,Zi,Luna,An,ZiN,LunaN,AnN,Ora,Min):-
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    consult("Eve.txt",calendar),
    eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,Text),
    write("Eveniment modificat"),nl,
    %ma folosesc de retract pentru a imi sterge
    %datele curente 
    retract(eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,_)),
    %salvez datele modificate in memorie
    assert(eveniment(Nume,Titlu,ZiN,LunaN,AnN,Ora,Min,Text)),
    %salvezi noul eveniment in fisier
    save("Eve.txt",calendar),
    fail.
    
modificadata(_,_,_,_,_,_,_,_,_,_):-
    write("------------------------------"),nl,
    readchar(_).
      
modificaora(Nume,Titlu,Zi,Luna,An,Ora,Min,OraN,MinN):-
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    consult("Eve.txt",calendar),
    eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,Text),
    write("Eveniment modificat"),nl,
    retract(eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,_)),
    assert(eveniment(Nume,Titlu,Zi,Luna,An,OraN,MinN,Text)),
    save("Eve.txt",calendar),
    fail.

modificaora(_,_,_,_,_,_,_,_,_):-
    write("------------------------------"),nl,
    readchar(_).
 
stergeev(Nume,Titlu,Zi,Luna,An,Ora,Min):-
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    consult("Eve.txt",calendar),
    eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,_),
    write("Eveniment sters"),nl,
    retract(eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,_)),
    save("Eve.txt",calendar),
    fail.
stergeev(_,_,_,_,_,_,_):-
    write("------------------------------"),
    readchar(_).
    %Afisez evenimentele mele

afisev(Nume):-
    retractall(eveniment(_,_,_,_,_,_,_,_)),
    consult("Eve.txt",calendar),
    eveniment(Nume,Titlu,Zi,Luna,An,Ora,Min,Text),
    write("Titlul: ",Titlu,"  ora",Ora,":",Min),nl,
    write(Zi,"-",Luna,"-",An),nl,
    write(Text),nl,
    write("------------------------------"),
    readchar(A),
    A='s',
    true.
    
afisev(_):-
    write("------------------------------"),
    readchar(_).
 
stergerelista():-
    deletefile("Eve.txt"),
    consult("Eve.txt",calendar),
    save("Eve.txt",calendar). 

%     
optiune(1):-
    makewindow(1,3,3,"Logare",6,40,9,30),
    write("Nume agenda  :"),readln(Nume),
    write("Parola:"),citparola("",Parola),nl,
    removewindow,
    logare(Nume,Parola). 
    
optiune(2):-
    addcont.
    
optiune(3):-
    cautarepersoana(Nume,Parola).
    
optiune(4):-
    system(exit).
    
optiune(_):-
    cursor(Lin,Col),
    write("Index inexistent!"),
    Lx=Lin-1,
    cursor(Lx,Col),
    citint(A,Lx,Col),
    optiune(A). 
 
 % adaug eveniment
opt(1,Nume):-
    makewindow(1,9,9,"Adauga eveniment",7,13,15,55),
    addevent(Nume),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).
    
% caut eveniment dupa titlu   
opt(2,Nume):-
    makewindow(1,9,9,"Cautare...",4,35,19,32),
    write("Titlul evenimentului:"),
    readln(Titlu),
    write("------------------------------"),nl,
    cautaredupanume(Nume,Titlu),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).
    
% caut eveniment dupa data
opt(3,Nume):-
    makewindow(1,9,9,"Cautare...",4,35,19,32),
    write("Data evenimentului"),nl,
    write("Ziua:"),nl,citzi(Zi),
    write("Luna:"),nl,citluna(Luna),
    write("Anul:"),nl,citan(An),
    write("------------------------------"),
    cautaredupadata(Nume,Zi,Luna,An),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).
    
% modific titlu eveniment
opt(4,Nume):-
    makewindow(1,9,9,"Actualizare",4,35,19,32),
    dateeveniment(Titlu,Zi,Luna,An,Ora,Min),
    write("Introdu noul titlu:"),readln(Titlu2),
    write("------------------------------"),
    modificatitlu(Nume,Titlu,Titlu2,Zi,Luna,An,Ora,Min),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).
    
% modific data eveniment
opt(5,Nume):-
    makewindow(1,9,9,"Actualizare",4,35,19,32),
    dateeveniment(Titlu,Zi,Luna,An,Ora,Min),
    write("Introdu noua data:"),nl,
    write("Ziua:"),nl,citzi(Zin),
    write("Luna:"),nl,citluna(Lunan),
    write("Anul:"),nl,citan(Ann),
    write("------------------------------"),
    modificadata(Nume,Titlu,Zi,Luna,An,Zin,Lunan,Ann,Ora,Min),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).

% modific ora eveniment    
opt(6,Nume):-
    makewindow(1,9,9,"Actualizare",4,35,19,32),
    dateeveniment(Titlu,Zi,Luna,An,Ora,Min),
    write("Introdu noua ora:"),nl,
    write("Ora:"),nl,citora(OraN),
    write("Minutul"),nl,citminut(MinN),
    write("------------------------------"),
    modificaora(Nume,Titlu,Zi,Luna,An,Ora,Min,OraN,MinN),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).

% sterg eveniment    
opt(7,Nume):-
    makewindow(1,9,9,"Stergere eveniment",4,35,19,32),
    dateeveniment(Titlu,Zi,Luna,An,Ora,Min),
    write("------------------------------"),
    stergeev(Nume,Titlu,Zi,Luna,An,Ora,Min),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).
    
%-vizualizare evenimente proprii    
opt(8,Nume):-
    makewindow(1,9,9,"Evenimente",4,35,19,32),
    write("Evenimentele tale:"),nl,
    write("Apasa 's' pentru a te intoarce"),nl,
    write("------------------------------"),
    afisev(Nume),
    removewindow,
    clearwindow,
    repeatMeniu2(Nume).
    
% inchidere fereastra curenta
opt(9,_):-
    removewindow,
    makewindow(1,9,9,"**Login**",6,20,9,40),
    meniu1.
    
opt(10,Nume):-
    removewindow,
    stergerelista(),
    afisev(Nume),
    removewindow,
    clearwindow.
    %repeatMeniu2(Nume).

opt(11,_):-
    system(exit).
    opt(_,Nume):-
    cursor(Lin,Col),
    write("Index inexistent! "),
    Lx=Lin-2,
    cursor(Lx,Col),
    citint(A,Lx,Col),
    opt(A,Nume).
 
 % caut numarul de telefon al unei persoane in agenda dupa nu numele sau
cautarepersoana(Nume,NumeUtilizator):-
    retractall(agenda(_,_,_,_)),
    consult("Log1.txt",agenda),
    makewindow(1,9,9,"**Detalii**",8,15,15,50),
    write("Agenda: "),
    readln(Nume),nl,
    write("NumeUtilizator:"),
    readln(NumeUtilizator),nl,
    consult("Log1.txt",agenda),
	agenda(Nume,Parola,NumeUtilizator,Telefon),
	write("Telefon ", Telefon),nl,
    readchar(_),
    removewindow,
    clearwindow,
  	meniu1.
  
  
meniu2(Nume):-
    write("1.Adauga eveniment"),nl,
    write("2.Cauta eveniment dupa titlu"),nl,
    write("3.Cauta evenimente din data"),nl,
    write("4.Modifica titlu eveniment"),nl,
    write("5.Modifica data eveniment"),nl,
    write("6.Modifica ora eveniment"),nl,
    write("7.Sterge eveniment"),nl,
    write("8.Evenimentele tale"),nl,
    write("9.Back"),nl,
    %write("10.Stergere intreaga tabela"),nl,
    write("11.Exit"),nl,nl,
    write("Introdu optiune: "),nl,
    cursor(Lin,Col),
    citint(Ras,Lin,Col),
    opt(Ras,Nume).
meniu1:-
    write("1.Deschide Agenda"),nl,
    write("2.Creeare Agenda"),nl,
    write("3.Vezi detalii utilizator"),nl,
    write("4.Exit"),nl,nl,
    write("Introdu optiune: "),nl,
    cursor(Lin,Col),
    citint(Ras,Lin,Col),
    optiune(Ras). 
 
  
goal
    run