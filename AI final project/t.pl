inserty(_,_,_,Res,Tmp,4):-append(Tmp,[],Res).
% agar soton ro peyda kard ba mohtava hamon khone concat mikone va edame
% mide ta list tmp(copy) kamel beshe
inserty(List,Y,Elm,Res,Tmp,Index):-Index=Y,Index2 is Index+1,nth0(Index,List,Elm2),
string_concat(Elm2,Elm,Tmp2),append(Tmp,[Tmp2],Tmp3),inserty(List,Y,Elm,Res,Tmp3,Index2).
%agar peyda nakard mire sotone ba'di
inserty(List,Y,Elm,Res,Tmp,Index):-Index\=Y,nth0(Index,List,Elm2),
    append(Tmp,[Elm2],Tmp2),Index2 is Index+1,inserty(List,Y,Elm,Res,Tmp2,Index2).
% X
insertx(_,_,_,_,Res,Tmp,4):-append(Tmp,[],Res).
%satr ro peya kard
insertx(List,X,Y,Elm,Res,Tmp,Index):-Index=X,Index2 is Index+1,nth0(Index,List,Elm2),
    inserty(Elm2,Y,Elm,Tmp2,[],0),append(Tmp,[Tmp2],Tmp3),insertx(List,X,Y,Elm,Res,Tmp3,Index2).
insertx(List,X,Y,Elm,Res,Tmp,Index):-Index\=X,nth0(Index,List,Elm2),
    append(Tmp,[Elm2],Tmp2),Index2 is Index+1,insertx(List,X,Y,Elm,Res,Tmp2,Index2).
%------------------------------------------
insert(List,X,Y,Elm,Res):-insertx(List,X,Y,Elm,Res,[],0).
%-----------------------------------insert 2d
updatey(_,_,_,Res,Tmp,4):-append(Tmp,[],Res).
updatey(List,Y,Elm,Res,Tmp,Index):-Index=Y,Index2 is Index+1,append(Tmp,[Elm],Tmp3),updatey(List,Y,Elm,Res,Tmp3,Index2).
updatey(List,Y,Elm,Res,Tmp,Index):-Index\=Y,nth0(Index,List,Elm2),
    append(Tmp,[Elm2],Tmp2),Index2 is Index+1,updatey(List,Y,Elm,Res,Tmp2,Index2).
updatex(_,_,_,_,Res,Tmp,4):-append(Tmp,[],Res).
updatex(List,X,Y,Elm,Res,Tmp,Index):-Index=X,Index2 is Index+1,nth0(Index,List,Elm2),
    updatey(Elm2,Y,Elm,Tmp2,[],0),append(Tmp,[Tmp2],Tmp3),updatex(List,X,Y,Elm,Res,Tmp3,Index2).
updatex(List,X,Y,Elm,Res,Tmp,Index):-Index\=X,nth0(Index,List,Elm2),
    append(Tmp,[Elm2],Tmp2),Index2 is Index+1,updatex(List,X,Y,Elm,Res,Tmp2,Index2).
%-------------------------------------------------
update(List,X,Y,Elm,Res):-updatex(List,X,Y,Elm,Res,[],0).
%-----------------------------------update 2d
split(S,In,L1,L2):-string_length(S,L),Index is L-In,sub_string(S,0,Index,_,L1),I2 is L-Index,sub_string(S,Index,I2,_,L2).
%-----------------------------------split
findelm(List,X,Y,Elm):-nth0(X,List,L1),nth0(Y,L1,Elm).
%-----------------------------------find element
movestack(List,X0,Y0,X1,Y1,Res,Number):-findelm(List,X0,Y0,Elm2),
split(Elm2,Number,L1,L2),update(List,X0,Y0,L1,List3),
    insert(List3,X1,Y1,L2,Res).
%-----------------------------------move stack
% check mikone ke khone out of bound nabashe va agar khali bashe mohre
% ro mizare
checkinsert(List,X,_,_,List):-X>3,write("out of bound1"),!.
checkinsert(List,X,_,_,List):-X<0,write("out of bound1"),!.
checkinsert(List,_,Y,_,List):-Y>3,write("out of bound1"),!.
checkinsert(List,_,Y,_,List):-Y<0,write("out of bound1"),!.
checkinsert(List,X,Y,_,List):-findelm(List,X,Y,Elm2),Elm2\="",write("already full"),!.
checkinsert(List,X,Y,Elm,Res):-findelm(List,X,Y,Elm2),Elm2="",insert(List,X,Y,Elm,Res).
%-----------------------------------check insert
%check mikone hm andaze adade dade shode mitune hrkt kone
checkmove(List,X0,Y0,_,_,List,Number):-findelm(List,X0,Y0,Elm2),string_length(Elm2,Len),
    Len<Number,write("number too big "),!.
checkmove(List,_,_,X1,Y1,List,Number):-findelm(List,X1,Y1,Elm3),string_length(Elm3,Len2),
    L3 is Len2+Number,L3>8,
    write("more than 8 in a house"),!.
checkmove(List,X0,Y0,X1,Y1,Res,Number):-movestack(List,X0,Y0,X1,Y1,Res,Number).
%-----------------------------------check update
% chek mikone khonee ke azash mikhad harkat kone vojod dare ya na?
moved(List,X0,_,_,_,List):-X0>3,write("out of bound2"),!.
moved(List,X0,_,_,_,List):-X0<0,write("out of bound2"),!.
moved(List,_,Y0,_,_,List):-Y0>3,write("out of bound2"),!.
moved(List,_,Y0,_,_,List):-Y0<0,write("out of bound2"),!.
%agar vojod dare
moved(List,X0,Y0,Dir,Num,Res):-Dir="u",X1 is X0-1,X1>=0,
    checkmove(List,X0,Y0,X1,Y0,Res,Num),!.
moved(List,_,_,Dir,_,List):-Dir="u",write("out of bound3"),!.
moved(List,X0,Y0,Dir,Num,Res):-Dir="d",X1 is X0+1,X1<4,
     checkmove(List,X0,Y0,X1,Y0,Res,Num),!.
moved(List,_,_,Dir,_,List):-Dir="d",write("out of bound3"),!.
moved(List,X0,Y0,Dir,Num,Res):-Dir="l",Y1 is Y0-1,Y1>=0,
    checkmove(List,X0,Y0,X0,Y1,Res,Num),!.
moved(List,_,_,Dir,_,List):-Dir="l",write("out of bound3"),!.
moved(List,X0,Y0,Dir,Num,Res):-Dir="r",Y1 is Y0+1,Y1<4,
     checkmove(List,X0,Y0,X0,Y1,Res,Num),!.
moved(List,_,_,Dir,_,List):-Dir="r",write("out of bound3"),!.
%-----------------------------------possible move
% retractall(ii(A)).
% sakhtan safheye bazi
startgame:-List=[["","","",""],["","","",""],["","","",""],["","","",""]],
Turn is 1,W is 20,B is 20,asserta(data(List,Turn,W,B)),
    asserta(pointl([[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]])),%PPPPPPPPPPPPPPPPPPPPPPPPPPpp???????????????
    write("player1 turn").
endgame:-retractall(data(_,_,_,_)).
% gereftan moqEyat va khandan data(safhe) va checkinsert agar nashod
% nobat baghi mimanad
insertdata(X,Y,Res):-data(L,T,_,_),
   checkinsert(L,X,Y,"a",Res),Res=L,write(" player"),write(T),write(" turn").%player turn nemitavanad mohre ra qarar dahad
insertdata(X,Y,Res):-data(L,T,W,B),
    checkinsert(L,X,Y,"w",Res),Res\=L,T=1,%mitavanad qarar dahad
    checkwin("w"),
    %OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
    pointl(LL),changeval(LL,1,X,Y,RR),retractall(pointl(_)),asserta(pointl(RR)),
    whatpos(RR),makestmax(" player2 turn: ",Re),write(Re),nx,
    %OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
    %tedad mohrehaye white kam mishe va object jadid az safhe sakhte mishe
    W2 is W-1,retractall(data(_,_,_,_)),asserta(data(Res,2,W2,B)).
%black
insertdata(X,Y,Res):-data(L,T,W,B),
    checkinsert(L,X,Y,"b",Res),Res\=L,T=2,
    checkwin("b"),
    %OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
    pointl(LL),changeval(LL,-1,X,Y,RR),retractall(pointl(_)),asserta(pointl(RR)),
    whatpos(RR),makestmin(" player1 turn: ",Re),write(Re),nx,
    %OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
    B2 is B-1,retractall(data(_,_,_,_)),asserta(data(Res,1,W,B2)).
%-----------------------------------insert data
updatedata(_,_,_,Num,Res):-Num>4,write("number too big "),data(Res,_,_,_),!.
%check krdne hrkt yedunei
% moved:..................................................................
updatedata(X,Y,Dir,1,Res):-data(L,T,W,B),T=1,moved(L,X,Y,Dir,1,Res),Res\=L,checkwin("w"),checkwin("b"),
    %OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
    calc(X,Y,X1,Y1,Dir),pointl(LL),changeval(LL,-1,X,Y,LL2),
    changeval(LL2,1,X1,Y1,RR),retractall(pointl(_)),asserta(pointl(RR)),
     whatpos(RR),makestmin(" player2 turn: ",Re), write(Re),nx,
    %OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
    %avaz krdne nobt
     retractall(data(_,_,_,_)),asserta(data(Res,2,W,B)),!.
%black
updatedata(X,Y,Dir,1,Res):-data(L,T,W,B),T=2,moved(L,X,Y,Dir,1,Res),Res\=L,checkwin("w"),checkwin("b"),
    %AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    calc(X,Y,X1,Y1,Dir),pointl(LL),changeval(LL,1,X,Y,LL2),
    changeval(LL2,-1,X1,Y1,RR),retractall(pointl(_)),asserta(pointl(RR)),
     whatpos(RR),makestmax(" player1 turn: ",Re), write(Re),nx,
    %AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    retractall(data(_,_,_,_)),asserta(data(Res,1,W,B)),!.
%upd:jahate harekt marhal ghbl,tedade max jabejai,x va y mogheeyat
%az inja varede continue mishavim
updatedata(X,Y,Dir,Num,Res):-data(L,T,W,B),Num>1,moved(L,X,Y,Dir,Num,Res),Res\=L,win(Res),
    write(" player"),write(T),write(" continue?"),
    retractall(data(_,_,_,_)),asserta(data(Res,T,W,B)),asserta(upd(Dir,Num,X,Y)),!.
%-----------------------------------update data
%nobt avaz mishe
continue(Ans):-Ans="no",data(L,T,W,B),T=1,
    retractall(data(_,_,_,_)),asserta(data(L,2,W,B)),write(" player2 turn"),
    retractall(upd(_,_,_,_)).
continue(Ans):-Ans="no",data(L,T,W,B),T=2,
    retractall(data(_,_,_,_)),asserta(data(L,1,W,B)),write(" player1 turn"),
    retractall(upd(_,_,_,_)).
% check krdne khune hae stack k adad dade shode bozorgtr az khune hae
% stack nabashn
continue(Ans,Num,Res):-Ans="yes",upd(_,N,_,_),Num>=N,write("number too big "),data(Res,_,_,_),!.
continue(Ans,Num,Res):-Ans="yes",upd(Dir,N,X0,Y0),Num<N,retractall(upd(_,_,_,_)),
    calc(X0,Y0,X,Y,Dir),updatedata(X,Y,Dir,Num,Res),!.
calc(X0,Y0,X1,Y0,D):-D="u",X1 is X0-1.
calc(X0,Y0,X1,Y0,D):-D="d",X1 is X0+1.
calc(X0,Y0,X0,Y1,D):-D="l",Y1 is Y0-1.
calc(X0,Y0,X0,Y1,D):-D="r",Y1 is Y0+1.
%-----------------------------------continue update
%movedata
%gereftan rasttarin character string
getlast(St,Elm):-atom_string(Atom,St),sub_atom(Atom, _, 1, 0, E),atom_string(E,Elm).
movedata(_,_,_,Num,_):-Num>4,!.%!!!!!!!!!!!!!!!!!!!!!!write out of bound!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%findel khundn khune
%chek mikone sare stack vase ma bashe
%tu stack hm andaze num mohre dashte bashim
movedata(X,Y,_,Num,_):-data(L,T,_,_),T=1,findelm(L,X,Y,St),getlast(St,Elm),
    Elm=="w",string_length(St,Len),Len<Num,write("number too big "),!.
%agehme shorut brgharar bud update mishe safhe
% kare update
% data:..................................................................
movedata(X,Y,Dir,Num,Res):-data(L,T,_,_),T=1,findelm(L,X,Y,St),getlast(St,Elm),
    Elm=="w",updatedata(X,Y,Dir,Num,Res),!.
%halate else sare stack ham rang to nist
movedata(_,_,_,_,L):-data(L,T,_,_),T=1,write("not your block"),!.
%black
movedata(X,Y,_,Num,_):-data(L,T,_,_),T=2,findelm(L,X,Y,St),getlast(St,Elm),
    Elm=="b",string_length(St,Len),Len<Num,write("number too big "),!.

movedata(X,Y,Dir,Num,Res):-data(L,T,_,_),T=2,findelm(L,X,Y,St),getlast(St,Elm),
    Elm=="b",updatedata(X,Y,Dir,Num,Res),!.

movedata(_,_,_,_,L):-data(L,T,_,_),T=2,write("not your block"),!.
%-----------------------------------move data
st(L):-assertz(dlist(L)).
ens:-retractall(ans(_,_)),retractall(dlist(_)).
%check krdne radifi
checkrow(X,_,_):-X>=4,!.
checkrow(_,Y,_):-Y>=4,!.
checkrow(_,Y,_):-Y<0,!.
%copy list asli:dlist
%age x be 3 resid age saresh hamun chizi k mikhastim bud bordim
checkrow(X,Y,C):-X=3,dlist(L),findelm(L,X,Y,Elm),getlast(Elm,C2),C=C2,data(_,Turn,_,_),writeln("win"),writeln(Turn),endgame,abort.
%##########DEBUGGED################
checkrow(X,Y,C):-Y=3,dlist(L),findelm(L,X,Y,Item),getlast(Item,C2),C=C2,data(_,Turn,_,_),writeln("win"),writeln(Turn),abort.
checkrow(X,Y,C):-dlist(L),findelm(L,X,Y,Elm),getlast(Elm,C2),C=C2,retractall(dlist(_)),
    update(L,X,Y,"Z",R),assertz(dlist(R)),
    calc(X,Y,X1,Y1,"d"),checkrow(X1,Y1,C),
    calc(X,Y,X2,Y2,"l"),checkrow(X2,Y2,C),
    calc(X,Y,X3,Y3,"r"),checkrow(X3,Y3,C).
%vaghti sare stack char ma nabashe
checkrow(_,_,_):-!.

%check krdne sotuni
checkcol(X,_,_):-X>=4,!.
checkcol(_,Y,_):-Y>=4,!.
checkcol(_,Y,_):-Y<0,!.
checkcol(X,Y,C):-Y=3,dlist(L),findelm(L,X,Y,Elm),getlast(Elm,C2),C=C2,data(_,Turn,_,_),writeln("win"),writeln(Turn),endgame,abort.
%##########DEBUGGED################
checkcol(X,Y,C):-X=3,dlist(L),findelm(L,X,Y,Item),getlast(Item,C2),C=C2,data(_,Turn,_,_),writeln("win"),writeln(Turn),endgame,abort.

checkcol(X,Y,C):-dlist(L),findelm(L,X,Y,Elm),getlast(Elm,C2),C=C2,retractall(dlist(_)),
    update(L,X,Y,"Z",R),assertz(dlist(R)),
    calc(X,Y,X1,Y1,"d"),checkcol(X1,Y1,C),
    calc(X,Y,X2,Y2,"u"),checkcol(X2,Y2,C),
    calc(X,Y,X3,Y3,"r"),checkcol(X3,Y3,C).
checkcol(_,_,_):-!.
%age mohre kasi sefr beshe elame mosavi
tie:-data(_,_,0,_),writeln("tie"),abort.
tie:-data(_,_,_,0),writeln("tie"),abort.
tie:-!.
%seda krdne check row va check call barae hr satr va sotun
checkwin(T):-data(L,_,_,_),checkrows(0,0,T,L),checkrows(0,1,T,L),checkrows(0,2,T,L),checkrows(0,3,T,L),
    checkcols(0,0,T,L),checkcols(1,0,T,L),checkcols(2,0,T,L),checkcols(3,0,T,L),tie.
checkrows(X,Y,T,List):-st(List),checkrow(X,Y,T),ens.
checkrcols(X,Y,T,List):-st(List),checkcol(X,Y,T),ens.
win(L):-st(L),checkwin("w"),ens,st(L),checkwin("b"),ens.
% ---------------------------------------------------------------check win
% %tabe bala be khane x,y meghdare n ra ezafe mikond
insertyn(_,_,_,Res,Tmp,4):-append(Tmp,[],Res).
insertyn(List,Y,Elm,Res,Tmp,Index):-Index=Y,Index2 is Index+1,nth0(Index,List,Elm2),
Tmp2 is Elm+Elm2,append(Tmp,[Tmp2],Tmp3),insertyn(List,Y,Elm,Res,Tmp3,Index2).
insertyn(List,Y,Elm,Res,Tmp,Index):-Index\=Y,nth0(Index,List,Elm2),
    append(Tmp,[Elm2],Tmp2),Index2 is Index+1,insertyn(List,Y,Elm,Res,Tmp2,Index2).
insertxn(_,_,_,_,Res,Tmp,4):-append(Tmp,[],Res).
insertxn(List,X,Y,Elm,Res,Tmp,Index):-Index=X,Index2 is Index+1,nth0(Index,List,Elm2),
    insertyn(Elm2,Y,Elm,Tmp2,[],0),append(Tmp,[Tmp2],Tmp3),insertxn(List,X,Y,Elm,Res,Tmp3,Index2).
insertxn(List,X,Y,Elm,Res,Tmp,Index):-Index\=X,nth0(Index,List,Elm2),
    append(Tmp,[Elm2],Tmp2),Index2 is Index+1,insertxn(List,X,Y,Elm,Res,Tmp2,Index2).
insertn(List,X,Y,Elm,Res):-insertxn(List,X,Y,Elm,Res,[],0).
% mogheyate khanehae balae paeni chapi rasti va khodash ba estedafe az
% tabe insert ba n jam mishavad
changeval(L,N,X,Y,R):- calc(X,Y,X1,Y1,"u"),calc(X,Y,X2,Y2,"d"),
    calc(X,Y,X3,Y3,"r"),calc(X,Y,X4,Y4,"l"),
     chanegeck(L,X1,Y1,R1,N),chanegeck(R1,X2,Y2,R2,N),chanegeck(R2,X3,Y3,R3,N),
    chanegeck(R3,X4,Y4,R4,N),chanegeck(R4,X,Y,R,N).
chanegeck(L,X,_,L,_):-X<0.
chanegeck(L,X,_,L,_):-X>3.
chanegeck(L,_,Y,L,_):-Y>3.
chanegeck(L,_,Y,L,_):-Y<0.
chanegeck(L,X,Y,R,E):-insertn(L,X,Y,E,R).
%-----------------------------------------make and change point array
minxx():-minnp(A,B,C),retract(minnp(A,B,C)),asserta(min2(A,B,C)),minxx2(A,_).
minxx2(R0,R):-minnp(A,B,C),retract(minnp(A,B,C)),min2(L,R1,R2),
    ddmin(A,B,C,L,R1,R2,Next,R0),minxx2(Next,R).
minxx2(R0,R0):- \+ minnp(_,_,_),!.
ddmin(A,B,C,L,R1,R2,A,R0):-A<R0,retractall(min2(L,R1,R2)),asserta(min2(A,B,C)).
ddmin(A,B,C,_,_,_,A,R0):-A=R0,asserta(min2(A,B,C)).
ddmin(A,_,_,_,_,_,R0,R0):-A>R0.
%----- min
maxxx():-minnyp(A,B,C),retract(minnyp(A,B,C)),asserta(max2(A,B,C)),maxxx2(A,_).
maxxx2(R0,R):-minnyp(A,B,C),retract(minnyp(A,B,C)),max2(L,R1,R2),
    ddmax(A,B,C,L,R1,R2,Next,R0),maxxx2(Next,R).
maxxx2(R0,R0):- \+ minnyp(_,_,_),!.
ddmax(A,B,C,L,R1,R2,A,R0):-A>R0,retractall(max2(L,R1,R2)),asserta(max2(A,B,C)).
ddmax(A,B,C,_,_,_,A,R0):-A=R0,asserta(max2(A,B,C)).
ddmax(A,_,_,_,_,_,R0,R0):-A<R0.
%----- max
whatpos(L):-findelm(L,0,0,E),asserta(minnp(E,0,0)),asserta(minnyp(E,0,0)),
    findelm(L,0,1,E0),asserta(minnp(E0,0,1)),asserta(minnyp(E0,0,1)),
    findelm(L,0,2,E1),asserta(minnp(E1,0,2)),asserta(minnyp(E1,0,2)),
    findelm(L,0,3,E2),asserta(minnp(E2,0,3)),asserta(minnyp(E2,0,3)),
    findelm(L,1,0,E3),asserta(minnp(E3,1,0)),asserta(minnyp(E3,1,0)),
    findelm(L,1,1,E4),asserta(minnp(E4,1,1)),asserta(minnyp(E4,1,1)),
    findelm(L,1,2,E5),asserta(minnp(E5,1,2)),asserta(minnyp(E5,1,2)),
    findelm(L,1,3,E6),asserta(minnp(E6,1,3)),asserta(minnyp(E6,1,3)),
    findelm(L,2,0,E7),asserta(minnp(E7,2,0)),asserta(minnyp(E7,2,0)),
    findelm(L,2,1,E8),asserta(minnp(E8,2,1)),asserta(minnyp(E8,2,1)),
    findelm(L,2,2,E9),asserta(minnp(E9,2,2)),asserta(minnyp(E9,2,2)),
    findelm(L,2,3,E10),asserta(minnp(E10,2,3)),asserta(minnyp(E10,2,3)),
    findelm(L,3,0,E11),asserta(minnp(E11,3,0)),asserta(minnyp(E11,3,0)),
    findelm(L,3,1,E12),asserta(minnp(E12,3,1)),asserta(minnyp(E12,3,1)),
    findelm(L,3,2,E13),asserta(minnp(E13,3,2)),asserta(minnyp(E13,3,2)),
    findelm(L,3,3,E14),asserta(minnp(E14,3,3)),asserta(minnyp(E14,3,3)),minxx(),maxxx(),!.
nx:-retractall(minnypt(_,_,_)),retractall(minnpt(_,_,_)),
    retractall(minnyp(_,_,_)),retractall(minnp(_,_,_)),
    retractall(min2(_,_,_)),retractall(max2(_,_,_)).
makestmin(Tmp,Res):-min2(A,B,C),retract(min2(A,B,C)),string_concat(Tmp,B,T2),
   string_concat(T2,",",T3),string_concat(T3,C,T4),string_concat(T4,"/",T5),
    makestmin(T5,Res).
makestmin(Tmp,Tmp):-!.
makestmax(Tmp,Res):-max2(A,B,C),retract(max2(A,B,C)),string_concat(Tmp,B,T2),
   string_concat(T2,",",T3),string_concat(T3,C,T4),string_concat(T4,"/",T5),
    makestmax(T5,Res).
makestmax(Tmp,Tmp):-!.




































































































