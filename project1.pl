removeFromList(E,[E|T],T).
removeFromList(E,[H|T],[H|Z]):- E\= H,removeFromList(E,T,Z). 

free_schedule(_,[],[]).

free_schedule(AllTas, [day(Day,BusyTaList)|T] ,[day(Day,FreeTaList)|T2]):-
    getFreeTaList(AllTas,Day,BusyTaList,FreeTaList),
	free_schedule(AllTas,T,T2).

getFreeTaList(_,_,[],[]).	
getFreeTaList(AllTas,Day,[Slot|Slots],[NewSlot|NewSlots]):-
    permutation(AllTas,DiffOrderTas),
    getFreeTasForSlot(DiffOrderTas,Day,Slot,NewSlot),
	getFreeTaList(AllTas,Day,Slots,NewSlots).

getFreeTasForSlot([],_,_,[]).
getFreeTasForSlot([ta(Name,DayOff)|Tas],Today,Slot,[Name|OtherTas]):-
    \+member(Name,Slot),
	DayOff \= Today,
	getFreeTasForSlot(Tas,Today,Slot,OtherTas).
getFreeTasForSlot([ta(Name,DayOff)|Tas],Today,Slot,OtherTas):-
    ((member(Name,Slot)); (DayOff = Today)),
	getFreeTasForSlot(Tas,Today,Slot,OtherTas).
     
     
assign_quiz(quiz(_,Day,SlotNo,TAsNeeded),FreeSched,AssignedTas):-
    slotFreeTAs(Day,SlotNo,FreeSched,FreeTAs),
	combination(TAsNeeded,FreeTAs,Comb),
	permutation(Comb,AssignedTas).
	
slotFreeTAs(Day,SlotNo,[day(Day,SlotList)|_],FreeTAs):-
    getSlot(SlotNo,SlotList,FreeTAs).
slotFreeTAs(Day,SlotNo,[day(AnotherDay,_)|RestOfWeek],FreeTAs):-
    Day \=AnotherDay,
	slotFreeTAs(Day,SlotNo,RestOfWeek,FreeTAs).

getSlot(1,[Slot|_],Slot).
getSlot(Number,[_|T],FreeTAs):- Number>1,NewNumber is Number-1, getSlot(NewNumber,T,FreeTAs).

combination(0,_,[]).
combination(K,L,[X|Xs]) :- K > 0,
   el(X,L,R), K1 is K-1, combination(K1,R,Xs).
   
el(X,[X|L],L).
el(X,[_|L],R) :- el(X,L,R).


assign_quizzes([],_,[]).
assign_quizzes([Quiz|Quizzes],FreeSchedule,[proctors(Quiz,AssignedTas)|Rest]):-
   assign_quiz(Quiz,FreeSchedule,AssignedTas),
   Quiz = quiz(_,Day,SlotNo,_),
   markNotFree(AssignedTas,FreeSchedule,Day,SlotNo,NewFreeSched),
   assign_quizzes(Quizzes,NewFreeSched,Rest).
   
markNotFree([],X,_,_,X).
markNotFree([TA|TAs],FreeSchedule,Day,SlotNo,NewFreeSched):-
   modifyWeek(TA,FreeSchedule,Day,SlotNo,ModifiedFreeSched),
   markNotFree(TAs,ModifiedFreeSched,Day,SlotNo,NewFreeSched).

modifyWeek(TA,[day(Day,L)|T],Day,SlotNo,[day(Day,ModifiedDay)|T]):-
   modifyDay(TA,SlotNo,L,ModifiedDay).
modifyWeek(TA,[day(AnotherDay,L)|Days],Day,SlotNo,[day(AnotherDay,L)|ModifiedDays]):-
  Day\=AnotherDay,
  modifyWeek(TA,Days,Day,SlotNo,ModifiedDays).

 modifyDay(TA,1,[H|T],[NewH|T]):- removeFromList(TA,H,NewH).
 modifyDay(TA,N,[H|T],[H|Modified]):- N>1,NewN is N-1, modifyDay(TA,NewN,T,Modified).   
	  

assign_proctors(AllTas,Quizzes,TeachingSchedule,ProctorSchedule):-
    free_schedule(AllTas,TeachingSchedule,FreeSchedule),
	assign_quizzes(Quizzes,FreeSchedule,ProctorSchedule).
	  
	  
	  
	  