(* ::Package:: *)

Clear[updateSubsets]
updateSubsets[subsets_,vec_]:=Join[subsets,BitXor[#,vec]&/@subsets]


BeginPackage["WSLS`"]


weightSpectrumLinearSubspace0::usage="\:0412\:044b\:0447\:0438\:0441\:043b\:044f\:0435\:0442 \:0441\:043f\:0435\:043a\:0442\:0440 \:043b\:0438\:043d\:0435\:0439\:043d\:043e\:0433\:043e \:043f\:043e\:0434\:043f\:0440\:043e\:0441\:0442\:0440\:0430\:043d\:0441\:0442\:0432\:0430, \:0433\:0434\:0435 \:0432\:0435\:0441 \:0432\:0435\:043a\:0442\:043e\:0440\:0430 \:0440\:0430\:0432\:0435\:043d \:043a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:0443 \:043d\:0443\:043b\:0435\:0439 \:0432 \:043d\:0435\:043c. \:041d\:0430 \:0432\:0445\:043e\:0434 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:043f\:043e\:0441\:0442\:0443\:043f\:0430\:0435\:0442 \:043f\:043e\:0440\:0430\:0436\:0434\:0430\:044e\:0449\:0435\:0435 \:043c\:043d\:043e\:0436\:0435\:0441\:0442\:0432\:043e.";


weightSpectrumLinearSubspace1::usage="\:0412\:044b\:0447\:0438\:0441\:043b\:044f\:0435\:0442 \:0441\:043f\:0435\:043a\:0442\:0440 \:043b\:0438\:043d\:0435\:0439\:043d\:043e\:0433\:043e \:043f\:043e\:0434\:043f\:0440\:043e\:0441\:0442\:0440\:0430\:043d\:0441\:0442\:0432\:0430, \:0433\:0434\:0435 \:0432\:0435\:0441 \:0432\:0435\:043a\:0442\:043e\:0440\:0430 \:0440\:0430\:0432\:0435\:043d \:043a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:0443 \:0435\:0434\:0438\:043d\:0438\:0446 \:0432 \:043d\:0435\:043c. \:041d\:0430 \:0432\:0445\:043e\:0434 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:043f\:043e\:0441\:0442\:0443\:043f\:0430\:0435\:0442 \:043f\:043e\:0440\:0430\:0436\:0434\:0430\:044e\:0449\:0435\:0435 \:043c\:043d\:043e\:0436\:0435\:0441\:0442\:0432\:043e.";


calculate::usage="\:041f\:0435\:0440\:0432\:044b\:0439 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442 - \:043f\:0443\:0442\:044c \:043a \:0438\:0441\:0445\:043e\:0434\:043d\:043e\:043c\:0443 \:0444\:0430\:0439\:043b\:0443, \:0432\:0442\:043e\:0440\:043e\:0439 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442 - \:043f\:0443\:0442\:044c \:043a \:0444\:0430\:0439\:043b\:0443, \:043a\:0443\:0434\:0430 \:043d\:0443\:0436\:043d\:043e \:0437\:0430\:043f\:0438\:0441\:0430\:0442\:044c \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442, \:0442\:0440\:0435\:0442\:0438\:0439 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442 - \:0444\:0443\:043d\:043a\:0446\:0438\:044f weightSpectrumLinearSubspace0 \:0438\:043b\:0438 weightSpectrumLinearSubspace1";


calculate;
weightSpectrumLinearSubspace0;
weightSpectrumLinearSubspace1;


Begin["`Private`"]


Clear[updateSubsets]
updateSubsets[subsets_,vec_]:=Join[subsets,BitXor[#,vec]&/@subsets]


weightSpectrumLinearSubspace1[basis_]:=
Module[
{n=Length@First@basis,spectrum},
MemoryConstrained[
spectrum=ConstantArray[0,n+1];
Map[spectrum[[#+1]]+=1&,Total[Fold[updateSubsets[#1,#2]&,{ConstantArray[0,n]},basis],{2}]];
Transpose[{Range[0,n],spectrum}],
IntegerPart[0.85*MemoryAvailable[]]
]]


weightSpectrumLinearSubspace0[basis_]:=
Module[
{n=Length@First@basis,spectrum},
MemoryConstrained[
spectrum=ConstantArray[0,n+1];
Map[spectrum[[#+1]]+=1&,n-Total[Fold[updateSubsets[#1,#2]&,{ConstantArray[0,n]},basis],{2}]];
Transpose[{Range[0,n],spectrum}],
IntegerPart[0.85*MemoryAvailable[]]
]]


calculate[pathIn_,pathOut_,func_]:=
Export[
pathOut,
Map[ToString@#[[1]]<>"\t"<>ToString@#[[2]]&,func[ToExpression@DeleteCases[Split[Characters@Import[pathIn],#!="\n"&],"\n",2]]] 
]


End[]


EndPackage[]
