(* ::Package:: *)

(* ::Title:: *)
(*Baseball-Reference Player Graph*)


BeginPackage["BRPlayerGraph`"]


BRPlayerGraph`TeamList;
BRPlayerGraph`TeamYears;
BRPlayerGraph`TeamYearPlayers;
BRPlayerGraph`TeamYearPlayersGraph;
BRPlayerGraph`WriteStory;
BRPlayerGraph`RandomStory;


Begin["`Private`"];


(* ::Section:: *)
(*Teams*)


TeamList[]:=
With[{teamlistxml=Import["https://www.baseball-reference.com/teams/","XMLObject"],teamstr=_String?(StringMatchQ[#1,"/teams/*/"]&)},
	DeleteDuplicates[Cases[teamlistxml,XMLElement["a",{___,"href"->ts:teamstr,___},{name_}]:><|"Path"->ts,"Name"->name|>,{1,Infinity}]]
]


(* ::Section:: *)
(*Years for each team*)


$teamyearpattern=(_String?(StringMatchQ[#,"/teams/"~~(WordCharacter..)~~"/"~~(DigitCharacter..)~~".*"]&));


TeamYears[team_Association]:=TeamYears[team["Path"]];


TeamYears[path_String]:=With[{xml=Import["https://www.baseball-reference.com"<>path,"XMLObject"]},
With[{pat=$teamyearpattern},DeleteDuplicates[Cases[xml,XMLElement["a",{___,"href"->ts:pat,___},{year_?(StringMatchQ[#,DigitCharacter..]&)}]:><|"Path"->ts,"Year"->ToExpression@year|>,{1,Infinity}]]
]
]


(* ::Section:: *)
(*Players for year/team*)


(* ::Subsection:: *)
(*Create a function to get a list of players for a team/year*)


$playerpattern=_String?(StringMatchQ[#,"/players/"~~WordCharacter~~"/*"]&)


TeamYearPlayers[year_Association]:=TeamYearPlayers[year["Path"]]


TeamYearPlayers[path_String]:=With[{xml=Import["https://www.baseball-reference.com"<>path,"XMLObject"]},
With[{body=Cases[xml,XMLElement["body",_,_?(!FreeQ[#,"Full-Season Roster & Games by Position"]&)],{1,Infinity}]},
DeleteDuplicates[Cases[body,XMLElement["a",{___,"href"->ts:$playerpattern,___},{val_}]:><|"Path"->ts,"Player"->val|>,{1,Infinity}]]
]
]


creategraphdata[teamyear_,players_]:=With[{ids=StringDelete[Lookup[players,"Path"],{"/players/"~~WordCharacter~~"/",".shtml"}],labels=Lookup[players,"Player"]},
With[{edges=UndirectedEdge[##,teamyear]&@@@Subsets[ids,{2}]},
{ids,edges,{MapThread[#1->{"Player"->#2}&,{ids,labels}]}}
]
]


(* ::Section:: *)
(*Subgraph*)


TeamYearPlayersGraph[teamyeardata_,y1_,y2_]:=With[{gdata={#Year,TeamYearPlayers[#Path]}&/@Flatten@Select[With[{t=#Name},
Association/@Map[{"Path"->#Path,"Year"->Hyperlink[ToString[#Year]<>" "<>t,"https://www.baseball-reference.com"<>#Path]}&,
Cases[#Years,KeyValuePattern[{"Year"->Alternatives@@Range[y1,y2]}]]]]&/@teamyeardata,ListQ]},
	Graph[#1,#2,AnnotationRules->#3]&@@(DeleteDuplicates[Flatten[#]]&/@Transpose[creategraphdata[##]&@@@gdata])
]


(* ::Section:: *)
(*Stories*)


WriteStory[g_,{v1_,v2_}]:=WriteStory[g,v1,v2]
WriteStory[g_,v1_,v2_]:=
Block[{path=FindShortestPath[g,v1,v2], names, teams},
names=AnnotationValue[{g,path},"Player"];
names=MapThread[Hyperlink[#1,URLBuild[{"https://www.baseball-reference.com/players/",StringTake[#2,1],#2<>".shtml"}]]&,{names,path}];
teams=First/@MovingMap[EdgeTags[g,#]&,path,1];
Column[{Style[Row[{"Connecting ",names[[1]]," and ",names[[-1]]}],20],
Row[ReplacePart[Riffle[Riffle[names,teams],{" who played on the "," with "}],2->" played on the "]]}]
]
RandomStory[g_]:=WriteStory[g,RandomSample[VertexList[g],2]]


End[]


EndPackage[]
