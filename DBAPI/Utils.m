(* ::Package:: *)
(* :Title: Utils *)
(* :Purpose: Generic transformations between Wolfram Language constructs and external serialization standards 
	This package is used as a foundation to build other packages on top of it *)
(* :Context: DBAPI`Utils` *)
(* :Dependencies: GeneralUtilities` *)

(* :PackageVersion: 0.9.0 *)
(* :History:  Version 0.9.0 December 24 2015 -- Initial version *)
(* :MathematicaVersion: >10.2.0 *)

(* :Discussion: *)

(* :Author: Athanassios I. Hatzis *)
(* :LatestReleaseDate: 2015-12-24 *)
(* :License: GNU LGPL - GNU Lesser General Public License *)
(*
	This file is part of DBAPI application project.

    DBAPI is free software: you can redistribute it and/or modify
    it under the terms of the GNU - Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    DBAPI is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU - Lesser General Public License for more details.

    You should have received a copy of the GNU - Lesser General Public License
    along with DBAPI.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["DBAPI`Utils`", {"GeneralUtilities`"}];


(* :Code Section (Call Unprotect and ClearAll): *)
UnProtect[		
	DBexpressionToJSON,	
	
	DBjsonToRules,
	DBjsonToAssociations,
	DBjsonToDataset,
	
	DBListSetToRuleSet,
	DBListSetToAssociationSet,
	DBListSetToDataset,
					
	DBListSetToSQL92,
	DBDatasetToSQL92
]


ClearAll[
	
	DBexpressionToJSON,	
	
	DBjsonToRules,
	DBjsonToAssociations,
	DBjsonToDataset,
	
	DBListSetToRuleSet,
	DBListSetToAssociationSet,
	DBListSetToDataset,
					
	DBListSetToSQL92,
	DBDatasetToSQL92
]


(* :Usage Messages: *)

DBexpressionToJSON::usage="DBexpressionToJSON[expression, (compact)]  
	transforms any List, Association, Dataset expression to JSON string"

DBjsonToRules::usage="DBjsonToRules[JSON String] 
	transforms a JSON string to a nested Rule structure"

DBjsonToAssociations::usage="DBjsonToAssociations[JSON String] 
	transforms a JSON string to a List of Association(s)"

DBjsonToDataset::usage="DBjsonToDataset[JSON string] 
	transforms a JSON string to a Dataset structure"


DBListSetToRuleSet::usage="DBListSetToRuleSet[Header, Body] 
	converts a relational set represented with List(s) to an equivalent one represented with Rule(s)"

DBListSetToAssociationSet::usage="DBListSetToAssociationSet[Header, Body] 
	converts a relational set represented with List(s) to an equivalent one represented with Association(s)"

DBListSetToDataset::usage="DBListSetToDataset[Header, Body] 
	converts a relational set represented with List(s) to an equivalent one represented as a table Dataset with named columns"


DBDatasetToSQL92::usage="DBDatasetToSQL92[Dataset, (values)]  
	transforms Header or Body of a relation, represented as a table Dataset with named columns, to SQL-92 format, string list of keys or string list of values respectively"

DBListSetToSQL92::usage="DBListSetToSQL92[List set, (values)] 
	transforms Header or Body of a relation represented with a set of List(s), to SQL-92 format, string list of keys or string list of values respectively" 

(* :Error Messages: *)


Begin["`Private`"];



(* Predicates 
====================================================== *)
DBlistEmptyQ[lst_List]:=Length[lst]==0

DBjsonQ[str_?StringQ]:=If[
	StringMatchQ[str, ("[" ~~ __) | ("{" ~~ __)], True, 
	False]

DBnonjsonQ[str_?StringQ]:=If[
	!StringMatchQ[str, ("[" ~~ __) | ("{" ~~ __)], True, 
	False]


(* Transformation from a Table (i.e. list of Associations) to and Indexed Table (i.e. Associations of Associations) 
This is achieved by specifying a certain key as the primary key and then either promote it for an Indexed Table or demote it for a Table

DBprimaryKeyUp[key_] := Query[GroupBy[key], First /* KeyDrop][key]]
DBprimaryKeyDown[key_]:=Query[Normal /* Map [Last[#]~Prepend~(key->First[#]) &]]
*)


(* Transformation from a Relational data set represented with Nested Lists, 
e.g. SQLSelect result, import from Excel, etc, to Rule Set, Association Set, Dataset 
The Relational data set is represented with Nested Lists, the first list as the Header (column names) and the rest as the Body of the Relation *)

DBListSetToRuleSet[columnNames_, relSet_] :=  Thread[Rule[columnNames, #]] & /@ relSet

DBListSetToAssociationSet[columnNames_, relSet_] := AssociationThread[columnNames, #] & /@ relSet

DBListSetToDataset[columnNames_, relSet_] := Dataset[DBListSetToAssociationSet[columnNames, relSet]]

(* Transformations : JSON to List of Rules 
====================================================== *)
DBjsonToRules[json_?DBjsonQ]:=ImportString[json,"JSON"]

DBjsonToAssociations[json_?DBjsonQ]:=ImportString[json,"RawJSON"]

DBjsonToDataset[json_?DBjsonQ]:=Dataset[ImportString[json,"RawJSON"]]

(* JSON Transformations 
======================================================= *)

Options[DBexpressionToJSON]={compact->False};

DBexpressionToJSON[rules_List, OptionsPattern[]]:=
	If[	OptionValue[compact],
		ExportString[rules, "JSON", "Compact"->True],
		ExportString[rules, "JSON"]]

DBexpressionToJSON[assoc_Association, OptionsPattern[]]:=
	If[	OptionValue[compact],
		ExportString[assoc, "JSON", "Compact"->True],
		ExportString[assoc, "JSON"]]

DBexpressionToJSON[ds_Dataset, OptionsPattern[]]:=
	If[	OptionValue[compact],
		ExportString[Normal@ds, "JSON", "Compact"->True],
		ExportString[Normal@ds,"JSON"]]


(* Transformations : List of Atomic Values, i.e. String, Numeric, To SQL-92 format 
========================================================== *)
(* Use with SQL  
	INSERT INTO Profile (name, surname) VALUES ("Jay", "Miner"), ("Bary", "Osborn"), .... *)

Options[DBListSetToSQL92]={values->True};

DBListSetToSQL92[lst_?DBlistEmptyQ]:="()"

DBListSetToSQL92[lst_?(Composition[Not, DBlistEmptyQ]), OptionsPattern[]] := Block[
	{transFun, depth},
	
	depth = Depth[lst];
		
	(* StringQ Base Case *)
	If[OptionValue[values], 
		transFun[x_?StringQ]:=("\"" <> x <> "\", "),
		transFun[x_?StringQ]:=(x <>", ")];			
	
	(* NumericQ Base Case *)
	transFun[x_?NumericQ]:=ToString[x] <> ", ";
	
	(* ListQ Base Case - convert the [....] json array to (......) *)
	transFun[x_?ListQ]:= Block[		
		{json, endpos},		
		json= DBexpressionToJSON[x, compact->True];
		endpos = StringLength[json];
		If[	depth==3,						
			StringReplacePart[json, {"(",  ")"}, {{1, 1}, {endpos, endpos}}] <>", ",
			json<>", "
		]
	];
	
	(* 	Recursion depth=2 is a list of atomic values, depth=3 lists of Rules *)
	Which[
		depth==2, "(" <> StringDrop[StringJoin@(transFun/@lst), -2] <> ")",
		depth==3, StringDrop[StringJoin@(transFun/@lst), -2],
		depth==4, "(" <> StringDrop[StringJoin@(transFun/@lst), -2] <> ")"
	]
  ] 

(* table Dataset with named columns,  i.e. many records *)
Options[DBDatasetToSQL92]={values->True};

DBDatasetToSQL92[ds_Dataset /; (ds[1] // Head) === Dataset, OptionsPattern[]]:=
	If[	OptionValue[values], 
		StringTake[(DBListSetToSQL92 /@ (ds // Normal // Values)) // ToString, {2, -2}],	
		"(" <> StringTake[  ToString[ds[1] // Normal // Keys], {2, -2}] <> ")"]


(* Dataset with a single record, row of keys and row of values  *)
DBDatasetToSQL92[ds_Dataset, OptionsPattern[]]:=
	If[	OptionValue[values],
		ds//Normal//Values//DBListSetToSQL92,				
		"(" <> StringTake[  ToString[ds // Normal // Keys], {2, -2}] <> ")"]
		

End[];

(* Protect Section *)
Protect[		
	DBexpressionToJSON,	
	
	DBjsonToRules,
	DBjsonToAssociations,
	DBjsonToDataset,
	
	DBListSetToRuleSet,
	DBListSetToAssociationSet,
	DBListSetToDataset,
					
	DBListSetToSQL92,
	DBDatasetToSQL92
]

(* Print Information *)
version=SystemInformation["Kernel"][[1]]
Print["DBAPI Application Project"]
Print["Promoted and Distributed by ", Hyperlink["HEALIS", "http://healis.eu"], "- Healthy Information Systems/Services"]
Print["Running on Mathematica ",version]
Print[]
Print["Data Utilities Package v0.9"]
Print["Copyright December 2015, By ", Hyperlink["Athanassios I. Hatzis","https://www.linkedin.com/in/athanassios"] ]
Print["Distributed under GNU LGPL - GNU Lesser General Public License\n"]

EndPackage[];
