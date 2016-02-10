(* ::Package:: *)
(* :Title: OrientDB *)
(* :Purpose: Porting of OrientDB RESTful HTTP API commands in Wolfram  *)
(* :Context: DBAPI`OrientDB` *)
(* :Dependencies: DBAPI`Utils`, GeneralUtilities`*)

(* :PackageVersion: 1.0.3 *)
(* :History:  
	Version 0.9.0 December 24 2015 -- 1st Release 
	Version 1.0.3 February 09 2016 -- 2nd Release *)

(* :MathematicaVersion: >10.2.0 *)

(* :Author: Athanassios I. Hatzis *)
(* :LatestReleaseDate: 2016-02-09 *)
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


BeginPackage["DBAPI`OrientDB`", {"DBAPI`Utils`","GeneralUtilities`"}];

(* Unprotect and Clear All*)
(*UnProtect[
	ODBapi, 
	ODBgetFieldAttributes,	
	ODBgetDataset
]

ClearAll[  
	ODBapi, 
	ODBgetFieldAttributes,	
	ODBgetDataset	
 ]
 *)
(* Usage Messages *)
ODBapi::usage="
	ODBapi[com->\"add<Command>\", options] 
	Uses the POST method of the HTTP protocol for both reading and writing the database

	ODBapi[com->\"del<Command>\", options] 
	Uses either the POST or the DELETE method of the HTTP protocol to destructively alter the database

	ODBapi[com->\"upd<Command>\", options] 
	Uses either the POST HTTP method to update record values with SQL UPDATE command or the PUT and PATCH methods of the HTTP protocol to update OrientDB structured Document records

	ODBapi[com->\"get<Command>\", options] 
	Uses the GET method of the HTTP protocol to retrieve values from the database. Operations are idempotent, i.e. they do not alter the database

	ODBapi[com->\"import/export\", options ] 
	Export a gzip file that contains the database JSON export using the GET method. Import a
	
	ODBapi[com->\"login/logout\", options ] 
	Uses the GET method of the HTTP protocol to connect to a remote server using basic authentication and the same method for disconnecting
"

ODBgetDataset::usage="ODBgetDataset[ODBapi response] 
	transforms the response of the ODBapi to a Dataset"

ODBgetFieldAttributes::usage="ODBgetFieldAttributes[Dataset, {attributes}] 
	get the field (property) attributes from an OrientDB schema-full Class"	
	
(* Error Messages *)

Begin["`Private`"];

(*
 	============================================= 
	Public Functions Definitions
	=============================================
*)

ODBgetFieldAttributes[ds_Dataset, attr_]:=Block[
	{
		classNames=Normal@ds[All,"name"],
		classProperties=ds[[#]]["properties"][All,attr]&/@ Range[1, Length@ds]		
	},
	Thread[classNames->classProperties]
]

ODBgetFieldAttributes[ds_Dataset /; Depth@Normal@ds==4, attr_]:=ds["properties"][All,attr]

(* This is based on DBjsonToDataset but here we check first and strip JSON from the "result" *)
ODBgetDataset[httpResponse_List]:=Block[
	{respAss},
	respAss = httpResponse[[1]] // DBjsonToAssociations;
	 If[
	 	KeyExistsQ[respAss,"result"],
	 	(httpResponse[[1]] // DBjsonToDataset)["result"],
	 	httpResponse[[1]] // DBjsonToDataset	 	
	 ] 
] (* End of ODBgetDataset *)	 	

Options[ODBfetch] = {
					(* url construction parameters *)
					server->"localhost",
					port->"2480",
					db->"",
					dbtype->"",
					com->"",
					arg->"",
					con->"",

					id->"",
					ver->-1,
					propnam->"",
					propval->"",
					proptype->"",
					attribnam->"",
					attribval->"",
					indexnam->"",
					indextype->"",
					record->"",
					all->False,
					uniq->False,
					
					(* sql command parameters *)
					sql->"",
					class->"",
					superclass->"",
					prjkt->"",
					keys->"",
					values->"",
					construct->"",
					from->"",
					to->"",
	
					(* URLFetch Parameters *)
					method->"PUT",
					param->{},
					body->"",
					usr->"admin",
					pwd->"admin",
					mode->"COMMAND",
					
					(* Debug, i.e. print the URLFetch command *)
					debug->False
}; (* End of ODBfetch Options *)

Options[ODBapi]=Options[ODBfetch]

ODBapi[opts: OptionsPattern[]] := Block[
	{result, allval, uniqval, modeval,
	comval,  sqlval, conval,
	dbval, dbtypeval, 
	classval, superclassval, supertype,
	prjktval, 
	keysval, valuesval, constructval, fromval, toval,
	recid, recval, recver, bodycontent, 
	fieldnam, fieldval, fieldtype, fieldarg,
	attrnam, attrval, ndxnam, ndxtype,
	methodval, argval, getcmd},
	
	(* supertype, bodycontent and fieldarg and getcmd
	    are temporary variables that are not set to option values like the rest *) 

	getcmd="";
	argval="";
	methodval=OptionValue[method];
	allval=OptionValue[all];
	uniqval=OptionValue[uniq];
	modeval=OptionValue[mode];
	
	comval=OptionValue[com];
	sqlval=OptionValue[sql];
	conval=OptionValue[con];
			
	dbval=OptionValue[db];
	dbtypeval=OptionValue[dbtype];
	
	classval=OptionValue[class];
	superclassval=OptionValue[superclass];
	supertype="";
	
	prjktval=OptionValue[prjkt];
	
	keysval=OptionValue[keys];
	valuesval=OptionValue[values];
	constructval=OptionValue[construct];
	fromval=OptionValue[from];
	toval=OptionValue[to];
		
	recid=OptionValue[id];
	recval=OptionValue[record];
	recver=OptionValue[ver];
	bodycontent="";
	
	fieldnam=OptionValue[propnam];
	fieldval=OptionValue[propval];
	fieldtype=OptionValue[proptype];
	fieldarg="";
	
	ndxnam=OptionValue[indexnam];
	ndxtype=OptionValue[indextype];
	
	attrnam=OptionValue[attribnam];
	attrval=OptionValue[attribval];
	
	(* Used in Which [comval == "getDatabases"....	*)
	If[ comval=="getDatabases",
		Which[
			dbval!="",
			getcmd="database",
			
			True,
			getcmd="listDatabases"] ];
			
	(* Used in Which [comval == "addOSQL"..... 
	The argument mode [BATCH, COMMMAND] specifies whether to execute 
	an sql scipt or an sql command. The sql argument is mandatory	*)
	If[ comval=="addOSQL",
		Which[
			modeval=="BATCH",
			getcmd="batch";
			bodycontent=ODBmakeJSONScript[sqlval],
			
			modeval=="COMMAND",
			argval="sql";getcmd="command";
			bodycontent=sqlval,
			
			True,
			comval="ABORT"] ];				
	
	(* Used in Which [comval == "getRecords".....
	You can either get a single record, i.e. structured document back by specifying the record id	
	Or get multiple records back with	
	Mandatory parameters, classval, fieldnam, fieldvalue	*)
	If[ comval=="getRecords",
		Which[ 
			recid!="",
			argval=recid;getcmd="document";
			bodycontent="",
			
			classval!="" && fieldval !="" && fieldnam !="",
			argval="sql";getcmd="command";
			bodycontent="select " <> prjktval <> " from " <> classval <> " WHERE " <> fieldnam <> "=" <> "\""<> fieldval <> "\"",
			
			classval!="",
			argval="sql";getcmd="command";
			bodycontent="select " <> prjktval <> " from " <> classval,
			
			True, 
			comval="ABORT"] ];
	
	(* Used in Which [comval=="delRecords"....
		WIth tha all->True Optional arguement 
		We use TRUNCATE CLASS to delete ALL records from a DOCUMENT class
		and DELETE VERTEX to delete ALL vertices from a VERTEX class
		Otherwise delete specific records by passing the recid, all->False *)
	If[	(comval=="delRecords"),
		Which[classval !="" && allval==True && constructval=="DOCUMENT", bodycontent="TRUNCATE CLASS "<> classval,
			classval !="" && allval==True && constructval=="VERTEX",  bodycontent="DELETE VERTEX " <> classval,
			allval==False && recid!="", bodycontent= "TRUNCATE RECORD "<>recid,
		True, comval="ABORT"] ]; 		 
	
	(* Used in Which [comval=="addEdge".....
		There are three cases, two of them use the default OrientDB Edges that can take properties
		We add either a json CONTENT record or a single value with SET
		The last case is used in OrientDB LightweightEdges, i.e. bidirectional links with no record attached  
	*)	
	If[ (comval=="addEdge"),
		Which[classval !="" && fromval!="" && toval!="",
			  If[recval !="", 
				bodycontent="CREATE EDGE " <> classval <> " FROM " <> fromval <> " TO " <> toval <> " CONTENT " <> recval,
				bodycontent="CREATE EDGE " <> classval <> " FROM " <> fromval <> " TO " <> toval ],
			
			  classval !="" && fromval!="" && toval!="" && fieldnam!="" && fieldval!="", 
			  bodycontent="CREATE EDGE " <> classval <> "  FROM " <> fromval <> " TO " <> toval <> " SET " <> fieldnam<>"="<>fieldval,						
			  
			  True, comval="ABORT"] ];
		
	(* Used in Which [comval == "addRecord" ...... 
	It uses the HTTP POST - Document and the body has a JSON content
	recval is a JSON formatted String and 
	more specifically a JSON object with an unordered collection of key-value pairs
	StringTake strips JSON from the brackets { ..... } and concatenates this with the rest of the String
	In the case recval="", i.e. it can be ommitted then an empty Document record is created 
	bodycontent="INSERT INTO "<>classval <> " content " <> " {\"@type\":\"d\"} " 
	*)
	If[ (comval=="addRecord" && classval !=""),		
		Which[	recval!="", bodycontent="{\"@class\":\"" <> classval <> "\"," <> StringTake[recval, {2, -2}] <> "}",
				recval=="", bodycontent="{\"@class\":\"" <> classval<>"\"}",
		True, comval="ABORT"]	];
		
	(* Used in Which [comval == "addInstance" ...... 	
	INSERT INTO Profile SET name = 'Jay'
	CREATE VERTEX Car SET brand = 'fiat'
	Note that construct option in that case is mandatory and takes values (DOCUMENT, VERTEX)
	
	The UPSERT clause only guarantees atomicity when you use a UNIQUE index and perform the look-up on the index through the WHERE condition.
	orientdb> UPDATE Client SET id = 23 UPSERT WHERE id = 23
	Here, you must have a unique index on Client.id to guarantee uniqueness on concurrent operations.	*)
	If[	(comval=="addInstance" && classval !=""),
		If[	(fieldnam!="" && fieldval!=""),
			Which[constructval=="DOCUMENT", bodycontent="INSERT INTO "<>classval<>" SET "<>fieldnam<>"="<>fieldval,
				constructval=="VERTEX", bodycontent="CREATE VERTEX "<>classval<>" SET "<>fieldnam<>"="<>fieldval,
				uniqval, bodycontent="UPDATE " <> classval <> " SET " <> fieldnam <> "=" <> fieldval <> " upsert return after @rid where " <> fieldnam <> "=" <> fieldval,
			True, comval="ABORT"],
			Which[constructval=="DOCUMENT", bodycontent="INSERT INTO "<>classval<>" CONTENT "<>"{\"@class\":\""<>classval<>"\"}",
				constructval=="VERTEX", bodycontent="CREATE VERTEX "<>classval,				
			True, comval="ABORT"]
		] 
	];
	
	(* Used in Which [comval=="updValues"....
		You can either update the value of a field in a number of records, all->True 
		or in a single record with a specific rid.	*)
	If[(comval=="updValues" && fieldnam!="" && fieldval!=""),
		Which[allval==True && classval!="", bodycontent="UPDATE "<>classval<>" set "<>fieldnam<>"="<>fieldval,
			allval==False && recid!="", bodycontent="UPDATE "<>recid<>" set "<>fieldnam<>"="<>fieldval,
		True, comval="ABORT"] ];
	
	(* Used in Which [comval=="addValues".... 
	Attention : keysval and valuesval are formatted according to SQL-92 syntax, e.g. 
	INSERT INTO Profile (name, surname) VALUES ("Jay", "Miner"), ("Bary", "Osborn"), .... 
	Do not use that to insert JSON content
	This is probably the fastest method for inserting multiple records *)
	If[ (comval=="addValues"),
		Which[ keysval!="" && valuesval!="",
			bodycontent = "INSERT INTO "<>classval<>" "<>keysval<>" VALUES "<>valuesval,
		True, comval="ABORT"] ]; 
	
	(* Used in Which [comval == "addContent" ...... 
	recordval is a JSON formatted String e.g.
	INSERT INTO Profile CONTENT {"name": "Jay", "surname" = "Miner"}
	Note that construct option in that case is mandatory and takes values (DOCUMENT, VERTEX, EDGE)
	CREATE VERTEX Person CONTENT {"name": "Jay", "surname" = "Miner"}
	CREATE EDGE hasFriend FROM #22:33 TO #22:55 CONTENT { "name" : "Jay", "surname" : "Miner" }
	
	Set Option construct->"DOCUMENT" or construct->"VERTEX" or construct->"EDGE" otherwise comval=ABORT operation 	*)	
	If[ (comval=="addContent" && classval !="" && recval !=""),
		 Which[constructval=="DOCUMENT", bodycontent="INSERT INTO " <> classval <> " content "	<> recval,
		 		   constructval=="VERTEX",  bodycontent="CREATE VERTEX " <> classval <> " content "	<> recval,
		 		   constructval=="EDGE", bodycontent="CREATE EDGE " <> classval <> " FROM " <> fromval <> " TO " <> toval <> " content " <> recval,
		 True, comval="ABORT"] ];
		 		   
	(* Used in Which [comval == "updRecord".......
	It uses the HTTP PUT or PATCH method - Document and the body has a JSON content
	Default method is HTTP PUT, remember to set method->"PATCH" for HTTP PATCH	
	Each updated record has a version metadata property. This prevents updating documents changed by other users (MVCC).
	With the PUT method, version is optional but with the PATCH method record version is MANDATORY
	*)
	If[ comval=="updRecord" && recid !="",
		Which[
			recver!=-1 && methodval=="PUT",
			bodycontent="{\"@version\":" <> ToString[recver] <> ", " <> StringTake[recval, {2, -2}] <> "}",
			
			recver==-1 && methodval=="PUT",
			bodycontent=recval,	
		
			recver!=-1 && methodval=="PATCH", 
			bodycontent="{\"@class\":" <> "\""<>classval<> "\", " <>"\"@version\":"<>ToString[recver]<> ", "<> StringTake[recval, {2, -2}] <> "}",
		
			True, 
			comval="ABORT"] ];
	
 	(* Used in Which [comval == "addProperty" ...... 
 	Uses HTTP POST 
 	*)	
	If[ fieldtype=="", 
		fieldarg=classval<>"/"<>fieldnam,
		fieldarg=classval<>"/"<>fieldnam<>"/"<>fieldtype];
		
	(* Used in Which [comval == "addClass" ...... *)
	If[ superclassval=="", 
		supertype="", 
		supertype=" extends "<>superclassval];
	
	result = Which[
						comval == "addOSQL", ODBfetch[
																		method-> "POST",																		
																		com->getcmd,																		
																		arg->argval,																		
																		db->dbval,
																		body->bodycontent,																		
																		opts, 
																		Sequence@@Options@ODBapi],
																		
						comval == "getOSQL", ODBfetch[
																		method-> "GET", 
																		com->"command", 
																		db->dbval<>"/sql", 
																		body->sqlval,
																		opts, 
																		Sequence@@Options@ODBapi],
												
						comval == "logout", ODBfetch[
																		method->"GET", 
																		com->"disconnect", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "login", ODBfetch[
																		method->"GET", 
																		com->"connect", 
																		opts, 
																		Sequence@@Options@ODBapi],												
						
						comval == "addDatabase", ODBfetch[
																		method-> "POST", 
																		com->"database",
																		arg->dbtypeval, 
																		opts, 
																		Sequence@@Options@ODBapi],

						comval == "addEdge", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->bodycontent, 
																		opts, 
																		Sequence@@Options@ODBapi],
																				
						comval == "addClass", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"CREATE CLASS "<>classval<>supertype, 
																		opts, 
																		Sequence@@Options@ODBapi],
																																				
						comval == "addClassViaHTTP", ODBfetch[
																		method-> "POST", 
																		com->"class",
																		arg->classval, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "addIndex", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"CREATE INDEX "<>ndxnam<>" ON "<>classval<>" ("<>fieldnam<>") "<>ndxtype, 
																		opts,
																		Sequence@@Options@ODBapi],
						
						comval == "addProperty", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"CREATE PROPERTY "<>classval<>"."<>fieldnam<>" "<>fieldtype, 
																		opts,
																		Sequence@@Options@ODBapi],
																		
						comval == "addPropertyViaHTTP", ODBfetch[
																		method-> "POST", 
																		com->"property",
																		arg->fieldarg,																		
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "addRecord", ODBfetch[
																		method-> "POST", 
																		com->"document",
																		db->dbval,
																		body->bodycontent,																																
																		opts, 
																		Sequence@@Options@ODBapi],
												
						comval == "addContent", ODBfetch[ 
																		method->"POST", 
																		com-> "command", 
																		db->dbval<>"/sql",
																		body->bodycontent, 																	
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "addValues", ODBfetch[ 
																		method->"POST", 
																		com-> "command", 
																		db->dbval<>"/sql",
																		body->bodycontent, 																	
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "addInstance", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->bodycontent, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "delDatabase", ODBfetch[
																		method-> "DELETE", 
																		com->"database", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "delClass", ODBfetch[
																		method-> "POST", 
																		com->"command", 
																		db->dbval<>"/sql", 
																		body->"DROP CLASS "<>classval,
																		opts, 
																		Sequence@@Options@ODBapi],																		
						
						comval == "delRecords", ODBfetch[
																		method-> "POST", 
																		com->"command", 
																		db->dbval<>"/sql", 
																		body->bodycontent,
																		opts, 
																		Sequence@@Options@ODBapi],																		
																								
						comval == "delConnection", ODBfetch[
																		method-> "POST", 
																		com->"connection",
																		arg->"kill/"<>conval, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						
						(* This doesn't remove the property values in records, but just change the schema information. 
						Records will continue to have the property values if any.*)
						comval == "delProperty", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"DROP PROPERTY "<>classval<>"."<>fieldnam, 
																		opts, 
																		Sequence@@Options@ODBapi],																		
						
						comval == "delValues", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"UPDATE "<>classval<>" remove "<>fieldnam, 
																		opts, 
																		Sequence@@Options@ODBapi],						
						
						comval == "updDatabase", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"ALTER DATABASE "<>attrnam<>" "<>attrval, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "updClass", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"ALTER CLASS "<>classval<>" "<>attrnam<>" "<>attrval, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "updProperty", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"ALTER PROPERTY "<>classval<>"."<>fieldnam<>" "<>attrnam<>" "<>attrval, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "updValues", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->bodycontent, 
																		opts, 
																		Sequence@@Options@ODBapi],												
																		
						comval == "updRecord", ODBfetch[
																		method-> methodval, 
																		com->"document",
																		arg->recid, 
																		body->bodycontent,																																
																		opts, 
																		Sequence@@Options@ODBapi],
																																				
						comval == "getServer", ODBfetch[
																		method->"GET", 
																		com->"server", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "getDatabases", ODBfetch[
																		method->"GET", 
																		com->getcmd, 
																		opts, Sequence@@Options@ODBapi],
												
						comval == "getClass", ODBfetch[
																		method->"GET", 
																		com->"class", 
																		arg->classval,
																		opts, Sequence@@Options@ODBapi],
						
						(* Better check first if record exists with an HTTP HEAD request *) 
						comval == "getRecords", ODBfetch[
																		method->"GET", 
																		com->getcmd,																		
																		arg->argval,																		
																		db->dbval,
																		body->bodycontent, 
																		opts, 
																		Sequence@@Options@ODBapi],
																								
						comval == "impDatabase", ODBfetch[
																		method->"POST", 
																		com->"import", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "expDatabase", ODBfetch[
																		method->"GET", 
																		com->"export", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						True, Return["ERROR: Wrong or Missing Arguments]"]																	
																																
					] (* End of Which *)
	
] (* End of ODBapi Block Structure*)


(*
 	============================================= 
	Private Functions Definitions
	=============================================
*)

ODBmakeJSONScript[content_]:= ExportString[
															 {
													    		"transaction" -> False,
													    		"operations" -> {{
															        "type" -> "script",
															        "language" -> "sql",
															        "script" -> content}}
															  }, "JSON"
] (* End of ODBmakeJSONScipt *)



ODBfetch[opts: OptionsPattern[]]:=Module[
	{result,url},
	
	url= "http://"<>OptionValue[server]
							<>":"<>OptionValue[port]
							<>If[OptionValue[com] !="","/"<>OptionValue[com],""]
							<>If[OptionValue[db] !="", "/"<>OptionValue[db],""]
							<>If[OptionValue[arg]!="","/"<>OptionValue[arg],""];							
	
	If[OptionValue[debug],Print[url, "\n=== Body ===\n",OptionValue[body]]];
	
	result=URLFetch[url, 
		{"Content", "StatusCode"},
		"Method"->OptionValue[method],
		"Parameters"->OptionValue[param],
		"BodyData"->OptionValue[body],
		"Username"->OptionValue[usr],
		"Password"->OptionValue[pwd]]
] (*End of ODBfetch *)



End[];

(* Protect Section *)
(*Protect[
	ODBapi, 
	ODBgetFieldAttributes,	
	ODBgetDataset
]
*)
(* Print Information *)

	Print["OrientDB API Package v1.0.3"]
	Print["Copyright February 2016, By ", Hyperlink["Athanassios I. Hatzis","https://www.linkedin.com/in/athanassios"] ]	
	Print["Distributed under GNU LGPL - GNU Lesser General Public License\n"]	

EndPackage[];
