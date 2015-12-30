(* ::Package:: *)
(* :Title: OrientDB *)
(* :Purpose: Porting of OrientDB RESTful HTTP API commands in Wolfram  *)
(* :Context: DBAPI`OrientDB` *)
(* :Dependencies: DBAPI`Utils`, GeneralUtilities`*)

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


BeginPackage["DBAPI`OrientDB`", {"DBAPI`Utils`","GeneralUtilities`"}];

(* Unprotect and Clear All*)
UnProtect[
	ODBapi, 
	ODBgetFieldAttributes,	
	ODBgetDataset
]

ClearAll[  
	ODBapi, 
	ODBgetFieldAttributes,	
	ODBgetDataset	
 ]
 
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
					record->"",
					
					(* sql command parameters *)
					sql->"",
					class->"",
					superclass->"",
					keys->"",
					values->"",
					construct->"",
					from->"",
					to->"",
					
					(* URLFetch Parameters *)
					method->"POST",
					param->{},
					body->"",
					usr->"admin",
					pwd->"admin",
					
					(* Debug, i.e. print the URLFetch command *)
					debug->False
}; (* End of ODBfetch Options *)

Options[ODBapi]=Options[ODBfetch]

ODBapi[opts: OptionsPattern[]] := Block[
	{result, 
	comval,  sqlval, conval,
	dbval, dbtypeval, 
	classval, superclassval, supertype, keysval, valuesval, constructval, fromval, toval,
	recid, recval, recver,recordcontent, 
	fieldnam, fieldval, fieldtype, fieldarg,
	attrnam, attrval},
		
	dbval=OptionValue[db];
	dbtypeval=OptionValue[dbtype];
	comval=OptionValue[com];
	sqlval=OptionValue[sql];
	classval=OptionValue[class];
	superclassval=OptionValue[superclass];
	keysval=OptionValue[keys];
	valuesval=OptionValue[values];
	constructval=OptionValue[construct];
	fromval=OptionValue[from];
	toval=OptionValue[to];
	conval=OptionValue[con];	
	recid=OptionValue[id];
	recval=OptionValue[record];
	recver=OptionValue[ver];
	fieldnam=OptionValue[propnam];
	fieldval=OptionValue[propval];
	fieldtype=OptionValue[proptype];
	attrnam=OptionValue[attribnam];
	attrval=OptionValue[attribval];
		
	(* Used in Which [comval == "addDOCUMENT" ...... 
	It uses the HTTP POST - Document and the body has a JSON content
	recordval is a JSON formatted String and 
	more specifically a JSON object with an unordered collection of key-value pairs
	StringTake strips JSON from the brackets { ..... } and concatenates this with the rest of the String 
	*)
	If[ 
		(classval !="" && recval !="" && comval=="addDOCUMENT"), 
		recordcontent="{\"@class\":\"" <> classval <> "\"," <> StringTake[recval, {2, -2}] <> "}"		
	];
	
	
	(* Used in Which [comval == "addCONTENT" ...... 
	recordval is a JSON formatted String e.g.
	INSERT INTO Profile CONTENT {"name": "Jay", "surname" = "Miner"}
	Note that construct option in that case is mandatory and takes values (RECORD, VERTEX, EDGE)
	CREATE VERTEX Person CONTENT {"name": "Jay", "surname" = "Miner"}
	CREATE EDGE hasFriend FROM #22:33 TO #22:55 CONTENT { "name" : "Jay", "surname" : "Miner" }
	
	Set Option construct->"RECORD" or construct->"VERTEX" or construct->"EDGE" otherwise comval=ABORT operation 	*)
	If[ 
		(classval !="" && recval !="" && comval=="addCONTENT"),
		 Which[constructval=="RECORD", recordcontent="INSERT INTO " <> classval <> " content "	<> recval,
		 		   constructval=="VERTEX",  recordcontent="CREATE VERTEX " <> classval <> " content "	<> recval,
		 		   constructval=="EDGE", recordcontent="CREATE EDGE " <> classval <> " FROM " <> fromval <> " TO " <> toval <> " content " <> recval,
		 		   True, comval="ABORT"]	
	];
	
	
	(* Used in Which [comval == "addVALUES" ...... 
	Attention : keysval and valuesval are formatted according to SQL-92 syntax, e.g. 
	INSERT INTO Profile (name, surname) VALUES ("Jay", "Miner"), ("Bary", "Osborn"), ....
	CREATE VERTEX Profile (name,surname) 
	Do not use that to insert JSON content
	This is probably the fastest method for inserting multiple records
	Note that construct option in that case is mandatory and takes values (RECORD, VERTEX)
	*)
	If[ 
		(keysval !="" && valuesval !="" && comval=="addVALUES"),
		Which[constructval=="RECORD", recordcontent="INSERT INTO "<> classval <> " "<> keysval <> " VALUES " <> valuesval,
		 		   constructval=="VERTEX",  recordcontent="CREATE VERTEX " <> classval <> " "<> keysval <> " VALUES " <> valuesval,		 		   
		 		   True, comval="ABORT"]
	]; 	
	
	(* Used in Which [comval == "updRecordPUT".......
	It uses the HTTP PUT - Document and the body has a JSON content	
	Remember to always pass the version to update. This prevent to update documents changed by other users (MVCC).
	*)
	If[
		(recid !="" && comval=="updRecordPUT"),
		Which[
			recver!=-1, recordcontent="{\"@version\":" <> ToString[recver] <> ", " <> StringTake[recval, {2, -2}] <> "}",
			recver==-1, recordcontent=recval]
	];
	
	
	(* Used in Which [comval == "updRecordPATCH".......
	Uses the HTTP PATCH - Document and the body has a JSON content	
	Record version is MANDATORY here. This prevent to update documents changed by other users (MVCC).
	*)
	If[
		(recid !="" && recver!=-1 && comval=="updRecordPATCH"),		
		recordcontent="{\"@class\":" <> "\""<>classval<> "\", " <>"\"@version\":"<>ToString[recver]<> ", "<> StringTake[recval, {2, -2}] <> "}"				
	];
	
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
						comval == "addOSQLScript", ODBfetch[
																		method-> "POST",
																		com->"batch", 																					
																		body->ODBmakeJSONScript[sqlval], 
																		opts, 
																		Sequence@@Options@ODBapi],
												
						comval == "addOSQLCommand", ODBfetch[
																		method-> "POST", 
																		com->"command", 
																		db->dbval<>"/sql", 
																		body->sqlval,
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "getOSQLCommand", ODBfetch[
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
						
						comval == "addDOCUMENT", ODBfetch[
																		method-> "POST", 
																		com->"document",
																		db->dbval,
																		body->recordcontent,																																
																		opts, 
																		Sequence@@Options@ODBapi],
												
						comval == "addCONTENT", ODBfetch[ 
																		method->"POST", 
																		com-> "command", 
																		db->dbval<>"/sql",
																		body->recordcontent, 																	
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "addVALUES", ODBfetch[ 
																		method->"POST", 
																		com-> "command", 
																		db->dbval<>"/sql",
																		body->recordcontent, 																	
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
						
						comval == "delAllRecords", ODBfetch[
																		method-> "POST", 
																		com->"command", 
																		db->dbval<>"/sql", 
																		body->"TRUNCATE CLASS "<>classval,
																		opts, 
																		Sequence@@Options@ODBapi],
																		
						comval == "delRecords", ODBfetch[
																		method-> "POST", 
																		com->"command", 
																		db->dbval<>"/sql", 
																		body->"TRUNCATE RECORD "<>recid,
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
						
						comval == "delPropertyValues", ODBfetch[
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
						
						comval == "updPropertyValues", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"UPDATE "<>classval<>" set "<>fieldnam<>"="<>fieldval, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "updPropertyValue", ODBfetch[
																		method-> "POST", 
																		com->"command",
																		db->dbval<>"/sql",
																		body->"UPDATE "<>recid<>" set "<>fieldnam<>"="<>fieldval, 
																		opts, 
																		Sequence@@Options@ODBapi],
																		
						comval == "updRecordPUT", ODBfetch[
																		method-> "PUT", 
																		com->"document",
																		arg->recid, 
																		body->recordcontent,																																
																		opts, 
																		Sequence@@Options@ODBapi],
																		
						comval == "updRecordPATCH", ODBfetch[
																		method-> "PATCH", 
																		com->"document",
																		arg->recid,
																		body->recordcontent,																																
																		opts, 
																		Sequence@@Options@ODBapi],
																		
						comval == "getServer", ODBfetch[
																		method->"GET", 
																		com->"server", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "getDatabases", ODBfetch[
																		method->"GET", 
																		com->"listDatabases", 
																		opts, Sequence@@Options@ODBapi],
						
						comval == "getDatabase", ODBfetch[
																		method->"GET", 
																		com->"database", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "getClass", ODBfetch[
																		method->"GET", 
																		com->"class", 
																		arg->classval,
																		opts, Sequence@@Options@ODBapi],
						
						(* Better check first if record exists with an HTTP HEAD request *) 
						comval == "getRecord", ODBfetch[
																		method->"GET", 
																		com->"document", 
																		arg->recid, 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "importDatabase", ODBfetch[
																		method->"POST", 
																		com->"import", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						comval == "exportDatabase", ODBfetch[
																		method->"GET", 
																		com->"export", 
																		opts, 
																		Sequence@@Options@ODBapi],
						
						True, Return["ERROR: Wrong Arguments]"]																	
																																
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
Protect[
	ODBapi, 
	ODBgetFieldAttributes,	
	ODBgetDataset
]

(* Print Information *)

	Print["OrientDB API Package v0.9"]
	Print["Copyright December 2015, By ", Hyperlink["Athanassios I. Hatzis","https://www.linkedin.com/in/athanassios"] ]	
	Print["Distributed under GNU LGPL - GNU Lesser General Public License"]	

EndPackage[];
