(* ::Package:: *)

(* Mathematica package 
List of possible options for ODBapi
Mathematically dynamically prompt the user to choose from a list with these options.
e.g.
ODBapi[com->......

This file must be placed under this folder
FileNameJoin[{$InstallationDirectory, "SystemFiles", "FrontEnd", "SystemResources", "FunctionalFrequency", "OptionValues"}]

The name of the file determines the name of the function that will take the optional arguments
*)

{com-> {		"\"addOSQLScript\"", 
				"\"addOSQLCommand\"",
				"\"addDatabase\"",
				"\"addClass\"",
				"\"addClassViaHTTP\"",
				"\"addProperty\"",
				"\"addPropertyViaHTTP\"",
				"\"addDOCUMENT\"",
				"\"addCONTENT\"",
				"\"addVALUES\"",
				
				"\"delDatabase\"",
				"\"delClass\"",
				"\"delProperty\"",
				"\"delPropertyValues\"",
				"\"delAllRecords\"",
				"\"delRecords\"",
				"\"delConnection\"",
				
				"\"updDatabase\"",
				"\"updClass\"",
				"\"updProperty\"",
				"\"updPropertyValue\"",
				"\"updPropertyValues\"",
				"\"updRecordPUT\"",
				"\"updRecordPATCH\"",
				
				"\"getServer\"",
				"\"getOSQLCommand\"",
				"\"getDatabases\"",
				"\"getDatabase\"",
				"\"getClass\"",
				"\"getRecord\"",
				
				"\"importDatabase\"",
				"\"exportDatabase\"",
				
				"\"logout\"",
				"\"login"	},
				
		debug->{"True","False"},
		
		construct->{"\"RECORD\"", "\"VERTEX\"","\"EDGE\""},
		user->{"\"admin\""},
		pwd->{"\"admin\""},
		port->{"\"2480\""},
		server->{"\"localhost\""},
		method->{"\"PUT\"","\"GET\"","\"DELETE\"","\"PUT\"","\"PATCH\""}
	} 
