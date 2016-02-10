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

{com-> {	"\"addOSQLScript\"", 
				"\"addOSQLCommand\"",
				"\"addDatabase\"",
				"\"addClass\"",
				"\"addClassViaHTTP\"",
				"\"addProperty\"",
				"\"addPropertyViaHTTP\"",
				"\"addRecord\"",
				"\"addContent\"",
				"\"addValues\"",
				"\"addInstance\"",
				"\"addIndex\"",
				"\"addEdge\"",
				
				"\"delDatabase\"",
				"\"delClass\"",
				"\"delProperty\"",
				"\"delValues\"",
				"\"delRecords\"",
				"\"delConnection\"",
				
				"\"updDatabase\"",
				"\"updClass\"",
				"\"updProperty\"",
				"\"updValues\"",
				"\"updRecord\"",
				
				"\"getServer\"",
				"\"getOSQLCommand\"",
				"\"getDatabases\"",
				"\"getClass\"",
				"\"getRecords\"",
				
				"\"impDatabase\"",
				"\"expDatabase\"",
				
				"\"logout\"",
				"\"login"	},
				
		debug->{"True","False"},
		all->{"True","False"},
		uniq->{"True","False"},
		mode->{"\"COMMAND\"","\"BATCH\""},
		construct->{"\"DOCUMENT\"", "\"VERTEX\"","\"EDGE\""},
		user->{"\"admin\""},
		pwd->{"\"admin\""},
		port->{"\"2480\""},
		server->{"\"localhost\""},
		method->{"\"PUT\"","\"GET\"","\"DELETE\"","\"PUT\"","\"PATCH\""}
	} 
