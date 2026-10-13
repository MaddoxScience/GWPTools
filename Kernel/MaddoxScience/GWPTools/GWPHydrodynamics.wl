(* ::Package:: *)

(* ::Title:: *)
(*GWPHydrodynamics Package*)


(* ::Section::Closed:: *)
(*BeginPackage*)


(* ========================================================================= *)
(* PACKAGE     : MaddoxScience`GWPTools`GWPHydrodynamics`                    *)
(* DESCRIPTION : Master orchestrator for Quantum Hydrodynamics extensions.   *)
(* ========================================================================= *)

BeginPackage["MaddoxScience`GWPTools`GWPHydrodynamics`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPHydrodynamics] BeginPackage"]];

Begin["`Private`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPHydrodynamics] Begin Private"]];

Needs["MaddoxScience`GWPTools`"];
Needs["MaddoxScience`GWPTools`GWPRegistry`"];


(* ::Section::Closed:: *)
(*Load Engine Extensions*)


LoadHydroExtension[type_, context_] := Module[{engineParamSymbol, packagePath},
  engineParamSymbol = "MaddoxScience`GWPTools`GWPDeveloper`GWP" <> type <> "PARAM";
  If[NameQ[engineParamSymbol],
    packagePath = "MaddoxScience`GWPTools`" <> context <> "`";
    If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPHydrodynamics] Loading Extension: ", packagePath]];
    Quiet[Needs[packagePath]];
  ]
];

LoadHydroExtension["1D", "GWPHydrodynamics1D"];
LoadHydroExtension["SS1D", "GWPHydrodynamicsSS1D"];
LoadHydroExtension["MC1D", "GWPHydrodynamicsMC1D"];
LoadHydroExtension["2D", "GWPHydrodynamics2D"];


(* ::Section::Closed:: *)
(*End*)


(* --- End "MaddoxScience`GWPTools`GWPHydrodynamics`Private`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPHydrodynamics] End Private"]];
End[]

(* Hide internal code for all Developer functions from the ? menu *)
SetAttributes[Evaluate[Names["MaddoxScience`GWPTools`GWPHydrodynamics`*"]], {ReadProtected}];

(* --- End "MaddoxScience`GWPTools`GWPHydrodynamics`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPHydrodynamics] EndPackage"]];
EndPackage[]

(* Scrub backend context to keep the user namespace pristine *)
$ContextPath = DeleteCases[$ContextPath, "MaddoxScience`GWPTools`GWPHydrodynamics`"];
