(* ::Package:: *)

(* ::Title:: *)
(*GWPPhaseSpace Package*)


(* ::Section::Closed:: *)
(*BeginPackage*)


(* ========================================================================= *)
(* PACKAGE     : MaddoxScience`GWPTools`GWPPhaseSpace`                       *)
(* DESCRIPTION : Master orchestrator for Phase Space extensions.             *)
(*               Dynamically loads subpackages based on active GWP engines.  *)
(* ========================================================================= *)

BeginPackage["MaddoxScience`GWPTools`GWPPhaseSpace`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPPhaseSpace] BeginPackage"]];

Begin["`Private`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPPhaseSpace] Begin Private"]];


(* ::Section::Closed:: *)
(*Load Engine Extensions*)


(* Helper to safely load a subpackage if its parent engine is active *)
LoadPhaseSpaceExtension[type_, context_] := Module[{engineParamSymbol, packagePath},
  (* We check if the engine's core parameter builder exists in the kernel *)
  engineParamSymbol = "MaddoxScience`GWPTools`GWPDeveloper`GWP" <> type <> "PARAM";
  
  If[NameQ[engineParamSymbol],
    packagePath = "MaddoxScience`GWPTools`" <> context <> "`";
    If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPPhaseSpace] Loading Extension: ", packagePath]];
    Quiet[Needs[packagePath]];
  ]
];

(* Dynamically load available phase space modules *)
LoadPhaseSpaceExtension["1D", "GWPPhaseSpace1D"];
LoadPhaseSpaceExtension["SS1D", "GWPPhaseSpaceSS1D"];
(* Add 2D and SS2D later as they are developed *)


(* ::Section::Closed:: *)
(*End*)


(* --- End "MaddoxScience`GWPTools`GWPPhaseSpace`Private`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPPhaseSpace] End Private"]];
End[]

(* Hide internal code for all Developer functions from the ? menu *)
SetAttributes[Evaluate[Names["MaddoxScience`GWPTools`GWPPhaseSpace`*"]], {ReadProtected}];

(* --- End "MaddoxScience`GWPTools`GWPPhaseSpace`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPPhaseSpace] EndPackage"]];
EndPackage[]

(* Scrub backend context to keep the user namespace pristine *)
$ContextPath = DeleteCases[$ContextPath, "MaddoxScience`GWPTools`GWPPhaseSpace`"];
