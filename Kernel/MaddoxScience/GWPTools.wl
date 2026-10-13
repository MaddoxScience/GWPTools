(* ::Package:: *)

(* ::Title:: *)
(*GWPTools Package*)


(* ========================================================================= *)
(* PACKAGE     : MaddoxScience`GWPTools`                                     *)
(* DESCRIPTION : The Master Paclet Loader (Facade). Orchestrates the loading *)
(*               sequence for the core registries, dispatchers, and engines. *)
(* ========================================================================= *)

BeginPackage["MaddoxScience`GWPTools`"]

If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], 
  Print["[GWPTools] Initiating Master Boot Sequence..."]
];

(* 1. Load the Database *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], 
  Print["[GWPTools] Loading MaddoxScience`GWPTools`GWPRegistry`..."]
];
Needs["MaddoxScience`GWPTools`GWPRegistry`"];

(* 2. Load the Dispatcher *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], 
  Print["[GWPTools] Loading MaddoxScience`GWPTools`GWPDispatcher`..."]
];
Needs["MaddoxScience`GWPTools`GWPDispatcher`"];

(* 3. Load Stable Math Engines *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], 
  Print["[GWPTools] Loading MaddoxScience`GWPTools`GWPEngine1D`..."]
];
Needs["MaddoxScience`GWPTools`GWPEngine1D`"];

(* 4. Load Experimental Engines and Extensions (Gated by Package Flag) *)
If[TrueQ[MaddoxScience`GWPTools`$GWPExperimental],
  If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], 
    Print["[GWPTools] Loading Experimental Extensions..."]
  ];
  Needs["MaddoxScience`GWPTools`GWPEngine2D`"];
  Needs["MaddoxScience`GWPTools`GWPEngineRDM1D`"];
  Needs["MaddoxScience`GWPTools`GWPEngineSS1D`"];
  Needs["MaddoxScience`GWPTools`GWPEngineMC1D`"];
  Needs["MaddoxScience`GWPTools`GWPHydrodynamics`"];
  Needs["MaddoxScience`GWPTools`GWPPhaseSpace`"];
  Needs["MaddoxScience`GWPTools`GWPNumerical`"]; 
   
  (* Expose the Developer context directly *)
  If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], 
    Print["[GWPTools] Loading GWPDeveloper Context..."]
  ];
  Get["MaddoxScience`GWPTools`GWPDeveloper`"];
];

If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], 
  Print["[GWPTools] Master Loading Sequence Complete."]
];

EndPackage[]
