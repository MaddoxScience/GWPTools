(* ::Package:: *)

(* ::Title:: *)
(*GWPNumerical Package*)


(* ::Section::Closed:: *)
(*GWPTools Usage Registration*)


BeginPackage["MaddoxScience`GWPTools`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPNumerical] BeginPackage GWPTools"]];

GWPClearCache::usage = "GWPClearCache[] clears all memoized NDSolveValue trajectory caches across loaded GWPNumerical packages.\nGWPClearCache[\"Quiet\"] clears the caches silently.";

If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPNumerical] EndPackage GWPTools"]];
EndPackage[]


(* ::Section::Closed:: *)
(*BeginPackage*)


(* ========================================================================= *)
(* PACKAGE     : MaddoxScience`GWPTools`GWPNumerical`                        *)
(* DESCRIPTION : Master orchestrator for Numerical Potential extensions.     *)
(* ========================================================================= *)

BeginPackage["MaddoxScience`GWPTools`GWPNumerical`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPNumerical] BeginPackage"]];
Begin["`Private`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPNumerical] Begin Private"]];

Needs["MaddoxScience`GWPTools`"];
Needs["MaddoxScience`GWPTools`GWPRegistry`"];


(* ::Section::Closed:: *)
(*Cache Management Utilities*)


(* Clear all memoized NDSolveValue trajectories across all loaded GWPNumerical packages *)
GWPClearCache[mode_String : "Verbose"] := Module[{cacheSyms, shortNames},
  cacheSyms = Union @ Join[
    Names["MaddoxScience`GWPTools`*`*NDSolveCache*"],
    Names["MaddoxScience`GWPTools`*`Private`*NDSolveCache*"]
  ];
  Scan[
    Function[symName,
      With[{s = Symbol[symName]},
        Module[{attrs = Attributes[s]},
          Unprotect[s];
          ClearAttributes[s, ReadProtected];
          DownValues[s] = Select[
            DownValues[s],
            !FreeQ[First[#], Pattern | Blank | BlankSequence | BlankNullSequence] &
          ];
          Attributes[s] = attrs;
        ]
      ]
    ],
    cacheSyms
  ];
  If[!StringMatchQ[mode, "Quiet", IgnoreCase -> True],
    shortNames = Last /@ StringSplit[cacheSyms, "`"];
    Print["[GWPNumerical] Cleared caches: ", shortNames];
  ];
];


(* ::Section::Closed:: *)
(*Load Engine Extensions*)


LoadNumericalExtension[type_, context_] := Module[{engineParamSymbol, packagePath},
  engineParamSymbol = "MaddoxScience`GWPTools`GWPDeveloper`GWP" <> type <> "PARAM";
  If[NameQ[engineParamSymbol],
    packagePath = "MaddoxScience`GWPTools`" <> context <> "`";
    If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPNumerical] Loading Extension: ", packagePath]];
    Quiet[Needs[packagePath]];
  ]
];

LoadNumericalExtension["1D", "GWPNumerical1D"];
LoadNumericalExtension["SS1D", "GWPNumericalSS1D"];
LoadNumericalExtension["MC1D", "GWPNumericalMC1D"];
LoadNumericalExtension["RDM1D", "GWPNumericalRDM1D"];
LoadNumericalExtension["2D", "GWPNumerical2D"];


(* ::Section::Closed:: *)
(*End*)


(* --- End "MaddoxScience`GWPTools`GWPNumerical`Private`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPNumerical] End Private"]];
End[]

(* Hide internal code for all Developer functions from the ? menu *)
SetAttributes[Evaluate[Names["MaddoxScience`GWPTools`GWPNumerical`*"]], {ReadProtected}];

(* --- End "MaddoxScience`GWPTools`GWPNumerical`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPNumerical] EndPackage"]];
EndPackage[]

(* Scrub backend context to keep the user namespace pristine *)
$ContextPath = DeleteCases[$ContextPath, "MaddoxScience`GWPTools`GWPNumerical`"];
