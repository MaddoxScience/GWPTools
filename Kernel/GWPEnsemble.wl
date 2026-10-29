(* ::Package:: *)

(* ::Title:: *)
(*GWPEnsemble Package*)


(* ::Section:: *)
(*GWPTools Usage Declaration*)


BeginPackage["MaddoxScience`GWPTools`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] BeginPackage GWPTools"]];

GWPEnsemble::usage = "GWPEnsemble[GWPObject[], options] generates a structured phase-space or coordinate ensemble for the given wavepacket.";

(* --- Messages --- *)
GWPEnsemble::badtype = "Unrecognized GWPType: `1`. The corresponding hydrodynamics extension may not be loaded.";

If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] EndPackage GWPTools"]];
EndPackage[]


(* ::Section:: *)
(*GWPDeveloper Usage Declaration*)


Needs["MaddoxScience`GWPTools`GWPDeveloper`"];
BeginPackage["MaddoxScience`GWPTools`GWPDeveloper`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] BeginPackage GWPDeveloper"]];
Off[General::shdw];

GWP1DENSEMBLE::usage = "GWP1DENSEMBLE[method, opts][param] generates a structured phase-space or coordinate ensemble using the evaluated 1D parameter sequence.";
GWP1DENSEMBLE::badmethod = "Unrecognized sampling method `1` for the `2` engine.";

GWPSS1DENSEMBLE::usage = "GWPSS1DENSEMBLE[method, opts][param] generates a structured phase-space or coordinate ensemble using the evaluated SS1D parameter sequence.";
GWPSS1DENSEMBLE::badmethod = "Unrecognized sampling method `1` for the `2` engine.";

GWPMC1DENSEMBLE::usage = "GWPMC1DENSEMBLE[method, opts][param] generates a structured phase-space or coordinate ensemble using the evaluated MC1D parameter sequence.";
GWPMC1DENSEMBLE::badmethod = "Unrecognized sampling method `1` for the `2` engine.";

GWP2DENSEMBLE::usage = "GWP2DENSEMBLE[method, opts][param] generates a structured phase-space or coordinate ensemble using the evaluated 2D parameter sequence.";
GWP2DENSEMBLE::badmethod = "Unrecognized sampling method `1` for the `2` engine.";

If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] EndPackage GWPDeveloper"]];
Quiet[EndPackage[], General::shdw]
$ContextPath = DeleteCases[$ContextPath, "MaddoxScience`GWPTools`GWPDeveloper`"];


(* ::Section:: *)
(*BeginPackage*)


(* ========================================================================= *)
(* PACKAGE     : MaddoxScience`GWPTools`GWPEnsemble`                         *)
(* DESCRIPTION : Standalone ensemble generation for GWPTools frameworks.     *)
(* ========================================================================= *)
BeginPackage["MaddoxScience`GWPTools`GWPEnsemble`", {"MaddoxScience`GWPTools`"}]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] BeginPackage"]];
Begin["`Private`"]
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] Begin Private"]];

(* Load Engine Dependencies *)
Needs["MaddoxScience`GWPTools`GWPEngine1D`"];
Needs["MaddoxScience`GWPTools`GWPEngineSS1D`"];
Needs["MaddoxScience`GWPTools`GWPEngineMC1D`"];
Needs["MaddoxScience`GWPTools`GWPEngine2D`"];

(* Load Hydrodynamics Dependencies (required for density and trajectory fields) *)
Needs["MaddoxScience`GWPTools`GWPHydrodynamics1D`"];
Needs["MaddoxScience`GWPTools`GWPHydrodynamicsSS1D`"];
Needs["MaddoxScience`GWPTools`GWPHydrodynamicsMC1D`"];
Needs["MaddoxScience`GWPTools`GWPHydrodynamics2D`"];

Needs["MaddoxScience`GWPTools`GWPDeveloper`"];
Needs["MaddoxScience`GWPTools`GWPRegistry`"];


(* ::Section:: *)
(*GWPEnsemble Dispatcher*)


Options[GWPEnsemble] = Join[
  {
    "SamplingMethod" -> Automatic,
    "Time" -> 0,
    "Grid" -> Automatic,
    "Structure" -> "Staggered",
    "IncludeCore" -> True,
    WorkingPrecision -> Automatic
  },
  DeleteCases[Options[FindRoot], (WorkingPrecision -> _)]
];

(* Shared Precision & Parsing Helpers *)
resolveNumWP[wp_] := If[wp === Automatic, MachinePrecision, wp];
applyGridWP[expr_, wp_] := If[wp === Automatic, expr, N[expr, wp]];
parse1DGrid[grid_] := If[ListQ[grid], First[grid], If[IntegerQ[grid], grid, 100]];

GWPEnsemble[obj_MaddoxScience`GWPTools`GWPObject, opts:OptionsPattern[]] := Module[
  {type, method, t0, params, pot, evalParamsList},
  
  type = obj["GWPType"];
  method = OptionValue["SamplingMethod"];
  t0 = OptionValue["Time"];
  
  (* 1. Extract the initial parameter list and potential flag *)
  params = obj["Parameters"];
  pot = obj["Potential"];
  
  (* 2. Evaluate parameters at t0 if dynamic. 
        Apply (@@) maps the parameter list into the sequence macro.
        We wrap it back in a List to prevent Sequence leakage before routing. *)
  evalParamsList = If[pot === None || t0 == 0,
    params,
    {obj["PotentialModel"][t0] @@ params}
  ];
  
  (* 3. Dispatch to the backend extensions, injecting the evaluated sequence.
        Filter the options dynamically to match the backend engine's accepted options. *)
  Which[
    type === "1D",   
      GWP1DENSEMBLE[method, Sequence @@ FilterRules[{opts}, Options[GWP1DENSEMBLE]]][Sequence @@ evalParamsList],
    
    type === "SS1D", 
      GWPSS1DENSEMBLE[method, Sequence @@ FilterRules[{opts}, Options[GWPSS1DENSEMBLE]]][Sequence @@ evalParamsList],
    
    type === "MC1D", 
      GWPMC1DENSEMBLE[method, Sequence @@ FilterRules[{opts}, Options[GWPMC1DENSEMBLE]]][Sequence @@ evalParamsList],
    
    type === "2D",   
      GWP2DENSEMBLE[method, Sequence @@ FilterRules[{opts}, Options[GWP2DENSEMBLE]]][Sequence @@ evalParamsList],
    
    True, 
      Message[GWPEnsemble::badtype, type]; 
      $Failed
  ]
];


(* ::Section:: *)
(*GWP1DENSEMBLE*)


Options[GWP1DENSEMBLE] = {
  "SamplingMethod" -> Automatic,
  "Grid" -> Automatic,
  WorkingPrecision -> Automatic
};

parse1DBounds[grid_, rx_, ux_] := Which[
  ListQ[grid] && Length[grid] == 2 && StringQ[grid[[2]]] && grid[[2]] === "3Sigma", {rx - 3*ux, rx + 3*ux},
  ListQ[grid] && Length[grid] == 2, grid[[2]],
  True, {rx - 4*ux, rx + 4*ux}
];

(* --- Lexical Injection Wrapper for the Parameter Bus --- *)
With[{VAL = GWP1DVAL},

  (* --- Uniform Cumulative (Quantile) Sampling --- *)
  GWP1DENSEMBLE["UniformC" | Automatic, opts:OptionsPattern[]][GWP1DARG] := Module[
    {grid, wp, numPoints, cList},
    
    grid = OptionValue["Grid"];
    wp = OptionValue[WorkingPrecision];
    numPoints = parse1DGrid[grid];
    
    cList = If[ListQ[grid] && Length[grid] == 2 && ListQ[grid[[2]]],
      Subdivide[grid[[2, 1]], grid[[2, 2]], numPoints - 1],
      Range[1/(2 * numPoints), 1 - 1/(2 * numPoints), 1/numPoints]
    ];
    
    (* Listable mapping through the backend Inverse CDF *)
    applyGridWP[GWP1DXC[cList][VAL], wp]
  ];

  (* --- Uniform Spatial Sampling --- *)
  GWP1DENSEMBLE["UniformX", opts:OptionsPattern[]][GWP1DARG] := Module[
    {grid, wp, numPoints, ux, bounds},
    
    grid = OptionValue["Grid"];
    wp = OptionValue[WorkingPrecision];
    numPoints = parse1DGrid[grid];
    ux = 1 / (2 * Sqrt[RA]);
    bounds = parse1DBounds[grid, RX, ux];
    
    applyGridWP[Subdivide[bounds[[1]], bounds[[2]], numPoints - 1], wp]
  ];

  (* --- Random Spatial Sampling (Analytic Normal Distribution) --- *)
  GWP1DENSEMBLE["Random", opts:OptionsPattern[]][GWP1DARG] := Module[
    {grid, wp, numPoints, ux},
    
    grid = OptionValue["Grid"];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    numPoints = parse1DGrid[grid];
    ux = 1 / (2 * Sqrt[RA]);
    
    Sort[RandomVariate[NormalDistribution[RX, ux], numPoints, WorkingPrecision -> wp]]
  ];

  (* --- Random Density Sampling (Numerical Probability Distribution) --- *)
  GWP1DENSEMBLE["RandomDensity", opts:OptionsPattern[]][GWP1DARG] := Module[
    {grid, wp, numPoints, ux, bounds, dist, x},
    
    grid = OptionValue["Grid"];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    numPoints = parse1DGrid[grid];
    ux = 1 / (2 * Sqrt[RA]);
    bounds = parse1DBounds[grid, RX, ux];
    
    (* Safe injection of VAL into the density backend *)
    dist = ProbabilityDistribution[
      GWP1DRHOX[x][VAL], 
      {x, bounds[[1]], bounds[[2]]}, 
      Method -> "Normalize"
    ];
    
    Sort[RandomVariate[dist, numPoints, WorkingPrecision -> wp]]
  ];

  (* --- Fallback Error Routing --- *)
  GWP1DENSEMBLE[method_, opts:OptionsPattern[]][GWP1DARG] := (
    Message[GWP1DENSEMBLE::badmethod, method, "1D"]; 
    $Failed
  );

]; (* End With *)


(* ::Section:: *)
(*GWPSS1DEnsemble*)


Options[GWPSS1DENSEMBLE] = Join[
  {"SamplingMethod" -> Automatic, "Grid" -> Automatic},
  Options[FindRoot]
];

(* Shared SS1D Parsing Helpers *)
parseSS1DBounds[valList_, grid_] := Module[
  {w1, w2, rx1, rx2, ux1, ux2, bx},
  
  (* valList is the unpacked parameter sequence {VAL} *)
  w1 = valList[[1]]; 
  w2 = valList[[2]];
  
  rx1 = w1[[3]]; ux1 = 1 / Sqrt[4 * w1[[1]]];
  rx2 = w2[[3]]; ux2 = 1 / Sqrt[4 * w2[[1]]];
  
  (* Generate strictly physical bounds *)
  bx = {Min[rx1 - 4*ux1, rx2 - 4*ux2], Max[rx1 + 4*ux1, rx2 + 4*ux2]};
  If[ListQ[grid] && Length[grid] == 2 && ListQ[grid[[2]]], grid[[2]], bx]
];

(* --- Lexical Injection Wrapper for the SS1D Parameter Bus --- *)
With[{VAL = GWPSS1DVAL},

  (* --- Uniform Cartesian Spatial Sampling --- *)
  GWPSS1DENSEMBLE["UniformX" | "Cartesian" | Automatic, opts:OptionsPattern[]][GWPSS1DARG] := Module[
    {grid, wp, numPoints, boundsX},
    
    grid = OptionValue["Grid"];
    wp = OptionValue[WorkingPrecision];
    numPoints = parse1DGrid[grid];
    boundsX = parseSS1DBounds[{VAL}, grid];
    
    applyGridWP[Subdivide[boundsX[[1]], boundsX[[2]], numPoints - 1], wp]
  ];

  (* --- Random Density Sampling (Numerical Rejection Sampling) --- *)
  GWPSS1DENSEMBLE["RandomDensity", opts:OptionsPattern[]][GWPSS1DARG] := Module[
    {grid, wp, numPoints, boundsX, w1, w2, maxRho, pts = {}, nNeeded, cX, cZ, densities, selector},
    
    grid = OptionValue["Grid"];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    numPoints = parse1DGrid[grid];
    boundsX = N[parseSS1DBounds[{VAL}, grid], wp];
    
    w1 = {VAL}[[1]]; 
    w2 = {VAL}[[2]];
    
    (* Explicitly pass '0' for the derivative argument and use exact 3/2 to preserve arbitrary precision *)
    maxRho = N[
      (3/2) * Max[
        GWPSS1DRHOX[0][w1[[3]]][VAL],
        GWPSS1DRHOX[0][w2[[3]]][VAL]
      ],
      wp
    ];
    
    nNeeded = numPoints;
    
    While[Length[pts] < numPoints,
      cX = RandomReal[boundsX, nNeeded, WorkingPrecision -> wp];
      cZ = RandomReal[{0, maxRho}, nNeeded, WorkingPrecision -> wp];
      
      (* Explicitly pass '0' for the derivative argument *)
      densities = GWPSS1DRHOX[0][#][VAL] & /@ cX; 
      selector = Thread[cZ < densities];
      
      pts = Join[pts, Pick[cX, selector]];
      nNeeded = numPoints - Length[pts];
    ];
    
    Sort[Take[pts, numPoints]]
  ];

  (* --- Inverse Cumulative Root-Finding --- *)
  GWPSS1DENSEMBLE["InverseC", opts:OptionsPattern[]][GWPSS1DARG] := Module[
    {grid, wp, numPoints, boundsX, cGrid, frOpts, x, firstRoot},
    
    grid = OptionValue["Grid"];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    numPoints = parse1DGrid[grid];
    boundsX = parseSS1DBounds[{VAL}, grid];
    
    cGrid = If[ListQ[grid] && Length[grid] == 2 && ListQ[grid[[2]]],
      Subdivide[grid[[2, 1]], grid[[2, 2]], numPoints - 1],
      Range[1/(2*numPoints), 1 - 1/(2*numPoints), 1/numPoints]
    ];
    
    (* Filter only valid FindRoot options and prepend our resolved WorkingPrecision *)
    frOpts = Join[
      {WorkingPrecision -> wp},
      DeleteCases[FilterRules[{opts}, Options[FindRoot]], (WorkingPrecision -> _)]
    ];
    
    (* Explicitly pass '0' to the Jacobian density function *)
    firstRoot = x /. Quiet @ FindRoot[
      GWPSS1DCX[x][VAL] - cGrid[[1]] == 0,
      {x, boundsX[[1]], boundsX[[1]] - 5, boundsX[[2]] + 5},
      Jacobian :> {{GWPSS1DRHOX[0][x][VAL]}},
      Evaluate[frOpts]
    ];
    
    FoldList[
      Function[{prevX, currC},
        x /. Quiet @ FindRoot[
          GWPSS1DCX[x][VAL] - currC == 0,
          {x, prevX, boundsX[[1]] - 5, boundsX[[2]] + 5},
          Jacobian :> {{GWPSS1DRHOX[0][x][VAL]}},
          Evaluate[Sequence @@ frOpts]
        ]
      ],
      firstRoot,
      Rest[cGrid]
    ]
  ];

  (* --- Fallback Error Routing --- *)
  GWPSS1DENSEMBLE[method_, opts:OptionsPattern[]][GWPSS1DARG] := (
    Message[GWPSS1DENSEMBLE::badmethod, method, "SS1D"]; 
    $Failed
  );

]; (* End With *)


(* ::Section:: *)
(*GWPMC1DEnsemble*)


Options[GWPMC1DENSEMBLE] = Join[
  {"SamplingMethod" -> Automatic, "Grid" -> Automatic},
  Options[FindRoot]
];

parseMC1DBounds[valList_, grid_] := Module[
  {nStates, packed, minX, maxX, rx, ux, param, i},
  
  nStates = valList[[1]]; 
  packed = valList[[2]];
  
  minX = Infinity;
  maxX = -Infinity;
  
  For[i = 1, i <= nStates, i++,
    param = packed[[MC1DIndex[i, i, nStates]]];
    rx = param[[3]];
    ux = 1 / Sqrt[4 * param[[1]]];
    minX = Min[minX, rx - 4*ux];
    maxX = Max[maxX, rx + 4*ux];
  ];
  
  If[ListQ[grid] && Length[grid] == 2 && ListQ[grid[[2]]], grid[[2]], {minX, maxX}]
];

With[{VAL = GWPMC1DVAL},

  (* --- Uniform Cartesian Spatial Sampling --- *)
  GWPMC1DENSEMBLE["UniformX" | "Cartesian" | Automatic, opts:OptionsPattern[]][GWPMC1DARG] := Module[
    {grid, wp, numPoints, boundsX},
    
    grid = OptionValue["Grid"];
    wp = OptionValue[WorkingPrecision];
    numPoints = parse1DGrid[grid];
    boundsX = parseMC1DBounds[{VAL}, grid];
    
    applyGridWP[Subdivide[boundsX[[1]], boundsX[[2]], numPoints - 1], wp]
  ];

  (* --- Random Density Sampling (Numerical Rejection Sampling) --- *)
  GWPMC1DENSEMBLE["RandomDensity", opts:OptionsPattern[]][GWPMC1DARG] := Module[
    {grid, wp, numPoints, boundsX, maxRho, pts = {}, nNeeded, cX, cZ, densities, selector, param, i},
    
    grid = OptionValue["Grid"];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    numPoints = parse1DGrid[grid];
    boundsX = N[parseMC1DBounds[{VAL}, grid], wp];
    
    (* Use exact 3/2 instead of 1.5 to preserve arbitrary precision *)
    maxRho = N[
      (3/2) * Max[
        Table[
          param = PACKEDPARAMS[[MC1DIndex[i, i, NSTATES]]];
          GWPMC1DRHOX[0][param[[3]]][VAL],
          {i, 1, NSTATES}
        ]
      ],
      wp
    ];
    
    nNeeded = numPoints;
    
    While[Length[pts] < numPoints,
      cX = RandomReal[boundsX, nNeeded, WorkingPrecision -> wp];
      cZ = RandomReal[{0, maxRho}, nNeeded, WorkingPrecision -> wp];
      
      densities = GWPMC1DRHOX[0][#][VAL] & /@ cX; 
      selector = Thread[cZ < densities];
      
      pts = Join[pts, Pick[cX, selector]];
      nNeeded = numPoints - Length[pts];
    ];
    
    Sort[Take[pts, numPoints]]
  ];

  (* --- Inverse Cumulative Root-Finding --- *)
  GWPMC1DENSEMBLE["InverseC", opts:OptionsPattern[]][GWPMC1DARG] := Module[
    {grid, wp, numPoints, boundsX, cGrid, frOpts, x, firstRoot},
    
    grid = OptionValue["Grid"];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    numPoints = parse1DGrid[grid];
    boundsX = parseMC1DBounds[{VAL}, grid];
    
    cGrid = If[ListQ[grid] && Length[grid] == 2 && ListQ[grid[[2]]],
      Subdivide[grid[[2, 1]], grid[[2, 2]], numPoints - 1],
      Range[1/(2*numPoints), 1 - 1/(2*numPoints), 1/numPoints]
    ];
    
    (* Filter only valid FindRoot options and prepend our resolved WorkingPrecision *)
    frOpts = Join[
      {WorkingPrecision -> wp},
      DeleteCases[FilterRules[{opts}, Options[FindRoot]], (WorkingPrecision -> _)]
    ];
    
    firstRoot = x /. Quiet @ FindRoot[
      GWPMC1DCX[x][VAL] - cGrid[[1]] == 0,
      {x, boundsX[[1]], boundsX[[1]] - 5, boundsX[[2]] + 5},
      Jacobian :> {{GWPMC1DRHOX[0][x][VAL]}},
      Evaluate[frOpts]
    ];
    
    FoldList[
      Function[{prevX, currC},
        x /. Quiet @ FindRoot[
          GWPMC1DCX[x][VAL] - currC == 0,
          {x, prevX, boundsX[[1]] - 5, boundsX[[2]] + 5},
          Jacobian :> {{GWPMC1DRHOX[0][x][VAL]}},
          Evaluate[Sequence @@ frOpts]
        ]
      ],
      firstRoot,
      Rest[cGrid]
    ]
  ];

  (* --- Fallback Error Routing --- *)
  GWPMC1DENSEMBLE[method_, opts:OptionsPattern[]][GWPMC1DARG] := (
    Message[GWPMC1DENSEMBLE::badmethod, method, "MC1D"]; 
    $Failed
  );

];


(* ::Section:: *)
(*GWP2DENSEMBLE*)


Options[GWP2DENSEMBLE] = {
  "SamplingMethod" -> Automatic,
  "Grid" -> Automatic,
  "Structure" -> "Staggered",
  "IncludeCore" -> True,
  WorkingPrecision -> Automatic
};

(* Shared 2D Parsing Helpers *)
parse2DGrid[grid_] := Which[
  ListQ[grid] && Length[grid] >= 2 && IntegerQ[grid[[1]]], {grid[[1]], grid[[2]], grid[[1]] * grid[[2]]},
  IntegerQ[grid], {Round[Sqrt[grid]], Round[Sqrt[grid]], grid},
  True, {20, 20, 400}
];

parse2DBounds[grid_, mu_, sigma_] := Module[
  {ux, uy, bx, by},
  ux = Sqrt[sigma[[1, 1]]];
  uy = Sqrt[sigma[[2, 2]]];
  bx = {mu[[1]] - 4*ux, mu[[1]] + 4*ux};
  by = {mu[[2]] - 4*uy, mu[[2]] + 4*uy};
  
  If[ListQ[grid] && Length[grid] == 3 && ListQ[grid[[3]]],
    bx = grid[[3, 1]]; by = grid[[3, 2]]
  ];
  {bx, by}
];

(* --- Lexical Injection Wrapper for the 2D Parameter Bus --- *)
With[{VAL = GWP2DVAL},

  (* --- Uniform Cartesian Spatial Sampling --- *)
  GWP2DENSEMBLE["Cartesian" | "UniformX" | Automatic, opts:OptionsPattern[]][GWP2DARG] := Module[
    {g, wp, mu, sigma, bx, by, xGrid, yGrid},
    
    g = parse2DGrid[OptionValue["Grid"]];
    wp = OptionValue[WorkingPrecision];
    mu = GWP2DRMAT[VAL];
    sigma = GWP2DCOVMAT[VAL];
    {bx, by} = parse2DBounds[OptionValue["Grid"], mu, sigma];
    
    xGrid = Subdivide[bx[[1]], bx[[2]], g[[1]] - 1];
    yGrid = Subdivide[by[[1]], by[[2]], g[[2]] - 1];
    
    applyGridWP[Flatten[Outer[List, xGrid, yGrid], 1], wp]
  ];

  (* --- Affine Cumulative (Quantile) Sampling --- *)
  GWP2DENSEMBLE["AffineC", opts:OptionsPattern[]][GWP2DARG] := Module[
    {g, wp, mu, sigma, cGridX, cGridY, xGrid, yGrid, zMesh, L},
    
    g = parse2DGrid[OptionValue["Grid"]];
    wp = OptionValue[WorkingPrecision];
    mu = GWP2DRMAT[VAL];
    sigma = GWP2DCOVMAT[VAL];
    
    cGridX = Range[1/(2 * g[[1]]), 1 - 1/(2 * g[[1]]), 1/g[[1]]];
    cGridY = Range[1/(2 * g[[2]]), 1 - 1/(2 * g[[2]]), 1/g[[2]]];
    
    xGrid = Sqrt[2] * InverseErf[2 * cGridX - 1];
    yGrid = Sqrt[2] * InverseErf[2 * cGridY - 1];
    zMesh = Flatten[Outer[List, xGrid, yGrid], 1];
    
    L = Transpose[CholeskyDecomposition[sigma]];
    applyGridWP[Map[(mu + L . #) &, zMesh], wp]
  ];

  (* --- Random Spatial Sampling (Analytic Normal Distribution) --- *)
  GWP2DENSEMBLE["Random", opts:OptionsPattern[]][GWP2DARG] := Module[
    {g, wp, mu, sigma},
    
    g = parse2DGrid[OptionValue["Grid"]];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    mu = GWP2DRMAT[VAL];
    sigma = GWP2DCOVMAT[VAL];
    
    RandomVariate[MultinormalDistribution[mu, sigma], g[[3]], WorkingPrecision -> wp]
  ];

  (* --- Random Density Sampling (Numerical Rejection Sampling) --- *)
  GWP2DENSEMBLE["RandomDensity", opts:OptionsPattern[]][GWP2DARG] := Module[
    {g, wp, mu, sigma, bx, by, maxRho, pts = {}, nNeeded, cX, cY, cZ, densities, selector},
    
    g = parse2DGrid[OptionValue["Grid"]];
    wp = resolveNumWP[OptionValue[WorkingPrecision]];
    mu = GWP2DRMAT[VAL];
    sigma = GWP2DCOVMAT[VAL];
    {bx, by} = N[parse2DBounds[OptionValue["Grid"], mu, sigma], wp];
    
    (* Evaluate max density safely at the spatial center at precision wp *)
    maxRho = N[GWP2DRHOX[mu[[1]], mu[[2]]][VAL], wp]; 
    nNeeded = g[[3]];
    
    While[Length[pts] < g[[3]],
      cX = RandomReal[bx, nNeeded, WorkingPrecision -> wp];
      cY = RandomReal[by, nNeeded, WorkingPrecision -> wp];
      cZ = RandomReal[{0, maxRho}, nNeeded, WorkingPrecision -> wp];
      
      (* Map the backend density formula cleanly over the coordinate arrays *)
      densities = MapThread[GWP2DRHOX[#1, #2][VAL] &, {cX, cY}];
      selector = Thread[cZ < densities];
      
      pts = Join[pts, Pick[Transpose[{cX, cY}], selector]];
      nNeeded = g[[3]] - Length[pts];
    ];
    
    Take[pts, g[[3]]]
  ];

  (* --- Polar Cumulative (Quantile) Sampling --- *)
  GWP2DENSEMBLE["PolarC", opts:OptionsPattern[]][GWP2DARG] := Module[
    {g, wp, structure, includeCore, crList, paramGrid, offset},
    
    g = parse2DGrid[OptionValue["Grid"]];
    wp = OptionValue[WorkingPrecision];
    structure = OptionValue["Structure"];
    includeCore = OptionValue["IncludeCore"];
    
    crList = Range[1/(g[[1]] + 1), g[[1]]/(g[[1]] + 1), 1/(g[[1]] + 1)];
    
    paramGrid = Flatten[
      Table[
        offset = If[structure === "Staggered", Mod[i, 2]/(2*g[[2]]), 0];
        Table[{crList[[i]], ct + offset}, {ct, 0, 1 - 1/g[[2]], 1/g[[2]]}],
        {i, 1, Length[crList]}
      ], 1
    ];
    
    If[includeCore, PrependTo[paramGrid, {0, 0}]];
    
    (* Apply the 2D backend Inverse CDF to the generated phase points *)
    applyGridWP[Map[GWP2DXC[#[[1]], #[[2]]][VAL] &, paramGrid], wp]
  ];

  (* --- Fallback Error Routing --- *)
  GWP2DENSEMBLE[method_, opts:OptionsPattern[]][GWP2DARG] := (
    Message[GWP2DENSEMBLE::badmethod, method, "2D"]; 
    $Failed
  );

]; (* End With *)


(* ::Section::Closed:: *)
(*End*)


(* --- End "MaddoxScience`GWPTools`GWPEnsemble`Private`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] End Private"]];
End[]

(* Hide internal code for all Developer functions from the ? menu *)
SetAttributes[Evaluate[Names["MaddoxScience`GWPTools`GWPEnsemble`*"]], {ReadProtected}];

(* --- End "MaddoxScience`GWPTools`GWPEnsemble`" --- *)
If[TrueQ[MaddoxScience`GWPTools`$GWPDebug], Print["[GWPEnsemble] EndPackage"]];
EndPackage[]

(* Scrub backend context to keep the user namespace pristine *)
$ContextPath = DeleteCases[$ContextPath, "MaddoxScience`GWPTools`GWPEnsemble`"];
