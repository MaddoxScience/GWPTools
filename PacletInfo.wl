(* ::Package:: *)

PacletObject[
    <|
        "Name" -> "MaddoxScience/GWPTools",
        "Version" -> "1.0.1",
        "WolframVersion" -> "14.+",
        "PublisherID" -> "MaddoxScience",
        "Description" -> "An extensible symbolic-numeric framework for exact time-dependent Gaussian wave packet dynamics.",
        "Creator" -> "Jeremy B. Maddox",
        "URL" -> "https://github.com/MaddoxScience/GWPTools",
        "License" -> "MIT",
        "Keywords" -> {"Quantum Mechanics", "Wave Packets", "Hydrodynamics", "Phase Space"},
        "Extensions" ->
            {
                {
                    "Kernel",
                    "Root" -> "Kernel",
                    "Context" -> {
                        {"MaddoxScience`GWPTools`", "GWPTools.wl"},
                        {"MaddoxScience`GWPTools`GWPDeveloper`", "GWPDeveloper.wl"},
                        {"MaddoxScience`GWPTools`GWPDiagnostics`", "GWPDiagnostics.wl"},
                        {"MaddoxScience`GWPTools`GWPDispatcher`", "GWPDispatcher.wl"},
                        {"MaddoxScience`GWPTools`GWPEngine1D`", "GWPEngine1D.wl"},
                        {"MaddoxScience`GWPTools`GWPEngine2D`", "GWPEngine2D.wl"},
                        {"MaddoxScience`GWPTools`GWPEngineCT`", "GWPEngineCT.wl"},
                        {"MaddoxScience`GWPTools`GWPEngineMC1D`", "GWPEngineMC1D.wl"},
                        {"MaddoxScience`GWPTools`GWPEngineRDM1D`", "GWPEngineRDM1D.wl"},
                        {"MaddoxScience`GWPTools`GWPEngineSS1D`", "GWPEngineSS1D.wl"},
                        {"MaddoxScience`GWPTools`GWPEnsemble`", "GWPEnsemble.wl"},
                        {"MaddoxScience`GWPTools`GWPHydrodynamics`", "GWPHydrodynamics.wl"},
                        {"MaddoxScience`GWPTools`GWPHydrodynamics1D`", "GWPHydrodynamics1D.wl"},
                        {"MaddoxScience`GWPTools`GWPHydrodynamics2D`", "GWPHydrodynamics2D.wl"},
                        {"MaddoxScience`GWPTools`GWPHydrodynamicsMC1D`", "GWPHydrodynamicsMC1D.wl"},
                        {"MaddoxScience`GWPTools`GWPHydrodynamicsSS1D`", "GWPHydrodynamicsSS1D.wl"},
                        {"MaddoxScience`GWPTools`GWPNumerical`", "GWPNumerical.wl"},
                        {"MaddoxScience`GWPTools`GWPNumerical1D`", "GWPNumerical1D.wl"},
                        {"MaddoxScience`GWPTools`GWPNumerical2D`", "GWPNumerical2D.wl"},
                        {"MaddoxScience`GWPTools`GWPNumericalMC1D`", "GWPNumericalMC1D.wl"},
                        {"MaddoxScience`GWPTools`GWPNumericalRDM1D`", "GWPNumericalRDM1D.wl"},
                        {"MaddoxScience`GWPTools`GWPNumericalSS1D`", "GWPNumericalSS1D.wl"},
                        {"MaddoxScience`GWPTools`GWPPhaseSpace`", "GWPPhaseSpace.wl"},
                        {"MaddoxScience`GWPTools`GWPPhaseSpace1D`", "GWPPhaseSpace1D.wl"},
                        {"MaddoxScience`GWPTools`GWPRegistry`", "GWPRegistry.wl"}
                    },
                    "Symbols" -> {
                        "MaddoxScience`GWPTools`GWP",
                        "MaddoxScience`GWPTools`GWPEnsemble",
                        "MaddoxScience`GWPTools`GWPObject"
                    }
                },
                {
                    "Documentation",
                    "Language" -> "English",
                    "MainPage" -> "Guides/GWPTools"
                },
                {
                    "Asset",
                    "Assets" -> {
                        {"Tests", "Tests"},
                        {"License", "LICENSE"},
                        {"ReadMe", "README.md"}
                    }
                }
            }
    |>
]