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
                        "MaddoxScience`GWPTools`",
                        "MaddoxScience`GWPTools`GWPDeveloper`",
                        "MaddoxScience`GWPTools`GWPDiagnostics`",
                        "MaddoxScience`GWPTools`GWPEngine1D`",
                        "MaddoxScience`GWPTools`GWPEngine2D`",
                        "MaddoxScience`GWPTools`GWPEngineCT`",
                        "MaddoxScience`GWPTools`GWPEngineMC1D`",
                        "MaddoxScience`GWPTools`GWPEngineRDM1D`",
                        "MaddoxScience`GWPTools`GWPEngineSS1D`",
                        "MaddoxScience`GWPTools`GWPEnsemble`",
                        "MaddoxScience`GWPTools`GWPHydrodynamics`",
                        "MaddoxScience`GWPTools`GWPNumerical`",
                        "MaddoxScience`GWPTools`GWPPhaseSpace`"
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