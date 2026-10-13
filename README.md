# GWPTools

[![Paclet Repository](https://img.shields.io/badge/Wolfram_Paclet-MaddoxScience%2FGWPTools-blue)](https://paclets.com)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

**GWPTools** is an extensible symbolic-numeric Wolfram Language framework for exact time-dependent Gaussian Wave Packet (GWP) dynamics. It provides an object-oriented, dispatcher-driven architecture for modeling quantum wavepacket propagation, hydrodynamics, phase-space representations, and structured ensemble generation across 1D and multi-dimensional systems.

---

## Key Capabilities

* **Master Object Interface (`GWP`, `GWPObject`):** High-level symbolic constructor and container for managing wavepacket states, potential models, and parameters.
* **Exact Math Engines:** Built-in exact propagation engines including `GWPEngine1D`, `GWPEngine2D`, `GWPEngineSS1D`, `GWPEngineMC1D`, and `GWPEngineRDM1D`.
* **Hydrodynamics & Trajectories (`GWPHydrodynamics`):** Tools for computing Bohmian trajectories, quantum potential fields, and velocity fields.
* **Phase-Space Dynamics (`GWPPhaseSpace`):** Exact Wigner and phase-space representation tools.
* **Ensemble Generation (`GWPEnsemble`):** Structured sampling methods for phase-space, coordinate, and density grid generation.
* **Developer API (`GWPDeveloper`):** Direct low-level access to parameter transformations, Jacobian arrays, and backend numerical routines.

---

## Installation

### From Wolfram Paclet File
Download the latest `.paclet` release from the [Releases](https://github.com/MaddoxScience/GWPTools/releases) page and install it directly in Mathematica:

```wolfram
PacletInstall["path/to/MaddoxScience-GWPTools-1.0.1.paclet"]

```

### Quick Start

```wolfram
Needs["MaddoxScience`GWPTools`"]

(* Define a default 1D Gaussian wavepacket *)
psi = GWP[]

(* Generate a structured spatial ensemble *)
ensemble = GWPEnsemble[psi, "SamplingMethod" -> "UniformX"]



---

## Academic Citation

If you use **GWPTools** in your research, please cite our corresponding paper in *Computer Physics Communications*:

```bibtex
@article{Maddox2026GWPTools,
  title   = {GWPTools: An Extensible Symbolic-Numeric Framework for Exact Time-Dependent Gaussian Wave Packet Dynamics},
  author  = {Maddox, Jeremy B. and Giliani, Ismail and Heck, Carter M. and Mattingly, Connor S.},
  journal = {Computer Physics Communications},
  note    = {Submitted / In Review},
  year    = {2026}
}

```

---

## Authors & Publisher

* **Authors:** Jeremy B. Maddox
* **Publisher:** Western Kentucky University


* **Organization:** MaddoxScience


* **License:** [MIT License](https://www.google.com/search?q=LICENSE)


```
