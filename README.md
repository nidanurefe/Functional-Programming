# Functional Haskell Simulations

This repository contains Haskell projects, showcasing the use of functional programming in solving computational problems related to geometry, social simulation, cellular automata, and graph theory.

---

## 1️⃣ Hilbert STL Generator

Generates a 2D Hilbert curve of a given order and exports it as a `.stl` file suitable for 3D printing or mesh visualization. It uses geometric transformations and triangle-based tessellation to represent the curve as a 3D object.

### Features:
- Functional implementation of recursive Hilbert curve generation.
- Automatic rectangle-to-triangle mesh conversion.
- STL file export support.

---

## 2️⃣ Schelling Segregation Simulator 

A cellular automaton simulation based on Thomas Schelling’s model of social segregation. This tool reads an input grid and simulates agent movement based on similarity thresholds in their neighborhood.

### Features:
- Adjustable neighborhood size and happiness thresholds.
- Efficient reallocation of agents using vectorized operations.
- Input/output support for CSV-style plain-text files.

---

## 3️⃣ Freebase MID Graph BFS 

A command-line graph utility that performs **Breadth-First Search** (BFS) between two MIDs in a graph derived from Freebase data. Useful for exploring semantic connections in large knowledge graphs.

### Features:
- TSV-based graph and MID-to-name parsing.
- Shortest path discovery between MIDs.
- Colorized console output for error handling and result display.

---

## 4️⃣ Conway’s Game of Life – Oscillator Simulation

A visual and animated simulation of Conway’s Game of Life using five classical **oscillator patterns**: Blinker, Toad, Beacon, Pulsar, and Pentadecathlon. Each pattern evolves over generations based on Conway’s rules.

### Features:
- Functional implementation of the Game of Life rules.
- Visual output using the JuicyPixels library (`.png` format).
- Animated `.gif` output generated for each pattern.
- Demonstrates periodic behavior of oscillators with clean, centralized rendering.

> All output images and animations are included in the project folders under `frames*/`.

---

## 🧩 Requirements

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- Cabal or Stack for dependency management (not required for these projects, but useful for expansion)

---

## 🚀 Getting Started

Each project contains a `main.hs` or equivalent file. You can compile and run each independently. Example:

```bash
ghc main.hs
./main
