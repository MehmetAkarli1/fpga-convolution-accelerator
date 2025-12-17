# FPGA Convolution Accelerator (Course Project)
This repository contains my work from a university FPGA/SoC course project focused on **RTL design, verification, and system integration** of a streaming convolution accelerator.

The convolution pipeline itself was provided as part of the course framework. My work focused on **buffering, control logic, memory-mapped interfaces, verification, and integration** around that pipeline.

---

## Project Overview
The project implements a **1D convolution accelerator** targeting FPGA-based signal processing.  
The design operates in a streaming fashion and interfaces with external memory using DMA-style read/write blocks.

A memory-mapped interface is used to configure and control the accelerator from software.

The top-level design (`user_app`) integrates:

- Signal and kernel buffering
- Memory-mapped control registers
- A provided multiply–add convolution pipeline
- Delay and clipping logic
- External DRAM read/write interfaces

The complete system was verified through RTL simulation using multiple dedicated testbenches.

---

## What I Implemented

### RTL Design
- Signal buffer with **sliding window support**
- Kernel buffer for coefficient loading
- Clipping / saturation logic for post-processing convolution output
- Delay logic to align data and control paths
- Top-level control logic (`user_app`) connecting buffers, pipeline, memory map, and DMA interfaces
- Valid/ready handshaking and control between blocks
- Reset, clear, and start/stop sequencing

### Verification
- Wrote VHDL testbenches for:
  - Signal buffer
  - Kernel buffer
  - Top-level user application
- Verified buffering behavior, data ordering, and pipeline alignment through simulation
- Used waveform inspection to validate timing and control correctness

### Integration
- Integrated provided IP blocks with custom RTL
- Connected memory-mapped registers to hardware behavior
- Managed control flow between DMA, buffers, and pipeline

---

## Provided Components
The following modules were **provided by the course** and were not modified by me:

- Convolution pipeline (multiply–add tree)
- DMA / DRAM read interface
- DMA / DRAM write interface
- Memory-map register block
- Wrapper-level infrastructure

These are placed under the `provided/` directory for clarity.

---

## Repository Structure
├── user_app/ # Top-level wrapper and control logic
├── buffers/ # Signal buffer, kernel buffer, delay, clipping
├── testbench/ # Testbenches for individual blocks and top level
├── provided/ # Modules provided by the course (unchanged)
└── docs/ # Original project description

---

## Tools & Technologies
- VHDL
- RTL design, simulation, synthesis, and implementation using Xilinx Vivado
- FPGA-oriented control and buffering design
- Memory-mapped interfaces
- Streaming data paths and valid/ready handshaking

---

## Notes
This project was completed as part of a university FPGA/SoC course.

The project emphasized designing, integrating, and verifying custom RTL blocks around an existing convolution pipeline, with a strong focus on control logic, buffering, dataflow correctness, and system-level behavior.

