#!/bin/bash
set -e

cd ./src; sbcl --script ./main-latex.cl \
| LC_ALL=C sed -e "s/^/$/g" \
| LC_ALL=C sed -e "s/$/$ @/g" \
| LC_ALL=C sed -e "s/(/@\\\\allowbreak(/g" \
| LC_ALL=C sed -e "s/)/@\\\\allowbreak)/g" \
| LC_ALL=C sed -e "s/\./.\\\\allowbreak /g" \
| LC_ALL=C sed -e "s/α/@\\\\allowbreak \\\\alpha /g" \
| LC_ALL=C sed -e "s/β/@\\\\allowbreak \\\\beta /g" \
| LC_ALL=C sed -e "s/γ/@\\\\allowbreak \\\\gamma /g" \
| LC_ALL=C sed -e "s/δ/@\\\\allowbreak \\\\delta /g" \
| LC_ALL=C sed -e "s/ε/@\\\\allowbreak \\\\varepsilon /g" \
| LC_ALL=C sed -e "s/ζ/@\\\\allowbreak \\\\zeta /g" \
| LC_ALL=C sed -e "s/η/@\\\\allowbreak \\\\eta /g" \
| LC_ALL=C sed -e "s/θ/@\\\\allowbreak \\\\theta /g" \
| LC_ALL=C sed -e "s/κ/@\\\\allowbreak \\\\kappa /g" \
| LC_ALL=C sed -e "s/μ/@\\\\allowbreak \\\\mu /g" \
| LC_ALL=C sed -e "s/ν/@\\\\allowbreak \\\\nu /g" \
| LC_ALL=C sed -e "s/ξ/@\\\\allowbreak \\\\xi /g" \
| LC_ALL=C sed -e "s/π/@\\\\allowbreak \\\\pi /g" \
| LC_ALL=C sed -e "s/ρ/@\\\\allowbreak \\\\rho /g" \
| LC_ALL=C sed -e "s/σ/@\\\\allowbreak \\\\sigma /g" \
| LC_ALL=C sed -e "s/τ/@\\\\allowbreak \\\\tau /g" \
| LC_ALL=C sed -e "s/υ/@\\\\allowbreak \\\\upsilon /g" \
| LC_ALL=C sed -e "s/φ/@\\\\allowbreak \\\\varphi /g" \
| LC_ALL=C sed -e "s/χ/@\\\\allowbreak \\\\chi /g" \
| LC_ALL=C sed -e "s/ψ/@\\\\allowbreak \\\\psi /g" \
| LC_ALL=C sed -e "s/ω/@\\\\allowbreak \\\\omega /g" \
| LC_ALL=C sed -e "s/λ/@\\\\allowbreak \\\\lambda /g" \
| LC_ALL=C tr "@" "\n" \
> lambdalisp.tex
