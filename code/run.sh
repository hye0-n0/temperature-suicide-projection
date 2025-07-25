#!/usr/bin/env bash
set -ex

# This is the master script for the capsule. When you click "Reproducible Run", the code in this file will execute.
bash run.sh "$@"

Stop on first error
set -e
 
echo ">>> Calibration of modeled temperauter..."
Rscript code/0.Calibration.R

echo ">>> Running first-stage analysis..."
Rscript code/1.Firststage.R

echo ">>> Running second-stage analysis..."
Rscript code/2.Secondstage.R

echo ">>> Gernerating modeled suicide mortality..."
Rscript code/3.ModeledSuicide.R

echo ">>> Running projection and uncertainty quantification..."
Rscript code/4.ProjectionANAF.R

echo ">>> All scripts completed."

