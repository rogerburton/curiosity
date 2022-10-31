#! /usr/bin/env bash

# Run our executable in various ways, collecting coverage information. The
# result is saved as HTML pages in the coverage/ directory.
# (In other words, this is not related to tests, but to invokation examples.)

# Compile the programs, with HPC enabled. This generates a .hpc/ directory
# containing .mix files. If the .hi and .o files are not deleted first,
# the corresponding .mix file will be missing, causing errors later when
# invoking `hpc report` or `hpc markup`.
echo "Building cty..."
rm -rf dist-newstyle
cabal build --enable-coverage

# With the HPCTIXDIR environment variable set, an HPC-instrumented process
# will use its own .tix file.
# `hpc combine` can then be used to aggregate the .tix files in a single
# new .tix file, used as usual with `hpc report`, `hpc markup` or
# `covered markup`.
# That directory is automatically created by the instrumented executable.
export HPCTIXDIR=tix

# Run the code in various ways.
echo "Running cty..."
export PATH=dist-newstyle/build/x86_64-linux/ghc-9.0.2/curiosity-0.1.0.0/build/cty/:$PATH 
cty --help > /dev/null 2>&1
cty run scenarios/quotation-flow.txt > /dev/null 2>&1

# Combine all the .tix files in a single curiosity.tix file.
TIX=$(ls tix/*.tix | head -n 1)
cp $TIX curiosity.tix
TIXS=$(ls tix/*.tix | tail -n +2)
for i in $TIXS ; do
  hpc combine --union $i curiosity.tix --output new.tix
  mv new.tix curiosity.tix
done

mkdir -p coverage
echo "HPC Report:"
hpc report curiosity.tix \
  --hpcdir dist-newstyle/build/x86_64-linux/ghc-9.0.2/curiosity-0.1.0.0/hpc/vanilla/mix/cty/ --hpcdir dist-newstyle/build/x86_64-linux/ghc-9.0.2/curiosity-0.1.0.0/hpc/vanilla/mix/curiosity-0.1.0.0/
echo "Writing HPC HTML report to coverage/..."
hpc markup curiosity.tix \
  --hpcdir dist-newstyle/build/x86_64-linux/ghc-9.0.2/curiosity-0.1.0.0/hpc/vanilla/mix/cty/ --hpcdir dist-newstyle/build/x86_64-linux/ghc-9.0.2/curiosity-0.1.0.0/hpc/vanilla/mix/curiosity-0.1.0.0/ \
  --destdir coverage

rm curiosity.tix
rm -rf tix
