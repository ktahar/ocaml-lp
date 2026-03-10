# tests of lp-gurobi

Test are disabled by default as the access to Gurobi is limited.
To enable, rename `dune.disable` to `dune`.
You also need to change the linker flag from `-lgurobi` to the installed
Gurobi library version such as `-lgurobi91`.
