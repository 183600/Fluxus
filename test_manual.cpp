Build profile: -w ghc-9.6.3 -O1
In order, the following will be built (use -v for more details):
 - fluxus-0.1.0.0 (lib) (first run)
 - fluxus-0.1.0.0 (exe:fluxus) (first run)
Preprocessing library for fluxus-0.1.0.0..
Building library for fluxus-0.1.0.0..
[21 of 23] Compiling Fluxus.CodeGen.CPP ( src/Fluxus/CodeGen/CPP.hs, /home/qwe12345678/hyperstatic2/dist-newstyle/build/x86_64-linux/ghc-9.6.3/fluxus-0.1.0.0/build/Fluxus/CodeGen/CPP.o, /home/qwe12345678/hyperstatic2/dist-newstyle/build/x86_64-linux/ghc-9.6.3/fluxus-0.1.0.0/build/Fluxus/CodeGen/CPP.dyn_o )

src/Fluxus/CodeGen/CPP.hs:412:7: error: [GHC-07626]
    Parse error in pattern: (_, _) when length ops + 1 == length exprs
    |
412 |       (_, _) when length ops + 1 == length exprs -> do
    |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: cabal: Failed to build fluxus-0.1.0.0 (which is required by exe:fluxus
from fluxus-0.1.0.0).

