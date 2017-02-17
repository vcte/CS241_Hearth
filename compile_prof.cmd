ghc --make -O2 -fexcess-precision -optc-O3 -optc-ffast-math -prof -auto-all -caf-all -rtsopts -fforce-recomp HearthProf

:: Compile HearthMain.hs, to HearthMain.exe, using following parameters:
:: -O2 - compiler optimization flag
:: -fexcess-precision - improve floating point performance, by keeping more intermediates in registers
:: -optc-O3 - run GCC compiler with -O3 optimization flag
:: -optc-ffast-math - speed up floating point arithmetic, by ignoring NaN and +/- Infinity
:: -fforce-recomp - recompile program, even if no changes made

:: -prof - enable profiling
:: -auto-all - automatically add cost centers for functions
:: -caf-all - get accurate values for constant applicative forms
:: -rtsopts - enable RTS options

:: to get profile, run HearthProf +RTS -p
