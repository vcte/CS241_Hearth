ghc --make -O2 -fexcess-precision -optc-O3 -optc-ffast-math -XBangPatterns -funbox-strict-fields -fforce-recomp HearthMain

:: Compile HearthMain.hs, to HearthMain.exe, using following parameters:
:: -O2 - compiler optimization flag
:: -fexcess-precision - improve floating point performance, by keeping more intermediates in registers
:: -optc-O3 - run GCC compiler with -O3 optimization flag
:: -optc-ffast-math - speed up floating point arithmetic, by ignoring NaN and +/- Infinity
:: -XBangPatterns - allow for bang patterns in 'where' functions
:: -fforce-recomp - recompile program, even if no changes made
