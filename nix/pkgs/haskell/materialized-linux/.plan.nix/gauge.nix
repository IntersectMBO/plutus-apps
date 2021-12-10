{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { analysis = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "gauge"; version = "0.2.5"; };
      license = "BSD-3-Clause";
      copyright = "2009-2016 Bryan O'Sullivan and others";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/vincenthz/hs-gauge";
      url = "";
      synopsis = "small framework for performance measurement and analysis";
      description = "This library provides a powerful but simple way to measure software\nperformance.  It provides both a framework for executing and\nanalysing benchmarks and a set of driver functions that makes it\neasy to build and run benchmarks, and to analyse their results.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.markdown" "changelog.md" "cbits/*.h" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ];
        buildable = if compiler.isGhc && (compiler.version).lt "7.10"
          then false
          else true;
        modules = [
          "Gauge/IO/Printf"
          "Gauge/Measurement"
          "Gauge/Monad"
          "Gauge/ListMap"
          "Gauge/Time"
          "Gauge/Optional"
          "Gauge/CSV"
          "Gauge/Format"
          "Gauge/Source/RUsage"
          "Gauge/Source/GC"
          "Gauge/Source/Time"
          "System/Random/MWC"
          "Paths_gauge"
          "Gauge"
          "Gauge/Main"
          "Gauge/Main/Options"
          "Gauge/Benchmark"
          ] ++ (pkgs.lib).optionals (flags.analysis && !(compiler.isGhcjs && true)) [
          "Statistics/Distribution"
          "Statistics/Distribution/Normal"
          "Statistics/Function"
          "Statistics/Internal"
          "Statistics/Math/RootFinding"
          "Statistics/Matrix"
          "Statistics/Matrix/Algorithms"
          "Statistics/Matrix/Mutable"
          "Statistics/Matrix/Types"
          "Statistics/Quantile"
          "Statistics/Regression"
          "Statistics/Resampling"
          "Statistics/Resampling/Bootstrap"
          "Statistics/Sample"
          "Statistics/Sample/Histogram"
          "Statistics/Sample/Internal"
          "Statistics/Sample/KernelDensity"
          "Statistics/Transform"
          "Statistics/Types"
          "Statistics/Types/Internal"
          "Numeric/MathFunctions/Comparison"
          "Numeric/MathFunctions/Constants"
          "Numeric/SpecFunctions"
          "Numeric/SpecFunctions/Internal"
          "Numeric/Sum"
          "Gauge/Analysis"
          ];
        cSources = (pkgs.lib).optional (!system.isGhcjs) "cbits/cycles.c" ++ (if system.isOsx
          then [ "cbits/time-osx.c" ]
          else if system.isWindows
            then [ "cbits/time-windows.c" ]
            else [ "cbits/time-posix.c" ]);
        hsSourceDirs = [ "." "statistics" "mwc-random" "math-functions" ];
        includeDirs = [ "cbits" ];
        };
      benchmarks = {
        "self" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            ];
          buildable = true;
          hsSourceDirs = [ "benchs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../contrib/gauge-0.2.5; }