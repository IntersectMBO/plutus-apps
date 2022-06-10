{ runCommand, cabal-fmt, src }:
runCommand "cabal-fmt"
{
  buildInputs = [ cabal-fmt ];
} ''
  set +e
  for fp in $(find ${src} -name "*.cabal" -not -path "dist*/*" -and -not -path ".stack-work/*"); do 
    cabal-fmt --check $fp
    if [ $? -ne 0 ]; then 
      echo "*** cabal-fmt found changes in $(basename $fp)"
      echo "*** Please run \`fix-cabal-fmt\` and commit changes"
      exit 1
    fi 
  done  
  echo 0 > $out
''
