#!/bin/bash
if [ -f ".paket/paket.exe" ]
then
   echo "paket.exe is available. So skipping paket.bootstrapper"
else
  mono .paket/paket.bootstrapper.exe
fi
exit_code=$?
if [ $exit_code -ne 0 ]; then
  exit $exit_code
fi
mono .paket/paket.exe restore
exit_code=$?
if [ $exit_code -ne 0 ]; then
	exit $exit_code
fi
export MONO_MANAGED_WATCHER=false
mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx