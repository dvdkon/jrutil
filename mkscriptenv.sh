#!/usr/bin/env sh
latest() {
	ls -td "$@" | head -n 1
}

cd scripts
for proj in ../*; do
	name=$(basename $proj)
	outdir=$(latest $proj/bin/*/* 2>/dev/null)
	depsfile=$outdir/$name.deps.json
	if [ -e $name ]; then rm -r $name; fi
	if [ ! -e "$depsfile" ]; then continue; fi
	mkdir $name
	cd $name
	proj=../$proj
	outdir=../$outdir
	depsfile=../$depsfile
	for dll in $outdir/*.dll; do
		ln -nsf $dll .
		echo '#r "'$(basename $dll)'"' >> ref.fsx
	done
	jqscript="$(cat <<EOF
(.targets | values[] | to_entries[]
 | select(.key | startswith("$name/"))
).value.dependencies | to_entries[]
| "\(.key) \(.value)"
EOF
)"
	jq -r "$jqscript" $depsfile | while read depline; do
		depname=$(echo $depline | cut -d" " -f1)
		depver=$(echo $depline | cut -d" " -f2)
		if [ ! -e $depname.dll ] && [ $depname != FSharp.Core ]; then
			echo '#r "nuget:'$depname,$depver'"' >> ref.fsx
		fi
	done
	cd ..
done
