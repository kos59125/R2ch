#!/bin/sh

package_name=R2ch
man_directory=${package_name}/man

if [ -d ${package_name} ]; then
	echo 'Directory exists. Exit.' >&2
	exit 1;
fi

Rscript --vanilla package.R
rm ${package_name}/Read-and-delete-me
ln -s ../NAMESPACE ${package_name}/NAMESPACE
cp -n ${man_directory}/* ./man
rm -r ${man_directory}
rm ./man/r2ch.*
ln -s ../man ${man_directory}
