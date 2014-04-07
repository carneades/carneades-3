#!/bin/bash

# Check, whether the following programs are installed:
# - ruby => 1.9.3
# - node

#
# Authors:
#   Sebastian Kaiser	sebastian.kaiser (at) fokus.fraunhofer.de
#   Pierre Allix                         (at) fokus.fraunhofer.de
#
# License:
#
# made for making the installation as smooth as possible
#
#
#################################################################################
####
####  For help, issue the following command:
####  ./install -h
####
#################################################################################
progname="install"

# see: http://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )

echo "$SCRIPTPATH"

version () {
    echo "$progname version 1.0.0"
}

print_report_issues () {
    echo "Report issues at:"
    echo "https://github.com/carneades/carneades/issues"
}

usage () {
    echo "$progname [option...] "
    echo "Install the necessary dependencies for building carneades"
    echo " Options are:"
    echo "   -war           create a war package"
    echo "   -h             Show this help"
    echo "   -v             Print version information"

    print_report_issues
}

while getopts hr:mi:v opt
do
    case "$opt" in
        v)
            version
            exit 0
            ;;

        h) usage
           exit 0
           ;;

        '?')
            exit 1
            ;;

        *)
            echo "unhandled option {$opt}" 1>&2
            exit 1
            ;;
   esac
done
shift "`expr $OPTIND - 1`"

# Prompt the user for continuing or quitting.
ok_or_quit () {
    cont=true
    while $cont
    do
	printf "Type [c] to continue, [q] to quit: "
	read line
	case "$line" in
	    c)
		cont=false
		;;
	    q)
		exit 0
		;;
	    *)
		echo "invalid input: {$line}" 1>&2
		;;
	esac
    done
}

#################################################################################
####                                                                            #
#### These are some dependency tests, don't touch them.                         #
#### If you can't compile properly I'm not gonna let you try ;)                 #
####                                                                            #
#################################################################################

DEP_FAIL_FLAG=0    #set the unmet dependency flag to "no missing deps"

# Usage:
# test_exe_exists COMMAND
#
# Example:
# if test_exe_exists wine
# then
#    ...case where the wine executable can be found...
# else
#    ...otherwise...
# fi
test_exe_exists () {
    result="`command -v "$1" 2>/dev/null`"
    if [ -n "$result" ]
    then
	echo "$1 found: $result"
	return 0
    else
	return 1
    fi
}

# Usage:
# check_program COMMAND PROGNAME
# Check that COMMAND can be found in the PATH.
# Otherwise, print an error message indicating that PROGNAME is probably not installed.
check_program () {
    if ! test_exe_exists "$1"
    then
	echo "cannot find command '$1'" 1>&2
	echo "Please make sure $2 is installed." 1>&2
	DEP_FAIL_FLAG=1;  #set the unmet dependency flag to "missing dep(s) found"
    fi
}

check_program ruby Ruby
check_program npm "The node.js package manager"
#################################################################################
####                                                                            #
#### end of dependency tests                                                    #
####                                                                            #
#################################################################################

#test if any dependency is unmet
if [ $DEP_FAIL_FLAG -eq 1 ]; then
  echo "Some dependencies are not met.  Please install them and try again." 1>&2
  exit 1
fi


#################################################################################
#download and extract packages based on ruby and npm                            #
#################################################################################
#echo "update ruby system"
#pdate --system

echo "Install haml..."
sudo apt-get install ruby-haml

echo "Install coffee..."
sudo apt-get install ruby-compass

echo "In order to complete the installation process we need ROOt priviliges"
sudo npm install -g grunt-cli
sudo npm install -g coffee-script
sudo npm install -g bower

cd $SCRIPTPATH/../client #root/client
npm install
bower install
grunt build
