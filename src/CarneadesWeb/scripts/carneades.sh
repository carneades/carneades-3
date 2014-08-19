#!/usr/bin/env bash

# Ensure this file is executable via `chmod a+x lein`, then place it
# somewhere on your $PATH, like ~/bin. The rest of Leiningen will be
# installed upon first run into the ~/.lein/self-installs directory.

export CARNEADES_VERSION="1.0.0-SNAPSHOT"

case $CARNEADES_VERSION in
    *SNAPSHOT) SNAPSHOT="YES" ;;
    *) SNAPSHOT="NO" ;;
esac

if [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "msys" ]]; then
    delimiter=";"
else
    delimiter=":"
fi

if [[ "$OSTYPE" == "cygwin" ]]; then
  cygwin=true
else
  cygwin=false
fi

function make_native_path {
    # ensure we have native paths
    if $cygwin && [[ "$1"  == /* ]]; then
    echo -n "$(cygpath -wp "$1")"
    elif [[ "$OSTYPE" == "msys" && "$1"  == /?/* ]]; then
    echo -n "$(sh -c "(cd $1 2</dev/null && pwd -W) || echo $1 | sed 's/^\\/\([a-z]\)/\\1:/g'")"
    else
    echo -n "$1"
    fi
}

#  usage : add_path PATH_VAR [PATH]...
function add_path {
    local path_var="$1"
    shift
    while [ -n "$1" ];do
        # http://bashify.com/?Useful_Techniques:Indirect_Variables:Indirect_Assignment
        if [[ -z ${!path_var} ]]; then
          export ${path_var}="$(make_native_path "$1")"
        else
          export ${path_var}="${!path_var}${delimiter}$(make_native_path "$1")"
        fi
    shift
    done
}

function download_failed_message {
    echo "Failed to download $1"
    echo "It's possible your HTTP client's certificate store does not have the"
    echo "correct certificate authority needed. This is often caused by an"
    echo "out-of-date version of libssl. Either upgrade it or set HTTP_CLIENT"
    echo "to turn off certificate checks:"
    echo "  export HTTP_CLIENT=\"wget --no-check-certificate -O\" # or"
    echo "  export HTTP_CLIENT=\"curl --insecure -f -L -o\""
    echo "It's also possible that you're behind a firewall haven't yet"
    echo "set HTTP_PROXY and HTTPS_PROXY."
}

function self_install {
  if [ -r "$CARNEADES_JAR" ]; then
    echo "The self-install jar already exists at $CARNEADES_JAR."
    echo "If you wish to re-download, delete it and rerun \"$0 self-install\"."
    exit 1
  fi
  echo "Downloading Carneades to $CARNEADES_JAR now..."
  mkdir -p "$(dirname "$CARNEADES_JAR")"
  #CARNEADES_URL="https://github.com/carneades/carneades/releases/download/$CARNEADES_VERSION/carneades-$CARNEADES_VERSION-standalone.jar"
  CARNEADES_URL="https://github.com/carneades/carneades/releases/download/test/hello_world.sh"
  $HTTP_CLIENT "$CARNEADES_JAR.pending" "$CARNEADES_URL"
  if [ $? == 0 ]; then
      # TODO: checksum
      mv -f "$CARNEADES_JAR.pending" "$CARNEADES_JAR"
  else
      rm "$CARNEADES_JAR.pending" 2> /dev/null
      download_failed_message "$CARNEADES_URL"
      exit 1
  fi
}

if [ `id -u` -eq 0 ] && [ "$CARNEADES_ROOT" = "" ]; then
    echo "WARNING: You're currently running as root; probably by accident."
    echo "Press control-C to abort or Enter to continue as root."
    echo "Set CARNEADES_ROOT to disable this warning."
    read _
fi

NOT_FOUND=1
ORIGINAL_PWD="$PWD"
while [ ! -r "$PWD/project.clj" ] && [ "$PWD" != "/" ] && [ $NOT_FOUND -ne 0 ]
do
    cd ..
    if [ "$(dirname "$PWD")" = "/" ]; then
        NOT_FOUND=0
        cd "$ORIGINAL_PWD"
    fi
done

export CARNEADES_HOME="${CARNEADES_HOME:-"$HOME/.carneades"}"

for f in "$CARNEADES_HOME/carneadesrc" ".carneadesrc"; do
  if [ -e "$f" ]; then
    source "$f"
  fi
done

if $cygwin; then
    export CARNEADES_HOME=`cygpath -w "$CARNEADES_HOME"`
fi

CARNEADES_JAR="$CARNEADES_HOME/self-installs/carneades-$CARNEADES_VERSION-standalone.jar"

# normalize $0 on certain BSDs
if [ "$(dirname "$0")" = "." ]; then
    SCRIPT="$(which $(basename "$0"))"
else
    SCRIPT="$0"
fi

# resolve symlinks to the script itself portably
while [ -h "$SCRIPT" ] ; do
    ls=`ls -ld "$SCRIPT"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "$link" : '/.*' > /dev/null; then
        SCRIPT="$link"
    else
        SCRIPT="$(dirname "$SCRIPT"$)/$link"
    fi
done

BIN_DIR="$(dirname "$SCRIPT")"

export CARNEADES_JVM_OPTS="${CARNEADES_JVM_OPTS-"-XX:+TieredCompilation -XX:TieredStopAtLevel=1"}"

# This needs to be defined before we call HTTP_CLIENT below
if [ "$HTTP_CLIENT" = "" ]; then
    if type -p curl >/dev/null 2>&1; then
        if [ "$https_proxy" != "" ]; then
            CURL_PROXY="-x $https_proxy"
        fi
        HTTP_CLIENT="curl $CURL_PROXY -f -L -o"
    else
        HTTP_CLIENT="wget -O"
    fi
fi

if [ -r "$BIN_DIR/../src/leiningen/version.clj" ]; then
    # Running from source checkout
    CARNEADES_DIR="$(dirname "$BIN_DIR")"

    # Need to use carneades release to bootstrap the carneades-core library (for aether)
    if [ ! -r "$CARNEADES_DIR/carneades-core/.carneades-bootstrap" ]; then
        echo "Carneades is missing its dependencies."
        echo "Please run \"carneades bootstrap\" in the carneades-core/ directory"
        echo "with a stable release of Leiningen. See CONTRIBUTING.md for details."
        exit 1
    fi

    # If project.clj for lein or leiningen-core changes, we must recalculate
    LAST_PROJECT_CHECKSUM=$(cat "$CARNEADES_DIR/.carneades-project-checksum" 2> /dev/null)
    PROJECT_CHECKSUM=$(sum "$CARNEADES_DIR/project.clj" "$CARNEADES_DIR/leiningen-core/project.clj")
    if [ "$PROJECT_CHECKSUM" != "$LAST_PROJECT_CHECKSUM" ]; then
        if [ -r "$CARNEADES_DIR/.carneades-classpath" ]; then
            rm "$CARNEADES_DIR/.carneades-classpath"
        fi
    fi
else # Not running from a checkout
    add_path CLASSPATH "$CARNEADES_JAR"

    BOOTCLASSPATH="-Xbootclasspath/a:$CARNEADES_JAR"

    if [ ! -r "$CARNEADES_JAR" -a "$1" != "self-install" ]; then
        self_install
    fi
fi

# TODO: explain what to do when Java is missing
export JAVA_CMD="${JAVA_CMD:-"java"}"
export CARNEADES_JAVA_CMD="${CARNEADES_JAVA_CMD:-$JAVA_CMD}"

# Support $JAVA_OPTS for backwards-compatibility.
export JVM_OPTS="${JVM_OPTS:-"$JAVA_OPTS"}"

# Handle jline issue with cygwin not propagating OSTYPE through java subprocesses: https://github.com/jline/jline2/issues/62
cygterm=false
if $cygwin; then
  case "$TERM" in
    rxvt* | xterm* | vt*) cygterm=true ;;
  esac
fi

if $cygterm; then
  CARNEADES_JVM_OPTS="$CARNEADES_JVM_OPTS -Djline.terminal=jline.UnixTerminal"
  stty -icanon min 1 -echo > /dev/null 2>&1
fi

# TODO: investigate http://skife.org/java/unix/2011/06/20/really_executable_jars.html
# If you're packaging this for a package manager (.deb, homebrew, etc)
# you need to remove the self-install and upgrade functionality or see lein-pkg.
if [ "$1" = "self-install" ]; then
    if [ -r "$BIN_DIR/../src/leiningen/version.clj" ]; then
        echo "Running self-install from a checkout is not supported."
        echo "See CONTRIBUTING.md for SNAPSHOT-specific build instructions."
        exit 1
    fi
    echo "Manual self-install is deprecated; it will run automatically when necessary."
    #self_install
elif [ "$1" = "upgrade" ] || [ "$1" = "downgrade" ]; then
    if [ "$CARNEADES_DIR" != "" ]; then
        echo "The upgrade task is not meant to be run from a checkout."
        exit 1
    fi
    if [ $SNAPSHOT = "YES" ]; then
        echo "The upgrade task is only meant for stable releases."
        echo "See the \"Hacking\" section of the README."
        exit 1
    fi
    if [ ! -w "$SCRIPT" ]; then
        echo "You do not have permission to upgrade the installation in $SCRIPT"
        exit 1
    else
        TARGET_VERSION="${2:-stable}"
        echo "The script at $SCRIPT will be upgraded to the latest $TARGET_VERSION version."
        echo -n "Do you want to continue [Y/n]? "
        read RESP
        case "$RESP" in
            y|Y|"")
                echo
                echo "Upgrading..."
                TARGET="/tmp/carneades-$$-upgrade"
                if $cygwin; then
                    TARGET=`cygpath -w $TARGET`
                fi
                CARNEADES_SCRIPT_URL="https://github.com/carneades/carneades/raw/$TARGET_VERSION/bin/carneades"
                $HTTP_CLIENT "$TARGET" "$CARNEADES_SCRIPT_URL"
                if [ $? == 0 ]; then
                    cmp -s "$TARGET" "$SCRIPT"
                    if [ $? == 0 ]; then
                        echo "Carneades is already up-to-date."
                    fi
                    mv "$TARGET" "$SCRIPT" && chmod +x "$SCRIPT"
                    exec "$SCRIPT" version
                else
                    download_failed_message "$CARNEADES_SCRIPT_URL"
                fi;;
            *)
                echo "Aborted."
                exit 1;;
        esac
    fi
else
    if $cygwin; then
        # When running on Cygwin, use Windows-style paths for java
        ORIGINAL_PWD=`cygpath -w "$ORIGINAL_PWD"`
    fi

    # apply context specific CLASSPATH entries
    if [ -f .carneades-classpath ]; then
        add_path CLASSPATH "$(cat .carneades-classpath)"
    fi

    if [ $DEBUG ]; then
        echo "Carneades's classpath: $CLASSPATH"
    fi
fi
