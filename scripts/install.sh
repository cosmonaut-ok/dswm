#!/bin/sh
# cl-travis install script. Don't remove this line.
set -x

# get <destination> <url(s)>
get() {
    destination=$1; shift
    for url in "$@"; do
        echo "Downloading ${url}..."
        if curl --no-progress-bar --retry 10  -o "$destination" -L "$url"; then
            return 0;
        else
            echo "Failed to download ${url}."
        fi
    done

    return 1;
}

# unpack <uncompression option> <file> <destination>
unpack() {
    opt=$1
    file=$2;
    destination=$3;

    echo "Unpacking tarball $1 into $3..."
    mkdir -p "$destination"
    tar -C "$destination" --strip-components=1 "$opt" -xf "$file"
}

install_i386_arch() {
    # Travis-CI's dpkg doesn't seem to know about --add-architecture.
    #sudo dpkg --add-architecture i386
    sudo apt-get install libc6:i386
}

# add_to_lisp_rc <string>
add_to_lisp_rc() {
    string=$1

    case "$LISP" in
        abcl) rc=".abclrc" ;;
        allegro*) rc=".clinit.cl" ;;
        sbcl*) rc=".sbclrc" ;;
        ccl*) rc=".ccl-init.lisp" ;;
        cmucl) rc=".cmucl-init.lisp" ;;
        clisp*) rc=".clisprc.lisp" ;;
        ecl) rc=".eclrc" ;;
        *)
            echo "Unable to determine RC file for '$LISP'."
            exit 1
            ;;
    esac

    echo "$string" >> "$HOME/.cim/init.lisp"
    echo "$string" >> "$HOME/$rc"
}

ASDF_URL="https://raw.githubusercontent.com/luismbo/cl-travis/master/deps/asdf.lisp"
ASDF_LOCATION="$HOME/asdf"

install_asdf() {
    get asdf.lisp "$ASDF_URL"
    add_to_lisp_rc "(load \"$ASDF_LOCATION\")"
}

compile_asdf() {
    echo "Compiling ASDF..."
    cl -c "$ASDF_LOCATION.lisp" -Q
}

ASDF_SR_CONF_DIR="$HOME/.config/common-lisp/source-registry.conf.d"
ASDF_SR_CONF_FILE="$ASDF_SR_CONF_DIR/cl-travis.conf"
LOCAL_LISP_TREE="$HOME/lisp"

setup_asdf_source_registry() {
    mkdir -p "$LOCAL_LISP_TREE"
    mkdir -p "$ASDF_SR_CONF_DIR"

    echo "(:tree \"$TRAVIS_BUILD_DIR/\")" > "$ASDF_SR_CONF_FILE"
    echo "(:tree \"$LOCAL_LISP_TREE/\")" >> "$ASDF_SR_CONF_FILE"

    echo "Created $ASDF_SR_CONF_FILE"
    cat -n "$ASDF_SR_CONF_FILE"
}

# install_script <path> <lines...>
install_script() {
    path=$1; shift
    tmp=$(mktemp)

    echo "#!/bin/sh" > "$tmp"
    for line; do
        echo "$line" >> "$tmp"
    done
    chmod 755 "$tmp"

    sudo mv "$tmp" "$path"
}

ABCL_TARBALL_URL1="http://www.abcl.org/releases/1.2.1/abcl-bin-1.2.1.tar.gz"
ABCL_TARBALL_URL2="http://kerno.org/~luis/ci/abcl-bin-1.2.1.tar.gz"
ABCL_TARBALL="abcl.tar.gz"
ABCL_DIR="$HOME/abcl"
ABCL_SCRIPT="/usr/local/bin/abcl"

install_abcl() {
    sudo apt-get install default-jre
    get "$ABCL_TARBALL" "$ABCL_TARBALL_URL1" "$ABCL_TARBALL_URL2"
    unpack -z "$ABCL_TARBALL" "$ABCL_DIR"

    install_script "$ABCL_SCRIPT" \
        "java -cp \"$ABCL_DIR/abcl-contrib.jar\" \
              -jar \"$ABCL_DIR/abcl.jar\" \"\$@\""

    cim use abcl-system --default
}

SBCL_TARBALL_URL1="http://prdownloads.sourceforge.net/sbcl/sbcl-1.2.6-x86-64-linux-binary.tar.bz2"
SBCL_TARBALL_URL2="http://common-lisp.net/~loliveira/tarballs/ci/sbcl-1.2.6-x86-64-linux-binary.tar.bz2"
SBCL_TARBALL_URL3="http://kerno.org/~luis/ci/sbcl-1.2.6-x86-64-linux-binary.tar.bz2"
SBCL_TARBALL="sbcl.tar.bz2"
SBCL_DIR="$HOME/sbcl"

install_sbcl() {
    echo "Installing SBCL..."
    get "$SBCL_TARBALL" "$SBCL_TARBALL_URL1" "$SBCL_TARBALL_URL2" "$SBCL_TARBALL_URL3"
    unpack -j "$SBCL_TARBALL" "$SBCL_DIR"
    ( cd "$SBCL_DIR" && sudo bash install.sh )

    cim use sbcl-system --default
}

SBCL32_TARBALL_URL1="http://common-lisp.net/~loliveira/tarballs/sbcl-1.2.6-x86-linux-binary.tar.bz2"
SBCL32_TARBALL_URL2="http://kerno.org/~luis/ci/sbcl-1.2.6-x86-linux-binary.tar.bz2"
SBCL32_TARBALL="sbcl32.tar.bz2"
SBCL32_DIR="$HOME/sbcl32"

install_sbcl32() {
    echo "Installing 32-bit SBCL..."
    install_i386_arch

    get "$SBCL32_TARBALL" "$SBCL32_TARBALL_URL1" "$SBCL32_TARBALL_URL2"
    unpack -j "$SBCL32_TARBALL" "$SBCL32_DIR"
    ( cd "$SBCL32_DIR" && sudo bash install.sh )
    sudo ln -s /usr/local/bin/sbcl /usr/local/bin/sbcl32

    cim use sbcl-system --default
}

CCL_TARBALL_URL1="ftp://ftp.clozure.com/pub/release/1.10/ccl-1.10-linuxx86.tar.gz"
CCL_TARBALL_URL2="http://kerno.org/~luis/ci/ccl-1.10-linuxx86.tar.gz"
CCL_TARBALL_URL3="http://common-lisp.net/~loliveira/tarballs/ci/ccl-1.10-linuxx86.tar.gz"
CCL_TARBALL="ccl.tar.gz"
CCL_DIR="$HOME/ccl"
CCL_SCRIPT_PREFIX="/usr/local/bin"

install_ccl() {
    if [ "$LISP" = "ccl32" ]; then
        echo "Installing 32-bit CCL..."
        install_i386_arch
        bin="lx86cl"
        script="ccl32"
    else
        echo "Installing CCL..."
        bin="lx86cl64"
        script="ccl"
    fi
    get "$CCL_TARBALL" "$CCL_TARBALL_URL1" "$CCL_TARBALL_URL2" "$CCL_TARBALL_URL3"
    unpack -z "$CCL_TARBALL" "$CCL_DIR"

    install_script "$CCL_SCRIPT_PREFIX/$script" "\"$CCL_DIR/$bin\" \"\$@\""
    if [ "$LISP" = "ccl32" ]; then
        # also install the 'ccl' script so that CIM can pick it up.
        install_script "$CCL_SCRIPT_PREFIX/ccl" "\"$CCL_DIR/$bin\" \"\$@\""
    fi

    cim use ccl-system --default
}

CMUCL_TARBALL_URL1="http://common-lisp.net/project/cmucl/downloads/snapshots/2014/12/cmucl-2014-12-x86-linux.tar.bz2"
CMUCL_EXTRA_TARBALL_URL1="http://common-lisp.net/project/cmucl/downloads/snapshots/2014/12/cmucl-2014-12-x86-darwin.extra.tar.bz2"
CMUCL_TARBALL_URL2="http://kerno.org/~luis/ci/cmucl-2014-12-x86-linux.tar.bz2"
CMUCL_EXTRA_TARBALL_URL2="http://kerno.org/~luis/ci/cmucl-2014-12-x86-darwin.extra.tar.bz2"
CMUCL_TARBALL="cmucl.tar.bz2"
CMUCL_EXTRA_TARBALL="cmucl-extra.tar.bz2"
CMUCL_DIR="$HOME/cmucl"
CMUCL_SCRIPT="/usr/local/bin/cmucl"

install_cmucl() {
    echo "Installing CMUCL..."
    install_i386_arch
    get "$CMUCL_TARBALL" "$CMUCL_TARBALL_URL1" "$CMUCL_TARBALL_URL2"
    get "$CMUCL_EXTRA_TARBALL" "$CMUCL_EXTRA_TARBALL_URL" "$CMUCL_EXTRA_TARBALL_URL2"
    mkdir -p "$CMUCL_DIR"
    tar -C "$CMUCL_DIR" -xjf "$CMUCL_TARBALL"
    tar -C "$CMUCL_DIR" -xjf "$CMUCL_EXTRA_TARBALL"

    install_script "$CMUCL_SCRIPT" \
        "CMUCLLIB=\"$CMUCL_DIR/lib/cmucl/lib\" \"$CMUCL_DIR/bin/lisp\" \"\$@\""

    # XXX: no CIM support for CMUCL
}

ECL_TARBALL_URL1="http://common-lisp.net/~loliveira/tarballs/ecl-13.5.1-linux-amd64.tar.gz"
ECL_TARBALL_URL2="http://kerno.org/~luis/ci/ecl-13.5.1-linux-amd64.tar.gz"
ECL_TARBALL="ecl.tar.gz"

install_ecl() {
    echo "Installing ECL..."
    get "$ECL_TARBALL" "$ECL_TARBALL_URL1" "$ECL_TARBALL_URL2"
    sudo tar -C / -xzf "$ECL_TARBALL"

    cim use ecl-system --default
}

install_clisp() {
    if [ "$LISP" = "clisp32" ]; then
        echo "Installing 32-bit CLISP..."
        sudo apt-get remove libsigsegv2
        sudo apt-get install libsigsegv2:i386
        sudo apt-get install clisp:i386
        sudo ln -s /usr/bin/clisp /usr/local/bin/clisp32
    else
        echo "Installing CLISP..."
        sudo apt-get install clisp
    fi
    cim use clisp-system --default
}

install_acl() {
    echo "Installing Allegro CL..."
    install_i386_arch

    case "$LISP" in
        allegro) acl=alisp ;;
        allegromodern) acl=mlisp ;;
        *)
            echo "Unrecognised lisp: '$LISP'"
            exit 1
            ;;
    esac

    cim install "$acl"

    sudo ln -vs "$HOME/.cim/bin/$acl" "/usr/local/bin/$acl"
    sudo ln -vs "$HOME/.cim/bin/$acl" "/usr/local/bin/$LISP"

    # XXX: cim doesn't support mlisp
    cim use "$acl" --default
}

QUICKLISP_URL="http://beta.quicklisp.org/quicklisp.lisp"

install_quicklisp() {
    get quicklisp.lisp "$QUICKLISP_URL"
    echo 'Installing Quicklisp...'
    cl -f quicklisp.lisp -e '(quicklisp-quickstart:install)'
    add_to_lisp_rc '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname))))
                      (when (probe-file quicklisp-init)
                        (load quicklisp-init)))'
}

CL_SCRIPT="/usr/local/bin/cl"
CIM_SCRIPT="/usr/local/bin/cim"
QL_SCRIPT="/usr/local/bin/ql"

install_cim() {
    curl -L https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | /bin/sh

    install_script "$CL_SCRIPT"  ". \"$HOME\"/.cim/init.sh; exec cl  \"\$@\""
    install_script "$CIM_SCRIPT" ". \"$HOME\"/.cim/init.sh; exec cim \"\$@\""
    # install_script "$QL_SCRIPT"  ". \"$HOME\"/.cim/init.sh; exec ql  \"\$@\""
}

(
    cd "$HOME"

    # sudo apt-get update
    install_cim
    install_asdf

    case "$LISP" in
        abcl) install_abcl ;;
        allegro|allegromodern) install_acl ;;
        sbcl) :;; # install_sbcl ;;
        sbcl32) :;; # install_sbcl32 ;;
        ccl|ccl32) install_ccl ;;
        cmucl) install_cmucl; exit 0 ;; # no CIM support
        clisp|clisp32) install_clisp ;;
        ecl) install_ecl ;;
        *)
            echo "Unrecognised lisp: '$LISP'"
            exit 1
            ;;
    esac

    compile_asdf

    cl -e '(format t "~%~a ~a up and running! (ASDF ~a)~%~%"
                   (lisp-implementation-type)
                   (lisp-implementation-version)
                   (asdf:asdf-version))'

    # install_quicklisp
    # setup_asdf_source_registry
)
