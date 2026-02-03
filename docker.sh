#!/bin/bash

function lowercase() {
    echo "$1" | tr '[:upper:]' '[:lower:]'
}

function usage() {
    echo "$0 [build|run] platform"
    echo ""
    echo "Build and/or run a docker image that can build/test noc-compiler."
    echo ""
    echo "  build      build docker image for specified platform"
    echo "             will be tagged as noc-compiler:<platform>"
    echo ""
    echo "  run        run an already built image for a specified platform"
    echo ""

    return 2
}

function attach() {
    local platform
    instance_name="$(lowercase "$1")"
    echo $instance_name

    if [[ ! "$instance_name" ]]
    then
        echo "Error: instance_name is required"
        echo "attach <instance_name>"
        echo "use docker ps to find instance name"
        exit 1
    fi

    local tag
    tag="$project:$platform"

    local id
    id="$(docker ps --format '{{.Names}} {{.ID}}' | grep "^$instance_name " | awk '{print $2}')"  

    docker exec -it "$id" "/bin/bash"
}

function build() {
    local platform
    platform="$(lowercase "$1")"

    local tag
    tag="$project:$platform"

    local file
    file="Dockerfile-$platform"
    if [[ ! -f "$file" ]]; then
        echo "Platform probably not supported, cannot find $file"
        return 1
    fi

    docker build \
        --tag "$tag" \
        --file "$file" \
        "$root"
}

function run() {
    local platform
    platform="$(lowercase "$1")"

    local tag
    tag="$project:$platform"

    echo $tag

    local guest_workdir
    guest_workdir="/app"

    docker run --rm --interactive --tty \
        --volume "$root:$guest_workdir" \
        "$tag"
}

function main() {
    if [[ $# -lt 1 ]]; then
        usage
        return $?
    fi

    # set action
    local action
    action="$1"

    # set platform
    local platform
    platform="debian"
    if [[ $# -ge 2 ]]; then
        platform="$2"
    fi

    declare -r project="noc-compiler"
    declare -r root="$(git rev-parse --show-toplevel)"

    case "$action" in
        attach)
            shift 1
            attach "$@" ;;
        build)
            build "$platform" ;;
        run)
            shift 1
            run "$platform" ;;
        *)
            usage
            return $? ;;
    esac
}

main "$@"
