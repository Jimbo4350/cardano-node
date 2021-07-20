usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    block-propagation RUN-NAME
                          Block propagation analysis for the entire cluster.

    machine-timeline RUN-NAME MACH-NAME
                          Produce a general performance timeline for MACH-NAME

    Options of 'analyse' command:

       --reanalyse        Skip the preparatory steps and launch 'locli' directly
       --time             Time the 'locli' executable runs
EOF
}

analyse() {
local skip_preparation= time= dump_logobjects= args=()
while test $# -gt 0
do case "$1" in
       --reanalyse | --re ) skip_preparation='true'; args+=($1);;
       --dump-logobjects )  dump_logobjects='true';  args+=($1);;
       --time )             time='eval time';        args+=($1);;
       * ) break;; esac; shift; done

local op=${1:-$(usage_analyse)}; shift

case "$op" in
    block-propagation | bp )
        local usage="USAGE: wb analyse $op [RUN-NAME=current].."

        local name=${1:-current}; shift
        local dir=$(run get "$name")
        local adir=$dir/analysis
        if test -z "$dir"
        then fail "malformed run: $name"; fi

        msg "analysing run $(jq .meta.tag "$dir"/meta.json --raw-output)"
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys | grep -v 'Temporary modify' > "$keyfile"

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logdirs=($(ls -d "$dir"/node-*/ 2>/dev/null) $(ls -d "$dir"/analysis/node-*/ 2>/dev/null))
        # "$dir"/node-*/ "$dir"/analysis/node-*/

        if test -z "$skip_preparation" -o -z "$(ls "$adir"/logs-node-*.flt.json 2>/dev/null)"
        then
            msg "filtering logs in: $dir/node-* "
            local jq_args=(
                --sort-keys
                --compact-output
                $(wb backend lostream-fixup-jqargs "$dir")
                ' delpaths([["app"],["env"],["loc"],["msg"],["ns"],["sev"]])
                '"$(wb backend lostream-fixup-jqexpr)"
            )
            for d in "${logdirs[@]}"
            do throttle_shell_job_spawns
               local logfiles="$(ls "$d"/stdout* 2>/dev/null | tac) $(ls "$d"/node-*.json 2>/dev/null)"
               if test -z "$logfiles"
               then msg "no logs in $d, skipping.."; fi
               local output="$adir"/logs-$(basename "$d").flt.json
               grep -hFf "$keyfile" $logfiles |
               jq "${jq_args[@]}" --arg dirHostname "$(basename "$d")" \
                 > "$output" &
            done
            wait
            msg "analysis block-propagation:  All done."
        fi

        msg "log sizes:  (files: $(ls "$adir"/*.flt.json 2>/dev/null | wc -l), lines: $(cat "$adir"/*.flt.json | wc -l))"

        msg "analysing.."
        local locli_args=(
            --genesis         "$dir"/genesis.json
            --run-metafile    "$dir"/meta.json
            ## ->
            --timeline-pretty "$adir"/block-propagation.txt
            --analysis-json   "$adir"/block-propagation.json
        )
        if test -n "$dump_logobjects"; then
            locli_args+=(--logobjects-json "$adir"/logs-cluster.logobjects.json); fi

        ${time} locli 'analyse' 'block-propagation' \
                "${locli_args[@]}" "$adir"/*.flt.json

        ## More than one run passed?
        test $# -gt 0 && analyse ${args[*]} block-propagation "$@";;

    grep-filtered-logs | grep | g )
        local usage="USAGE: wb analyse $op BLOCK [MACHSPEC=*] [RUN-NAME=current]"
        local expr=$1
        local mach=${2:-*}
        local name=${3:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        grep -h "$expr" "$adir"/logs-$mach.flt.json;;

    list-blocks | blocks | bs )
        local usage="USAGE: wb analyse $op [RUN-NAME=current]"
        local name=${1:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        fgrep -h "TraceForgedBlock" "$adir"/*.flt.json |
            jq '{ at: .at, host: .host } * .data | del(.peer) | del(.slot)' -c |
            sort | uniq;;

    block-propagation-block | bpb )
        local usage="USAGE: wb analyse $op BLOCK [RUN-NAME=current]"
        local block=$1
        local name=${2:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        grep -h "$block" "$adir"/*.flt.json |
            grep 'AddBlock\|TraceForgedBlock\|AddedToCurrentChain' |
            jq '{ at: .at, host: .host } * .data | del(.peer) | del(.slot)' -c |
            sort --stable | uniq;;

    machine-timeline | machine | mt )
        local usage="USAGE: wb analyse $op [RUN-NAME=current] [MACH-NAME=node-1]"
        local name=${1:-current}
        local mach=${2:-node-1}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        msg "analysing run $(jq .meta.tag "$dir"/meta.json --raw-output)"
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys | grep -v 'Temporary modify' > "$keyfile"

        if test "$mach" = 'all'
        then local machs=($(wb run list-hosts $name))
        else local machs=($mach); fi

        for mach in ${machs[*]}
        do throttle_shell_job_spawns
           (
           ## 1. enumerate logs, filter by keyfile & consolidate
           local logs=($(ls "$dir"/$mach/stdout* 2>/dev/null | tac) $(ls "$dir"/$mach/node-*.json 2>/dev/null) $(ls "$dir"/analysis/$mach/node-*.json 2>/dev/null)) consolidated="$adir"/logs-$mach.json

           if test -z "${logs[*]}"
           then msg "no logs for $mach in run $name"; continue; fi

           if test -z "$skip_preparation" -o -z "$(ls "$adir"/logs-$mach.json 2>/dev/null)"
           then grep -hFf "$keyfile" "${logs[@]}"  > "$consolidated"; fi

           msg "analysing logs of:  $mach  (lines: $(wc -l "$consolidated"))"
           local locli_args=(
               --genesis         "$dir"/genesis.json
               --run-metafile    "$dir"/meta.json
               ## ->
               --timeline-pretty "$adir"/logs-$mach.timeline.txt
               --stats-csv       "$adir"/logs-$mach.stats.csv
               --analysis-json   "$adir"/logs-$mach.analysis.json
               # --slotstats-json  "$adir"/logs-$mach.slotstats.json
               # --timeline-csv            "$adir"/logs-$mach.timeline.csv
               # --cpu-spans-histogram-png "$adir"/logs-"$mach".cpu85-span-lens.png
               # --derived-vectors-0-csv   "$adir"/logs-$mach".derived.1.csv
               # --derived-vectors-1-csv   "$adir"/logs-$mach.derived.1.csv
           )
           if test -n "$dump_logobjects"; then
               locli_args+=(--logobjects-json "$adir"/logs-$mach.logobjects.json); fi

           ${time} locli 'analyse' 'machine-timeline' \
                   "${locli_args[@]}" "$consolidated"
           ) &
        done

        wait
        msg "analysis machine-timeline:  All done.";;

    * ) usage_analyse;; esac
}

num_jobs="\j"
num_threads=$(grep processor /proc/cpuinfo 2>/dev/null || echo -e '\n\n\n' | wc -l)

throttle_shell_job_spawns() {
    sleep 0.5s
    while ((${num_jobs@P} >= num_threads - 4))
    do wait -n; sleep 0.$(((RANDOM % 5) + 1))s; done
}
