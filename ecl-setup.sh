#!/bin/bash
export ECLS=$(which ecl)
export ECLS_BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
alias ecl="rlwrap -b \$ECLS_BREAK_CHARS $ECLS"
