#!/bin/bash
export ECLS_BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
ECLS=(which ecl)
alias ecl="rlwrap -b \$ECLS_BREAK_CHARS $ECLS"
