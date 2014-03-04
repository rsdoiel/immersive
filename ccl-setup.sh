#!/bin/bash
export CCL_BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
export CCLS="$HOME/ccl/armcl"
alias ccl="rlwrap -b \$CCL_BREAK_CHARS $CCLS"
