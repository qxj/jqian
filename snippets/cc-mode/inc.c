# -*- mode: snippet -*-
#name : #include <...>
#key: incc
# --
#include <${1:$$(yas/choose-value '("assert.h" "errno.h" "limits.h" "signal.h" "stdarg.h" "stddef.h" "string.h" "stdlib.h" "stdio.h"))}>