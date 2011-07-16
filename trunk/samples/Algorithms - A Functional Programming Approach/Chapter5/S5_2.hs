module S5_2 where
import Stack
s1 = push 1 (push 2 (push 3 emptyStack))

{- examples of evaluations and results
   with both implementations
? s1
1|2|3|-
? push 4 s1
4|1|2|3|-
? pop s1
2|3|-
? top s1
1
? stackEmpty s1
False
? stackEmpty emptyStack
True
? push "hello" (push "world" emptyStack)
"hello"|"world"|-
----}

