# Desugarer

Desugars do-notation into infix applications of `(>>=)` and `return`.
Just a fun adventure in SYB-land. Compiled to JS using GHCJS, with a
toy interface written with Reflex.

No support for `-XArrows` - I have no idea how they work and don't know
how to desugar them properly.