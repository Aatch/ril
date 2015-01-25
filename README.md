# RIL - Rust Intermediate Language

A prototype/experiment/proof-of-concept of an SSA-form intermediate language for use in the Rust
compiler.

It makes heavy use of unsafe code to try and be reasonable efficient, however, the use various uses
of unsafe code have **not** been closely examined and segfaults or worse may occur during reasonable
use.

This code is licensed under the same terms as the Rust Programming Language Project.