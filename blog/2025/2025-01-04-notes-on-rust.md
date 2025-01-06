---
title: Mapping C concepts to Rust
tags: rust
---

NOTE: These are my notes as I learn Rust. Accuracy is on par with a hallucinating LLM.

# Build System

## Organization

Package vs Crate vs Module vs Workspace

<https://doc.rust-lang.org/book/ch07-01-packages-and-crates.html>

- A crate is the equivalent of a library. 
- A binary crate is the equivalent of an executable.
- A package is a bundle of one or more crates.
  - Package should have atleast 1 crate.
  - Package can have atmost 1 library crate.
  - Package can have multiple binary crates.
- By conventions, entry point for compilation of a 
  - library crate is `src/lib.rs`
  - binary crate is `src/main.rs`
  
  The entry point is referred to as crate root.
- Crate root can define modules. Module source can be at `src/mod_name.rs`.
- A module can define submodules. Submodules are found at `src/mod_name/submod_name.rs`.
- There is no makefile equivalent for a crate. The files to be compiled are inferred from the crate root.
- A workspace is a cargo supported way to develop multiple closely related packages together.

At a high level,

- Workspace has multiple packages.
- One package has 1 library crate plus some related application.
- Each crate can define some modules and submodules.

## KConfig

# Preprocessor

## Conditional compilation

~~~{.c}
void handle_version()
{
#if (CONFIG_VER==1)
    handle_v1();
#elif (CONFIG_VER==2)
    handle_v2();
#else
    handle_other_versions();
#endif
}
~~~

Rust equivalent is using the [cfg-if](https://crates.io/crates/cfg-if) crate.

~~~{.rust}
fn handle_version() 
{
    if #[cfg(unix)] {
        handle_v1();
    } else if #[cfg(target_pointer_width = "32")] {
        handle-v2();
    } else {
        handle_others();
    }
}
~~~

# Links

1. https://doc.rust-lang.org/book/appendix-04-useful-development-tools.html
