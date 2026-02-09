# ⚠️ Refactor in Progress

Many of the examples in this folder **do not compile or run at the moment**.

The primary goal of the refactor is to redesign core components to use explicit lifetimes and improve ownership semantics. This requires reworking large portions of the existing codebase, which temporarily breaks several examples.

---

## Why This Is Happening

The original implementations prioritized experimentation and feature exploration.

The current effort focuses on:

- Introducing explicit lifetime annotations
- Improving borrowing correctness
- Eliminating unnecessary allocations
- Strengthening memory safety guarantees
- Making ownership relationships explicit

This transition is non-trivial and affects public interfaces, which is why several examples are currently out of sync.

---

## What to Expect

- Some examples may fail to compile.
- Some APIs may change without notice.
- Code structure may shift significantly.

Stability will return once the lifetime model and ownership structure are finalized.

---

If you are browsing this repository, please treat this folder as work-in-progress rather than a stable reference.