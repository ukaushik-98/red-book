## Monad intuition: State vs Option

### flatten (aka join)

**State**
- `flatten: State[S, State[S, A]] => State[S, A]`
- Flatten runs the *outer* state action first, producing:
    - an inner `State[S, A]`
    - an intermediate state `S`
- It then runs the inner state action using that intermediate state.
- This results in **two `run` invocations**, with the state threaded:
  `s -> s2 -> s3`.

**Option**
- `flatten: Option[Option[A]] => Option[A]`
- Flatten collapses structure:
    - `Some(Some(x)) -> Some(x)`
    - `Some(None) -> None`
    - `None -> None`
- No computation is sequenced; this is purely structural unwrapping.

---

### map

**State**
- `map` runs the state action **once** to obtain `(a, s2)`.
- It applies a pure function to the value `a`.
- The state `s2` is passed through unchanged.
- Exactly **one `run` invocation** occurs.

**Option**
- `map` pattern-matches on the option:
    - `Some(x)` → apply the function to `x`
    - `None` → do nothing
- No sequencing or short-circuiting beyond structural inspection.

---

### flatMap

**State**
- `flatMap` sequences *two stateful computations*.
- First, it runs the current state action:
    - `run(s) -> (a, s2)`
- Then it uses `a` to produce the *next* state action and runs it with `s2`.
- The composed computation performs **two `run` invocations**:
    - one for the first state action
    - one for the second
- These are the two points where the state can change.
- The final `run` does not introduce a new transition; it executes the second state action produced by the first.

**Option**
- `flatMap` unwraps `Some(x)` and passes `x` to a function that may fail.
- If any step returns `None`, computation stops (short-circuiting).
- There is no explicit state threading, but success vs failure can be viewed as an implicit two-state system.

---

### Summary intuition

- **Option**
    - Models *presence vs absence*
    - `map` transforms values
    - `flatMap` propagates failure
    - `flatten` removes nested structure

- **State**
    - Models *explicit state threading*
    - `map` transforms values without changing control flow
    - `flatMap` sequences stateful computations
    - `flatten` executes two state actions in order, threading state between them
