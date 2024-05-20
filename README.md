## Dependent Hazelnut

An experimental structured editing environment for a redimentary dependently typed language, based on the theory of [Hazel](https://hazel.org/), specifically the [marked lambda calculus](https://hazel.org/papers/marking-popl24.pdf).

Usage notes: The purple circles are holes. The let bindings and functions look non-standard. Hopefully you can figure out what they are. The boxes are type universes. Use arrow keys to move, tab to go to the next hole. Type "fun," "let," or "typ," then press space, to create a function, let-binding, or type universe. Type "=" to create an arrow type. Press space to create a function application. Press ctrl+r to refine a hole into a function abstraction if the goal is an arrow. I apologize to anyone who actually tries to move the cursor around in the current state.

UI Screenshot:
![image](https://github.com/thomasporter522/dependent-hazelnut/assets/22896135/5452ab5b-bb51-43f6-967e-faadd0b92ce9)

Todos:
- Complete implementing De Bruijn indices
- Eta equivalence
- Basic type hole inference
- Fix Type : Type

- Better editing flow
- Better visuals

- Additional type theoretic features
