## Proact

Proact is a set of utilities that leverage interactions with `purescipt-react` through Free programs written with `purescipt-run`. These interactions give access to a global singular state that is composable via Profunctor lenses.
Free programs separate DSLs from their side-effects which will be handled later by a separate routine at the time of execution, that is, when respoding to an event or during the initial rendering of the Proact component.

## Installation
```sh
git clone https://github.com/alvart/purescript-proact.git proact
cd proact
spago install
```

## Building

```sh
spago build
```

## Overview

Creating components in Proact is meant to be quick and simple:

1. Define the state of the component.
2. Design a GUI that renders according to the state.
3. Fetch the state and pass it on to the GUI.
4. Write event handlers that will change the state of the component or trigger external interactions encoded as extensible effects.
5. Link those event handlers to the page events as properties of other Proact components.

Once your app components are defined, integrate them to other components by `focus`ing on their state. Focusing requires the programmer to write lens functions which are usually very easy to implement for most cases.

## Examples

- [To-do Application](https://github.com/alvart/proact-todo). The "Hello World!" of web applications.
- [Chat Application](https://github.com/alvart/proact-chat). A more complex application showcasing side-effects in its components' actions.
