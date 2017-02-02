## Setup

- Prerequisite: Node.js must be installed
- Clone this Git repo and enter the directory
- `npm install` (this will take a while)
- `export PATH=$PWD/node_modules/.bin:$PATH`
- `npm run build`
- Then open `dist/index.html` in Chrome - *currently only Chrome is supported!*

## Architecture

### Goals

- Something that runs in a browser
- Type-safety, i.e. not plain JavaScript
- Prefer (pure) functional programming
- Same language on frontend and backend
- Event store for managing state updates, with the ability to replay

### Technologies

- PureScript
- Halogen for UI
- Inline SVG for graphics

### Why PureScript and not...

- **...JavaScript?** Started with it. Even with linting and TypeScript, it is hard to make JS truly safe.
- **...Elm?** Tried it. Elm apparently lacks server-side support. Also, it forces a certain architecture, and the language is limited.
- **...Scala?** Thought about it. Scala is complex, and allows purely functional programming, but not by default.
- **...Haskell?** Didn't really think about it. PureScript is a more modern and clean variant of Haskell, designed for compilation to JS.

Very interesting read: [Selecting a platform - JavaScript vs Elm vs PureScript vs GHCjs vs Scalajs](http://mutanatum.com/posts/2017-01-12-Browser-FP-Head-to-Head.html)

### Limitations

#### SVG Graphics

Tried Canvas before, but it felt useless without a third-party library like Fabric.js - Canvas is probably more powerful than SVG, but rendering SVG programmatically with Halogen is easier.

#### CSS Animations

Halogen lacks support for animations, so only CSS animations are used. The SVG standard does not support CSS animations on SVG attributes. Maybe in the future, but currently only Chrome supports this. See: [StackOverflow](http://stackoverflow.com/a/32410381/1862339)

Other options are:
- using SVG SMIL animations (with a polyfill... but apparently does not support all use-cases perfectly)
- using a JS library (with annoying interop)
