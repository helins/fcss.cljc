# Functional CSS

Advanced CSS toolkit providing:

- Browser dev environment with live generation and reloading of CSS (written using [Garden](https://github.com/noprompt/garden))
- Prevention of name clashes by design
- Aggressive CSS optimization for advanced CLJS builds:
    - Dead rule elimination, only ship what is stricly needed
    - Splitting of classes into [short-named atomic styles](https://css-tricks.com/lets-define-exactly-atomic-css) for maximum rule reuse

The end result is a similar experience to
[CSS-in-JS](https://en.wikipedia.org/wiki/CSS-in-JS) with a notable difference:
everything is effectively static and pre-computed. It is a zero cost abstraction
without any burden during runtime, quite the opposite.

Release pending.


## Acknowledgment

Special thanks to Productions Associées ASBL for funding the upcoming release of this
library.


## License

Copyright © 2021 Adam Helinski

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
