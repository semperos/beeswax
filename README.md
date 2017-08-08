# Beeswax

Concatenative language for dynamic platforms.

Goals:

* Be trivially portable to dynamically-typed, garbage-collected platforms.
* Be interruptable. A program can run to a certain point, stop, then pick up on a different system.

## Design Thoughts

* Each word could have a set of functionality tags that indicate, at a system level, what the word does, needs access to, etc. (think GUI vs. headless environment, for example)

## License

Copyright © 2017 Daniel L. Gregoire (semperos)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
