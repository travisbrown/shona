# Shona

This branch of the Shona project is a demonstration by [Travis Brown](https://twitter.com/travisbrown)
of how Scala 2.10's `def` macros can be used to create references to
singleton types. It's a little more verbose without type macros, since we need a stable identifier
in order to be able to refer to the type member:

``` scala
val id = Sing("id")
val name = Sing("name")
val venue = Sing("venue")

val venueV = Vertex[venue.T] ~ (
  int   [id.T],
  string[name.T]
)
```

Instead of the much more concise version in `master`:

``` scala
val venue = Vertex[label("venue")] ~ (
  int   [label("id")], 
  string[label("name")]
)
```

But it works more or less the same—e.g., `id.T` is exactly the same type as `label("id")`.

## Contribution Policy

Contributions via GitHub pull requests are gladly accepted from their original author.

Along with any pull requests, please state that the contribution is your original work and 
that you license the work to the project under the project's open source license.

Whether or not you state this explicitly, by submitting any copyrighted material via pull request, 
email, or other means you agree to license the material under the project's open source license and 
warrant that you have the legal authority to do so.

## License

    This software is licensed under the Apache 2 license, quoted below.

    Copyright 2013 Alois Cochard 

    Licensed under the Apache License, Version 2.0 (the "License"); you may not
    use this file except in compliance with the License. You may obtain a copy of
    the License at http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
    License for the specific language governing permissions and limitations under
    the License.
