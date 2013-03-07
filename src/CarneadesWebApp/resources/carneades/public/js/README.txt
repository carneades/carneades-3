For historical reason the client implementation of the application
is done in a mix of JavaScript and ClojureScript.

At first the application was in JavaScript, then in JavaScript with
Backbone.

Most of the new code is now written in ClojureScript but lot of code
from each 'period' still remain and was not rewritten.

Some macros were written to use Backbone from ClojureScript but
finally this bring more trouble than anything else because of the
mismatch between the functional/immutable approach of ClojureScript
and the object/mutable approach of Backbone.

Looking at clojurescriptone.org the simple dispatch.cljs file offers
50% of what Backbone provides with less trouble.

Therefore I think the cleanest the solution is to now use exclusively
ClojureScript and dispatch.cljs.

This solution is used with success in questions.cljs which has
a complex logic. I would recommand to use this approach in combination
with the current template engine (ICanHaz) for further development.

It would also makes sense in the long term to get rid of JSON on the
client and use EDN.
