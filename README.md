# skeleton-snippet
a template system based on skeleton.el

YSnippet is very good but it takes some time at startup, 100ms
to 300ms approximately, depending on how many snippets you have.

skeleton-snippet is much faster at startup, it does
nothing. Everything is autoloaded. It is only implemented by less than
200 lines of elisp source codes. It does not have many fancy features,
like editing snippets. A snippet file is just a s-expression for
`skeleton-insert`, it is not a good news if you don't like
s-expression.


## Where are the snippets?

`skeleton-snippet` does not bundles snippets directly, you can get it
to from https://github.com/wcy123/skeleton-snippet-store.
