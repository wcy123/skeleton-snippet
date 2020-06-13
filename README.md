# skeleton-snippet
a template system based on skeleton.el

YSnippet is very good, but it takes sometime at startup, ~100ms to ~300ms approximately, depending on how many snippets you have.

skeleton-snippet is much faster at startup, it does nothing. Everything is autoloaded. It is only implemented by less than 200 lines of elisp source codes. It do not have many fancy features, like editing snippets. A snippet file is just a s-exp for `skeleton-insert`
