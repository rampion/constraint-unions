let &makeprg="cabal"
nnoremap <Leader>m :make build<CR>
nnoremap <Leader>t :make test --show-details=always<CR>
nnoremap <Leader>d :!pandoc -f markdown_github -i README.md -o README.html -s<CR>

" recognize test error locations
let &errorformat="### Failure in %f:%l: %m," . &errorformat

" highlight haskell blocks in markdown files
let g:markdown_fenced_languages=['haskell']
