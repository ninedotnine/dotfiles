source /home/danso/dotfiles/.vimrc
source /home/danso/dotfiles/.config/nvim/colour.lua


set backupdir=~/.local/state/nvim/backups
set directory=~/.local/state/nvim/backups
set undodir=~/.local/state/nvim/undo

" this does apparently nothing
" lua vim.g.moonlight_disable_background = true
" colorscheme moonlight
" highlight Normal guibg = none

colorscheme material

" noremap <c-[> ilbrack
" noremap <c-{> ibrace
" noremap <c-(> ibpar
" noremap <c-)> irpar
" noremap <c-}> irbrace
" noremap <c-]> irbrack
" noremap <c-+> iplus
" noremap <c-*> istar
" noremap <c-=> iex
" noremap <c--> imines
" noremap <c-/> islacsh
" noremap <c-&> iand
"
" noremap <c-_> iunder

" " inoremap <c-[> <c-o>[[
" inoremap <c-{> <c-o>{
" inoremap <c-(> <c-o>(
" inoremap <c-)> <c-o>)
" inoremap <c-}> <c-o>}
" " inoremap <c-]> <c-o>]]
" inoremap <c-+> +++++
" inoremap <c-*> *****
" inoremap <c-=> =====
" inoremap <c--> -----
" inoremap <c-/> /////
" inoremap <c-&> &&&&&

" inoremap <c-_> _____

" Settings {{{
let g:InsertSingleCharacter_show_prompt_message = get(g:, "InsertSingleCharacter_show_prompt_message", 0)
let g:InsertSingleCharacter_keep_cursor_position = get(g:, "InsertSingleCharacter_keep_cursor_position", 1)
let g:InsertSingleCharacter_reuse_first_count_on_repeat = get(g:, "InsertSingleCharacter_reuse_first_count_on_repeat", 1)
" }}}

" Plug mappings {{{
nnoremap <silent><expr> <Plug>(ISC-insert-at-cursor) insert_single_character#InsertAtCursor()
nnoremap <silent><expr> <Plug>(ISC-append-at-cursor) insert_single_character#AppendAtCursor()
nnoremap <silent><expr> <Plug>(ISC-insert-at-start) insert_single_character#InsertAtStart()
nnoremap <silent><expr> <Plug>(ISC-append-at-end) insert_single_character#AppendAtEnd()

inoremap <silent> <Plug>(ISC-insert-at-start-insert-mode) <Esc>:<C-u>call insert_single_character#InsertAtStartInsertMode()<CR>
inoremap <silent> <Plug>(ISC-append-at-end-insert-mode) <Esc>:<C-u>call insert_single_character#AppendAtEndInsertMode()<CR>

nnoremap <silent><expr> <Plug>(ISC-insert-enter-at-cursor) insert_single_character#InsertEnterAtCursor()
nnoremap <silent><expr> <Plug>(ISC-append-enter-at-cursor) insert_single_character#AppendEnterAtCursor()
" }}}

" usermappings {{{
nnoremap = <Plug>(ISC-insert-at-cursor)
nnoremap ∝ <Plug>(ISC-insert-at-start)
nnoremap + <Plug>(ISC-append-at-cursor)
nnoremap † <Plug>(ISC-append-at-end)
" temporary ¶ until i fix my kbd
nnoremap ¶ <Plug>(ISC-append-at-end)
imap <c-=> <Plug>(ISC-insert-at-start-insert-mode)
imap <c-+> <Plug>(ISC-append-at-end-insert-mode)
" nmap à <Plug>(ISC-insert-enter-at-cursor)
" nmap À <Plug>(ISC-append-enter-at-cursor)
" }}}

" works with kitty? 
noremap <s-Tab> :set expandtab!<CR>
noremap <c-Tab> :tabnext #<CR>

" make nbsps appear
set conceallevel=1
syn match NBSP '\%xa0' conceal cchar=␣
syn match NarrowNBSP '\%u202F' conceal cchar=␣
set concealcursor=nc
