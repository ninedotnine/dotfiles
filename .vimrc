" more configs in /etc/vimrc and /usr/share/vim/vimfiles

set nocompatible
set relativenumber
set number
set ruler
set rulerformat=%49(%=%n:%<%t%y%r%m%h%w\ %-10.19(%l,%c%V,%o%)\ %P%)
set laststatus=1
set statusline=%n:%f\ %y%r%m%=%h%w\ %-10.19(%l,%c%V,%o%)\ %P
set scrolloff=5 " keep 5 lines of context above and below cursor

set t_Co=256
set background=dark

filetype indent on
set wrap linebreak
set autoindent
set smartindent

set expandtab
set tabstop=4 
set shiftwidth=4
autocmd FileType make setlocal noexpandtab   " makefiles hate spaces.

set wildmenu           " better command-line completion
set showcmd            " Show (partial) command in status line.
set showmatch          " Show matching brackets.
set ignorecase         " Do case insensitive matching
set smartcase          " Do smart case matching
set incsearch          " Incremental search
set mouse=nv            " Enable mouse usage, but not in insert mode
set nostartofline       " don't jump to column 0 when possible

set nomodeline          " don't try to read vim settings from a file

set formatoptions+=j   " remove a comment leader when joining lines. 
set formatoptions+=o   " insert the comment leader after hitting 'o'

set wildmode=longest,list,full          " better filename tab completion
" set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.

set history=20

set ttyfast             " this might improve performance, iono
set ttimeoutlen=100     " this might fix the slow O problem

set spelllang=en_us
set spellfile=~/.vim/spell.add           " my goodwords

set hlsearch            " keep matches highlighted after searching
" Clear highlighting from screen
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>

" colours after 80 chars
" set colorcolumn=81
" set columns=81
" whatever
" end of new stuff, aug 21 2014

" these three lines are for the solarized colour scheme
let g:solarized_termcolors = 1
let g:solarized_termtrans = 1
colorscheme solarized

if has("syntax")
    syntax on
    set foldmethod=syntax " automatic folding
"     set textwidth=79
else
    set spell
    set foldmethod=indent " automatic folding
"     "set wrapmargin=14
endif

set foldlevel=100

" c-specific highlighting options
let c_gnu = 1
let c_comment_strings = 1
let c_space_errors = 1
au FileType c,cpp,java,javascript syntax keyword Debug assert

" for the haskell.vim syntax highlighting, highlight things like "undefined"
let hs_highlight_debug = "yes"

nnoremap Y y$
nnoremap <space> :
nnoremap Q :

" command causes vim to explode when i reload $MYVIMRC
" ! forces overwrite of whatever the command was before
command! Q qall
" command! W w
cabbr W w       
"  how is cabbr different?

"this stuff is also set in /usr/share/vim/vimfiles/archlinux.vim
"allow backspacing over everything in insert mode
set backspace=indent,eol,start
" Suffixes that get lower priority when doing tab completion for filenames.
" These are files we are not likely to want to edit or read.
" set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.png,.jpg

" these files are always ignored by vim because i won't ever want to edit them.
set wildignore=*.o,*.obj,*.hi,*.png,*.jpg


" by default, vim keeps backup files in the same dir as the working file.
" much better to keep them somewhere else, yes?
set backupdir=~/.vim/backups
set directory=~/.vim/backups

" persistent undo, allows undoing after exiting vim
if has('persistent_undo')
    set undodir=~/.vim/undo
    set undofile

    " disable persistent undo for files stored in /tmp or ~/tmp
    au BufWritePre /tmp/* setlocal noundofile
    au BufWritePre ~/tmp/* setlocal noundofile
endif

" compile and display a latex file
" noremap <c-b> :! pdflatexandevince % <CR> <CR>

" abbreviations, these are pretty cool
abbreviate #d #define
abbreviate #i #include
abbreviate Wall {-# OPTIONS_GHC -Wall #-}
abbreviate LANGUAGE {-# LANGUAGE #-}

" these two functions are to integrate dmenu
" thanks dude: http://leafo.net/posts/using_dmenu_to_open_quickly.html
" Strip the newline from the end of a string
function! Chomp(str)
    return substitute(a:str, '\n$', '', '')
endfunction

" open file under cursor in new tab
noremap gf gf

" Find a file and pass it to cmd
function! DmenuOpen(cmd)
    let fname = Chomp(system("find . | dmenu -i -l 20 -p " . a:cmd))
"     let fname = Chomp(system("ls -A1 | dmenu -i -l 20 -p " . a:cmd))
    if empty(fname)
        return
    endif
    execute a:cmd . " " . fname
endfunction

noremap <c-t> :call DmenuOpen("tabe")<cr>
" noremap <c-e> :call DmenuOpen("e")<cr>

" open help in a vertical split on the left
autocmd FileType help wincmd L

" jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
        \ | exe "normal! g`\""
    \ | endif


" automatically comment out lines 
au FileType haskell,vhdl,ada let b:comment_leader = '-- '
au FileType c,cpp,java,javascript let b:comment_leader = '// '
au FileType bash,zsh,sh,python,perl,make,conf,gitcommit,muttrc let b:comment_leader = '# '
au FileType vim let b:comment_leader = '" '
au FileType tex let b:comment_leader = '% '
au FileType fortran,xdefaults let b:comment_leader = '! '
au FileType souc let b:comment_leader = '; '
if !exists("b:comment_leader") 
    let b:comment_leader = '# ' " a sane default 
endif
noremap <silent> g/ :<C-B>sil <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
noremap <silent> g- :<C-B>sil <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>
" nnoremap g/ :<C-B>sil <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
" nnoremap g- :<C-B>sil <C-E>s/^/V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>

" highlight current line number 
hi clear LineNr
hi clear CursorLine
hi clear CursorLineNR
hi LineNr ctermfg=magenta
hi CursorLineNR cterm=bolditalic ctermfg=93
set cursorline
set nocursorcolumn

" highlights for the tab bar
hi clear TabLine
hi clear TabLineFill
hi clear TabLineSel
hi TabLineSel cterm=bolditalic ctermfg=124
hi TabLine ctermfg=brown

" different higlhights for st
if $TERM == "xterm-256color"
    set background=light
    hi clear LineNr
    hi clear CursorLine
    set hlsearch
endif

nnoremap <F5> :diffu<CR>
set pastetoggle=<F11>
nnoremap <F12> :so $MYVIMRC<CR>

" save whenever focus is lost
au FocusLost * silent! wa

" before saving, delete spaces on otherwise empty lines
autocmd BufWritePre * silent! :%s/^\s\+$//

nnoremap <F2> :r! earlget 

" if writing mail, set the spellchecker to F7 (-e for email syntax)
autocmd FileType mail :nnoremap <F7> :w<CR>:!aspell -e -c %<CR>:e<CR> 

" email, git commits - wrap at 68 for future quoting, enable spelling
au FileType mail setlocal tw=79 colorcolumn=80 spell
au FileType gitcommit setlocal tw=68 colorcolumn=69 spell

" disable annoying behavior where starting an auto-indented line with a hash
" makes it unindent and refuse to >>
:inoremap # #

function! StripTrailingWhitespace()
    " preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " do the business:
    %s/\s\+$//e
    " clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

autocmd FileType c,cpp,java,go,php,javascript,puppet,python,rust,twig,xml,yml,perl,sql autocmd BufWritePre <buffer> if !exists('g:spf13_keep_trailing_whitespace') | call StripTrailingWhitespace() | endif
