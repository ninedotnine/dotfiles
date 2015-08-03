" more configs in /etc/vimrc and /usr/share/vim/vimfiles

set nocompatible
set number
" set relativenumber
set t_Co=256
set background=dark

" new stuff, aug 21 2014
set ttyfast   " this might improve performance, iono
set scrolloff=5 " keep 5 lines of context above and below cursor

" colours after 80 chars
" set colorcolumn=81
" set columns=81
" whatever
" end of new stuff, aug 21 2014

" these three lines are for the solarized colour scheme
let g:solarized_termcolors = 1
let g:solarized_termtrans = 1
colorscheme solarized

highlight LineNr ctermfg=brown
" set hlsearch
if has("syntax")
    syntax on
"     set textwidth=79
else
    set spell
    "set wrapmargin=14
endif

filetype indent on
set wrap linebreak
set autoindent
set smartindent

" i think i meant "command Q q" because i wanted to quit with :Q
" it probably had nothing to do with macros
" it was probably to stop accidentally entering ex mode
" nnoremap Q q
nnoremap Y y$
nnoremap <space> :
nnoremap Q :

" command causes vim to explode when i reload $MYVIMRC
" ! forces overwrite of whatever the command was before
command! Q qall
command! W w

" these are all set in /etc/vimrc anyway...
" no they aren't
set expandtab
set tabstop=4
set sw=4

"this stuff is also set in /usr/share/vim/vimfiles/archlinux.vim
"allow backspacing over everything in insert mode
" set backspace=indent,eol,start
" Suffixes that get lower priority when doing tab completion for filenames.
" These are files we are not likely to want to edit or read.
" set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.png,.jpg

" these files are always ignored by vim because i won't ever want to edit them.
set wildignore=*.o,*.obj,*.hi,*.png,*.jpg

set ruler
" set history=20

" by default, vim keeps backup files in the same dir as the working file.
" much better to keep them somewhere else, yes?
set backupdir=~/.vim/backups
set directory=~/.vim/backups

" persistent undo, allows undoing after exiting vim
set undodir=~/.vim/undo
set undofile

" disable persistent undo for files stored in /tmp or ~/tmp
au BufWritePre /tmp/* setlocal noundofile
au BufWritePre ~/tmp/* setlocal noundofile

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd            " Show (partial) command in status line.
set showmatch          " Show matching brackets.
set ignorecase         " Do case insensitive matching
set smartcase          " Do smart case matching
set incsearch          " Incremental search
" set autowrite          " Automatically save before commands like :next and :make
" set hidden             " Hide buffers when they are abandoned
set mouse=nv            " Enable mouse usage, but not in insert mode

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
"     let fname = Chomp(system("ls -1 | dmenu -i -l 20 -p " . a:cmd))
    if empty(fname)
        return
    endif
    execute a:cmd . " " . fname
endfunction

noremap <c-t> :call DmenuOpen("tabe")<cr>
" noremap <c-e> :call DmenuOpen("e")<cr>

" automatically comment out lines 
au FileType haskell,vhdl,ada let b:comment_leader = '-- '
au FileType c,cpp,java,javascript let b:comment_leader = '// '
au FileType bash,zsh,sh,python,perl,make,conf,gitcommit let b:comment_leader = '# '
au FileType vim let b:comment_leader = '" '
au FileType tex let b:comment_leader = '% '
au FileType fortran,xdefaults let b:comment_leader = '! '
if !exists("b:comment_leader") 
    let b:comment_leader = '# ' " a sane default 
endif
noremap <silent> g/ :<C-B>sil <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
noremap <silent> g- :<C-B>sil <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>
" nnoremap g/ :<C-B>sil <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
" nnoremap g- :<C-B>sil <C-E>s/^/V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>

" these lines are to make the current line number highlighted
hi clear LineNr
hi clear CursorLine
hi clear CursorLineNR
hi LineNr ctermfg=magenta
hi CursorLineNR cterm=bolditalic ctermfg=93
set cursorline
set nocursorcolumn

" highlights for the tab bar
:hi clear TabLine
:hi clear TabLineFill
:hi clear TabLineSel
:hi TabLineSel cterm=bolditalic ctermfg=124
:hi TabLine ctermfg=brown

" different higlhights for st
if $TERM == "xterm-256color"
    set background=light
    hi clear LineNr
    hi clear CursorLine
    set hlsearch
endif


nnoremap <F4> :diffu<CR>
nnoremap <F12> :so $MYVIMRC<CR>

" save whenever focus is lost
au FocusLost * silent! wa

nnoremap <F2> :r! earlget 

" if writing mail, set the spellchecker to F7 (-e for email syntax)
autocmd FileType mail :nmap <F7> :w<CR>:!aspell -e -c %<CR>:e<CR> 
