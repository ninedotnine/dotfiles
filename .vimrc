set nocompatible
let g:is_posix = 1
set relativenumber
set number
set ruler
" set rulerformat=%49(%=%n:%<%t%y%r%m%h%w\ %-10.19(%l,%c%V,%o%)\ %P%)
set rulerformat=%=%5.16(%l,%v,%o%)\ %P
set laststatus=1
" set statusline=%n:%f\ %y%r%m%=%h%w\ %-10.19(%l,%c%V,%o%)\ %P
set statusline=%n:%f\ %w%y%r%m%=%5.16(%l,%v,%o%)\ %P
set shortmess=filnrxtoOsTf
set scrolloff=3 " keep 3 lines of context above and below cursor
set sidescrolloff=3

" scroll 8 lines with ctrl-d
set scroll=7

set display+=lastline,truncate

let mapleader='¿' " i don't really use this, but i gotta change it from '\'

set lazyredraw  " do not redraw screen while executing a macro

set t_Co=256
set background=dark

filetype indent on
set wrap linebreak
let &showbreak = "»»"
set autoindent
set smartindent

set expandtab
set tabstop=4
set shiftwidth=4

augroup tabstuff
    autocmd!
    autocmd FileType make setlocal noexpandtab   " makefiles hate spaces.
    autocmd FileType souc,surc setlocal noexpandtab   " souc uses tabs too.
    autocmd FileType javascript,typescript,html,htmlm4,css setlocal tabstop=2
    autocmd FileType javascript,typescript,html,htmlm4,css setlocal shiftwidth=2
augroup end

set wildmenu           " better command-line completion
set showcmd            " Show (partial) command in status line.
set showmatch          " Show matching brackets.
set ignorecase         " Do case insensitive matching
set smartcase          " Do smart case matching
set incsearch          " Incremental search
set mouse=nv            " Enable mouse usage, but not in insert mode
set nostartofline       " don't jump to column 0 when possible

set clipboard+=unnamedplus  " also delete, yank, paste from the clipboard

set nrformats-=octal  " don't skip 8 and 9 when incrementing

set nomodeline          " don't try to read vim settings from a file

set formatoptions+=j   " remove a comment leader when joining lines.
set formatoptions+=o   " insert the comment leader after hitting 'o'

set wildmode=longest,list,full          " better filename tab completion
" set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.

set history=300

" set ttyfast             " this might improve performance, iono
" set ttimeoutlen=100     " this might fix the slow O problem

set spelllang=en_us

set hlsearch            " keep matches highlighted after searching
" Clear highlighting from screen
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>

" start empty files in insert mode
" autocmd BufNewFile * start

autocmd BufNewFile  *.c 0r ~/dotfiles/skeletons/skeleton.c
autocmd BufNewFile  *.h 0r ~/dotfiles/skeletons/skeleton.h
autocmd BufNewFile  *.py 0r ~/dotfiles/skeletons/skeleton.py
autocmd BufNewFile  *.hs 0r ~/dotfiles/skeletons/skeleton.hs

if has('nvim')
" in vim, <c-m> is the same as return.
" nvim with a compatible terminal (kitty) should be able to use <c-m>.
"     inoremap <c-m> :! make<CR>
"     nnoremap <c-m> :! make<CR>
endif

" colours after 80 chars
" set colorcolumn=81
" set columns=81
" whatever
" end of new stuff, aug 21 2014

" these three lines are for the solarized colour scheme
let g:solarized_termcolors = 256
let g:solarized_termtrans = 1
colorscheme solarized

" if has("syntax")
"     syntax on
"     set foldmethod=syntax " automatic folding
" "     set textwidth=79
" else
"     set spell
"     set foldmethod=indent " automatic folding
" "     "set wrapmargin=14
" endif

set foldlevel=100
highlight Folded ctermbg=238 ctermfg=250

function! FoldMethod(lnum)
    let l:indent_this = indent(a:lnum)
"     let l:indent_prev = indent(a:lnum-1)
    let l:indent_next = indent(a:lnum+1)
"     if l:indent_next < l:indent_this
"         let l:level = l:indent_this / &shiftwidth
"         return '<' .. l:level
"     elseif l:indent_this < l:indent_prev
"         let l:level = l:indent_this / &shiftwidth
"         return '>' .. l:level
"     endif
    let l:indent_max = max([indent_this, indent_next])
    return l:indent_max / &shiftwidth
endfunction

function! FoldText()
    let line = getline(v:foldstart)
    let size = v:foldend - v:foldstart
    return '÷' .. size .. v:folddashes .. ' ' .. line .. ' '
endfunction

set foldmethod=expr
set foldexpr=FoldMethod(v:lnum)
set foldtext=FoldText()

" augroup foldstuff
"     autocmd!
"     au FileType haskell setlocal foldmethod=indent
" augroup end


" c-specific highlighting options
let c_gnu = 1
let c_comment_strings = 1
let c_space_errors = 1
augroup cstuff
    autocmd!
    au FileType c,cpp,java,javascript,typescript syntax keyword Debug assert
augroup end

" for the haskell.vim syntax highlighting, highlight things like "undefined"
let hs_highlight_debug = "yes"

" make `cw` behave like `dwi` instead of like `ce`
set cpoptions-=_

nnoremap Y y$
nnoremap <space> :

" visual up and down, unless a count is used
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')

" my keyboard has a "Qu" key instead of "Q"
nnoremap Qu Q
nnoremap qu q

inoremap <KEnter> ;<CR>
inoremap <S-CR> ;<CR>

inoremap <C-CR> <esc>
inoremap <C-space> <esc>

nnoremap , ;
nnoremap ; ,

noremap ` '
noremap ' `

noremap _ ^

vnoremap « <
vnoremap » >
vnoremap « <gv
vnoremap » >gv

" make « and » take a count of how many shiftwidths
nnoremap <expr> <silent> » (v:count > 0 ? "@='>>'<CR>" : '>>')
nnoremap <expr> <silent> « (v:count > 0 ? "@='<<'<CR>" : '<<')

inoremap <c-'> <c-d>
inoremap <c-"> <c-t>

" insert a (s)ingle character
nnoremap <silent>= :exec "normal i".nr2char(getchar())."\e"<CR>
nnoremap <silent>∝ :exec "normal a".nr2char(getchar())."\e"<CR>
nnoremap <silent>+ :exec "normal i".nr2char(getchar())."\e"<CR>
nnoremap <silent>† :exec "normal a".nr2char(getchar())."\e"<CR>

" insert a blank line above or below the current line
nnoremap é :set paste<CR>m`o<Esc>``h:set nopaste<CR>
nnoremap à :set paste<CR>m`O<Esc>``:set nopaste<CR>

" delete the line above or below if it is blank
nnoremap <silent>É m`:silent +g/\m^\s*$/d<CR>``h:noh<CR>
nnoremap <silent>À m`:silent -g/\m^\s*$/d<CR>``h:noh<CR>

" bring text until end-of-line to a new line below or above
nnoremap Ô d$O<esc>p0
nnoremap ô i<cr><esc>

" delete all characters on a line, leaving the line blank
nnoremap dc cc<esc>

if has('nvim')
    " this version doesn't seem to work?
    " map("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection DOWN" })
    " map("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection UP" })
    vnoremap J :m '>+1<CR>gv=gv
    vnoremap K :m '<-2<CR>gv=gv
endif

noremap · #
noremap × #

noremap \ ?
noremap ÷ ?

nnoremap <c-h> <c-u>

nnoremap <c-s> :w<CR>
nnoremap <c-q> :x<CR>

inoremap <c-l> <c-x><c-l>

inoremap <c-j> <Down>
inoremap <c-k> <Up>

nnoremap <c-j> <PageDown>
nnoremap <c-k> <PageUp>

noremap <c-b> <c-x>

" disable i_0_CTRL-D and i_^_CTRL-D
" unsatisfyingly, this breaks . repetition.
" iunmap 0<c-d>
" inoremap 0<c-d> <Esc>:set timeoutlen=1<CR>a0<Esc>:set timeoutlen=1000<CR>a
" inoremap 0<c-d> <Esc>a0<Esc>a
" inoremap <nowait> 0 0<esc>a
" inoremap <nowait> ^ ^<esc>a
" iunmap ^<c-d>

nnoremap <c-n> gt
nnoremap <c-p> gT

" make deleting words/lines in insert mode undoable
inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>

" command causes vim to explode when i reload $MYVIMRC
" ! forces overwrite of whatever the command was before
command! Q qall
" command! W w
cabbr W w
"  how is cabbr different?

" go to [count] next unmatched ']'  or '['
noremap <silent> ]b :call searchpair('\[','','\]')<cr>
noremap <silent> [b :call searchpair('\[','','\]','b')<cr>


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
    augroup undostuff
        autocmd!
        au BufWritePre /tmp/* setlocal noundofile
        au BufWritePre ~/tmp/* setlocal noundofile
    augroup end
endif

" compile and display a latex file
" noremap <c-b> :! pdflatexandevince % <CR> <CR>

" abbreviations, these are pretty cool
abbreviate #d #define
abbreviate #i #include

" these two functions are to integrate dmenu
" thanks dude: http://leafo.net/posts/using_dmenu_to_open_quickly.html
" Strip the newline from the end of a string
" function! Chomp(str)
"     return substitute(a:str, '\n$', '', '')
" endfunction
" " Find a file and pass it to cmd
" function! DmenuOpen(cmd)
"     let fwindow = Chomp(system("xdotool getwindowfocus"))
"     let fname = Chomp(system("find . -path ./.git -prune -o -print | dmenu -i -l 20 -p " . a:cmd))
" "     let fname = Chomp(system("ls -A1 | dmenu -i -l 20 -p " . a:cmd))
"     let unused = system("xdotool windowfocus " . fwindow)
"     if empty(fname)
"         return
"     endif
"     execute a:cmd . " " . fname
" endfunction

" noremap <c-t> :call DmenuOpen("tabe")<cr>
" " noremap <c-e> :call DmenuOpen("e")<cr>

" noremap <c-t> :call fzf#run({'sink': 'tabnew', 'options': "--prompt '» ' --pointer '»'", 'down': '20%', 'left': '40%'})<cr>
noremap <silent> <c-t> :call fzf#run({'sink': 'tabnew', 'options': "--prompt '» ' --pointer '»'", 'window': { 'width': 0.7, 'height': 0.5 }})<cr>

" open file under cursor in new tab
noremap gf gf

augroup helpstuff
    autocmd!
    " open help in a vertical split on the left
    autocmd FileType help wincmd L
augroup end

augroup jumpstuff
    " jump to last cursor position unless it's invalid or in an event handler
    autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
            \ | exe "normal! g`\""
        \ | endif
augroup end

augroup commentstuff
    autocmd!
    " automatically comment out lines
    au FileType haskell,vhdl,ada,lua let b:comment_leader = '-- '
    au FileType c,cpp,java,d,javascript,typescript,rust,go let b:comment_leader = '// '
    au FileType bash,zsh,sh,python,perl,ruby,make,conf,gitcommit,muttrc let b:comment_leader = '# '
    au FileType vim let b:comment_leader = '" '
    au FileType tex let b:comment_leader = '% '
    au FileType fortran,xdefaults let b:comment_leader = '! '
    au FileType souc,surc let b:comment_leader = '; '
augroup end
if !exists("b:comment_leader")
    let b:comment_leader = '# ' " a sensible default
endif
noremap <silent> g/ :<C-B>sil <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
noremap <silent> g- :<C-B>sil <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>
" nnoremap g/ :<C-B>sil <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
" nnoremap g- :<C-B>sil <C-E>s/^/V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>

" highlights
hi MatchParen ctermfg=cyan ctermbg=240
hi Search ctermfg=7 ctermbg=8
hi CurSearch ctermfg=0 ctermbg=11

" highlight current line number
hi clear LineNr
hi clear CursorLine
hi clear CursorLineNR
hi clear StatusLineNC
hi StatusLineNC ctermbg=238
hi LineNr ctermfg=141
hi CursorLineNR cterm=bold,italic ctermfg=93
set cursorline
set nocursorcolumn

" highlights for the tab bar
" hi clear TabLine
" hi clear TabLineFill
" hi clear TabLineSel
hi TabLineSel cterm=bold,italic ctermfg=124
hi TabLine ctermfg=brown

set tabpagemax=15   " limit number of tabs open at once

" different higlhights for st
if $TERM == "xterm-256color"
    set background=light
    hi clear LineNr
    hi clear CursorLine
    set hlsearch
endif

" press K to load a man page
runtime ftplugin/man.vim

augroup effkeys
    autocmd!
    " append current date/time
    nnoremap <F2> a<C-R>=strftime("%c")<CR><Esc>
    nnoremap <F4> :diffu<CR>
    nnoremap <F5> :! make<CR>
    inoremap <F5> <c-o>:! make<CR>
    nnoremap <silent> <F6> :call ToggleLineNumbers()<CR>
    nnoremap <silent> <F7> :lua require('material.functions').toggle_style()<CR>
    " if writing mail, set the spellchecker to F7 (-e for email syntax)
    autocmd FileType mail :nnoremap <F7> :w<CR>:!aspell -e -c %<CR>:e<CR>
    " delete spaces at end-of-line
    nnoremap <F8> :%s/\s\+$//<CR>
    nnoremap <F9> =
    vnoremap <F9> =
    " toggle search highlight
    nnoremap <F10> :set hlsearch! hlsearch?<CR>
    nnoremap <F12> :so $MYVIMRC<CR> :nohlsearch<CR><C-l>
augroup end

function! ToggleLineNumbers()
    set relativenumber!
    set number!
endfunction

augroup savingstuff
    autocmd!
    " save whenever focus is lost
    au FocusLost * silent! wa

    " before saving, delete spaces on otherwise empty lines
    autocmd BufWritePre * silent! :%s/^\s\+$//

    autocmd FileType c,cpp,java,d,go,php,javascript,typescript,puppet,python,rust,twig,xml,yml,perl,sql,haskell autocmd BufWritePre <buffer> if !exists('g:spf13_keep_trailing_whitespace') | call StripTrailingWhitespace() | endif

    " disallow writing to files named "w"
    autocmd BufWriteCmd w :w % | echo 'not writing to w' | set nomodified
augroup end


augroup columnstuff
    autocmd!
    " if writing mail, set the spellchecker to F7 (-e for email syntax)
    autocmd FileType mail :nnoremap <F7> :w<CR>:!aspell -e -c %<CR>:e<CR>

    " email, git commits - wrap at 68 for future quoting, enable spelling
    au FileType mail setlocal tw=71 colorcolumn=72 spell
    au FileType gitcommit setlocal tw=68 colorcolumn=69 spell

    " useful for mutt with `text_flowed=yes` in .muttrc
    autocmd FileType mail setlocal formatoptions+=w
augroup end

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

" for rainbow parens
" let g:rainbow_conf = { 'ctermfgs': [ 'darkmagenta', 'blue', 'darkcyan', 'darkred', 'darkblue' ] }
let g:rainbow_conf = { 'ctermfgs': ['darkyellow', 'darkcyan', 'darkgray', 'darkblue', 'darkmagenta', 'magenta'] }
let g:rainbow_active = 1
