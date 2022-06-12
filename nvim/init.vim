lua require('plugins')

set number
set relativenumber
set mouse=a
set tabstop=2
set shiftwidth=2
set splitright
set splitbelow
syntax on

filetype plugin indent on
filetype indent on

" Default value is clap
let g:dashboard_default_executive ='telescope'

let g:user_emmet_leader_key=','

nnoremap <C-o> :NeoTreeShowToggle<CR>
inoremap jk <ESC>

colorscheme gruvbox

let g:onedark_terminal_italics = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:doom_one_terminal_colors = v:true
