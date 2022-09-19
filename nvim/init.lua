local set = vim.opt

-- mapper function
function map(mode, lhs, rhs, opts)
  local options = { noremap = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.g.maplocalleader = " "
vim.g.user_emmet_leader_key = ","

set.compatible = false
set.tabstop = 8
set.shiftwidth = 8
set.softtabstop = 8
set.expandtab = true
set.scrolloff = 5
set.background = "dark"
set.swapfile = false
set.autoindent = true
set.wrap = false
set.number = true
set.relativenumber = true
set.termguicolors = true
set.encoding = "UTF-8"
set.cursorline = true
set.splitbelow = true
set.splitright = true
set.colorcolumn = { 120 }
set.showcmd = true
set.showmatch = true
set.ignorecase = true
set.ttyfast = true

vim.cmd("filetype indent on")
vim.cmd("filetype plugin on")
vim.cmd("syntax on")
vim.cmd("colorscheme darcula")
vim.cmd("autocmd TermOpen * set nonumber norelativenumber")

map("n", "<localleader>s", ":w<CR>")
map("n", "<localleader>nh", ":noh<CR>")
map("n", "<localleader>q", "@q")
map("n", "<leader>f", ":Expl<CR>")
map("n", "<leader>tn", ":tabnew<CR>")
map("n", "<leader>td", ":tabclose<CR>")
map("n", "<leader>tt", ":term<CR>")
map("n", "<leader>Tb", ":Telescope buffers<CR>")
map("n", "<leader>Tf", ":Telescope fd<CR>")
map("n", "<leader>Tm", ":Telescope man_pages<CR>")
map("n", "<leader>Th", ":Telescope help_tags<CR>")
map("n", "<leader>d", ":Startify<CR>")

map("i", "jk", "<ESC>")

map("t", "<ESC>", "<C-\\><C-n>")

require('plugins')  -- plugin manager
require("nvim-autopairs").setup {}  -- autopairs
require("which-key").setup {} -- which key
-- treesitter
require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  ensure_installed = { "c", "cpp", "lua", "rust", "python", "bash", "vim" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  auto_install = true,

  -- List of parsers to ignore installing (for "all")
  ignore_install = { "javascript" },

  ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
  -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
    -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
    -- the name of the parser)
    -- list of language that will be disabled
    --disable = { "c", "rust" },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}

