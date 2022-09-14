return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  use 'kyazdani42/nvim-web-devicons'
  use 'tpope/vim-surround'
  use 'mattn/emmet-vim'
  use 'itchyny/lightline.vim'
  use { 'kaicataldo/material.vim', branch = 'main'  }
  use { 'neoclide/coc.nvim', branch = 'release' }
  use 'ryanoasis/vim-devicons'
  use 'kien/ctrlp.vim'
  use 'preservim/nerdcommenter'
  use 'ayu-theme/ayu-vim'
  use 'windwp/nvim-autopairs'
  use 'doums/darcula'
  use 'nvim-treesitter/nvim-treesitter'
  use 'mhinz/vim-startify'
end)
