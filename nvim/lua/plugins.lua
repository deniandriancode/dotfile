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
  use 'ryanoasis/vim-devicons'
  use 'kien/ctrlp.vim'
  use 'preservim/nerdcommenter'
  use 'ayu-theme/ayu-vim'
  use {
	  "windwp/nvim-autopairs",
    config = function() require("nvim-autopairs").setup {} end
  }
  use 'doums/darcula'
  use 'nvim-treesitter/nvim-treesitter'
  use 'mhinz/vim-startify'
  use 'folke/which-key.nvim'
  use 'tomasiser/vim-code-dark'
  use 'joshdick/onedark.vim'
  use 'digitaltoad/vim-pug'
  use {
  "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    requires = { 
      "nvim-lua/plenary.nvim",
      "kyazdani42/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    }
  }
end)
