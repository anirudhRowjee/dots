-- Configure plugins
local plugins = {

  { "max397574/better-escape.nvim", enabled = false },

  {
    "ray-x/lsp_signature.nvim",
    lazy = false,
    event = "BufRead",
    config = function() require("lsp_signature").setup() end,
  },

  {
    "ivanjermakov/troublesum.nvim",
    config = function()
      require("troublesum").setup {
        enabled = true,
        autocmd = true,
        severity_format = { "E", "W", "I", "H" },
        severity_highlight = { "DiagnosticError", "DiagnosticWarn", "DiagnosticInfo", "DiagnosticHint" },
        format = function(counts) end,
        display_summary = function(bufnr, ns, text) end,
      }
    end,
    lazy = false,
  },

  -- Colorschemes
  {
    "ellisonleao/gruvbox.nvim",
    lazy = false,
    config = function()
      require("gruvbox").setup {
        underline = true,
        italic = {
          strings = true,
          comments = true,
          operators = false,
          folds = true,
        },
        strikethrough = true,
        invert_selection = false,
        invert_signs = false,
        invert_tabline = false,
        invert_intend_guides = false,
        inverse = true, -- invert background for search, diffs, statuslines and errors
        contrast = "hard", -- can be "hard", "soft" or empty string
        dim_inactive = false,
        transparent_mode = false,
      }
    end,
  },

  { "TimUntersberger/neogit", lazy = false, dependencies = { "nvim-lua/plenary.nvim" } },

  {
    "Shatur/neovim-ayu",
    lazy = false,
    config = function()
      require("ayu").setup {
        mirage = false, -- Set to `true` to use `mirage` variant instead of `dark` for dark background.
        overrides = {}, -- A dictionary of group names, each associated with a dictionary of parameters (`bg`, `fg`, `sp` and `style`) and colors in hex.
      }
    end,
  },

  {
    "folke/tokyonight.nvim",
    lazy = false,
    config = function()
      require("tokyonight").setup {
        style = "night",
        on_colors = function(colors)
          colors.bg = "#0d0e12"
          colors.bg_dark = "#0d0e12"
          colors.bg_highlight = "#0d0e12"
        end,
      }
    end,
  },

  -- Add Trouble
  { "folke/trouble.nvim", config = function() require("trouble").setup() end },

  -- Sessions and Workspaces
  {
    "natecraddock/sessions.nvim",
    config = function()
      require("sessions").setup {
        events = { "VimLeavePre" },
        session_filepath = ".nvim/session",
      }
    end,
    lazy = false,
  },

  {
    "natecraddock/workspaces.nvim",
    config = function()
      require("workspaces").setup {
        hooks = {
          open = function() require("sessions").load(".nvim/session", { silent = true }) end,
        },
      }
    end,
    lazy = false,
  },

  {
    "folke/zen-mode.nvim",
    config = function()
      require("zen-mode").setup {
        window = {
          width = 0.8,
          height = 0.8,
          options = {
            signcolumn = "no",
            number = false,
            relativenumber = false,
            cursorline = false,
            cursorcolumn = false,
            foldcolumn = "0",
          },
        },
      }
    end,
    lazy = false,
  },

  { "folke/twilight.nvim", config = function() require("twilight").setup() end, lazy = false },

  { "dnlhc/glance.nvim", config = function() require("glance").setup {} end, lazy = false },

  { "dhruvasagar/vim-table-mode" },
  { "jbyuki/venn.nvim" },
  { "iamcco/markdown-preview.nvim" },
  { "rust-lang/rust.vim" },
  { "simrat39/rust-tools.nvim" },

  {
    "olexsmir/gopher.nvim",
    requires = { -- dependencies
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    lazy = false,
    config = function()
      require("gopher").setup {
        commands = {
          go = "go",
          gomodifytags = "gomodifytags",
          gotests = "~/go/bin/gotests", -- also you can set custom command path
          impl = "impl",
          iferr = "iferr",
        },
      }
    end,
  },

  { "shortcuts/no-neck-pain.nvim", lazy = false },

  {
    -- override nvim-autopairs plugin
    "hrsh7th/nvim-cmp",
    -- override the options table that is used in the `require("cmp").setup()` call
    opts = function(_, opts)
      -- opts parameter is the default options table
      -- the function is lazy loaded so cmp is able to be required
      local cmp = require "cmp"
      -- modify the sources part of the options table
      opts.sources = cmp.config.sources {
        { name = "nvim_lsp", priority = 1000 },
        { name = "luasnip", priority = 750 },
        { name = "buffer", priority = 500 },
        { name = "path", priority = 250 },
      }
      -- return the new table to be used
      return opts
    end,
  },

  {
    "rebelot/heirline.nvim",
    opts = function(_, opts)
      local status = require "astronvim.utils.status"

      opts.statusline = {
        -- statusline
        hl = { fg = "fg", bg = "bg" },
        -- when adding the mode component, enable the mode text with padding to the left/right of it
        status.component.mode { mode_text = { padding = { left = 1, right = 1 } } },
        status.component.git_branch(),
        status.component.file_info { padding = { left = 1, right = 1 } },
        status.component.git_diff(),
        status.component.diagnostics(),
        status.component.fill(),
        status.component.cmd_info(),
        status.component.fill(),
        status.component.lsp(),
        status.component.treesitter(),
        status.component.nav(),
        status.component.mode { surround = { separator = "right" } },
      }

      opts.winbar = {
        -- winbar
        init = function(self) self.bufnr = vim.api.nvim_get_current_buf() end,
        fallthrough = false,
        {
          -- inactive winbar
          condition = function() return not status.condition.is_active() end,
          status.component.separated_path(),
          status.component.file_info {
            file_icon = { hl = status.hl.file_icon "winbar", padding = { left = 0 } },
            file_modified = false,
            file_read_only = false,
            hl = status.hl.get_attributes("winbarnc", true),
            surround = false,
            update = "BufEnter",
          },
        },
        { -- active winbar
          status.component.breadcrumbs { hl = status.hl.get_attributes("winbar", true) },
        },
      }

      opts.tabline = { -- tabline
        {
          -- file tree padding
          condition = function(self)
            self.winid = vim.api.nvim_tabpage_list_wins(0)[1]
            return status.condition.buffer_matches(
              { filetype = { "aerial", "dapui_.", "neo%-tree", "NvimTree" } },
              vim.api.nvim_win_get_buf(self.winid)
            )
          end,
          provider = function(self) return string.rep(" ", vim.api.nvim_win_get_width(self.winid) + 1) end,
          hl = { bg = "tabline_bg" },
        },
        status.heirline.make_buflist(status.component.tabline_file_info()), -- component for each buffer tab
        status.component.fill { hl = { bg = "tabline_bg" } }, -- fill the rest of the tabline with background color
        {
          -- tab list
          condition = function() return #vim.api.nvim_list_tabpages() >= 2 end, -- only show tabs if there are more than one
          status.heirline.make_tablist { -- component for each tab
            provider = status.provider.tabnr(),
            hl = function(self) return status.hl.get_attributes(status.heirline.tab_type(self, "tab"), true) end,
          },
          {
            -- close button for current tab
            provider = status.provider.close_button { kind = "TabClose", padding = { left = 1, right = 1 } },
            hl = status.hl.get_attributes("tab_close", true),
            on_click = {
              callback = function() require("astronvim.utils.buffer").close_tab() end,
              name = "heirline_tabline_close_tab_callback",
            },
          },
        },
      }

      opts.statuscolumn = { -- statuscolumn
        status.component.foldcolumn(),
        status.component.fill(),
        status.component.numbercolumn(),
        status.component.signcolumn(),
      }

      -- return the final configuration table
      return opts
    end,
  },

  -- More Testing support (actually, better testing support)
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-neotest/neotest-go",
    },
    lazy = false,
    config = function()
      -- get neotest namespace (api call creates or returns namespace)
      local neotest_ns = vim.api.nvim_create_namespace "neotest"
      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)

      require("neotest").setup {
        adapters = {
          -- require "neotest-plenary",
          require "neotest-go",
        },
      }
    end,
  },

  -- More Colorschemes
  {
    "AlexvZyl/nordic.nvim",
    lazy = false,
    priority = 1000,
    config = function() require("nordic").load() end,
  },
  {
    "cpea2506/one_monokai.nvim",
    lazy = false,
    config = function()
      require("one_monokai").setup {
        transparent = false,
        colors = {},
        themes = function(colors)
          return {
            Normal = { bg = colors.bg:darken(0.2) },
            -- Normal = { bg = "#141414"},
            VertSplit = { bg = colors.NONE, fg = colors.black },
            FoldColumn = { bg = colors.NONE, fg = colors.NONE },
          }
        end,
        italics = true,
      }
    end,
  },
  {
    "ray-x/starry.nvim",
    lazy = false,
    config = function()
      vim.cmd [[
      let starry_darker_contrast=v:true  
    ]]
    end,
  },
  -- Lazy
  {
    "polirritmico/monokai-nightasty.nvim",
    lazy = false,
    priority = 1000,
  },
}

return plugins
