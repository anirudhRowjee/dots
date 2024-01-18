-- Anirudh Rowjee's AstroNvim Config
-- Tuesday 26 September 2023 10:32:37 PM IST

local config = {

  -- Configure AstroNvim updates
  updater = {
    remote = "origin", -- remote to use
    channel = "stable", -- "stable" or "nightly"
    version = "latest", -- "latest", tag name, or regex search like "v1.*" to only do updates before v2 (STABLE ONLY)
    branch = "main", -- branch name (NIGHTLY ONLY)
    commit = nil, -- commit hash (NIGHTLY ONLY)
    pin_plugins = nil, -- nil, true, false (nil will pin plugins on stable only)
    skip_prompts = false, -- skip prompts about breaking changes
    show_changelog = true, -- show the changelog after performing an update
    auto_reload = true, -- automatically reload and sync packer after a successful update
    auto_quit = false, -- automatically quit the current session after a successful update
    -- remotes = { -- easily add new remotes to track
    --   ["remote_name"] = "https://remote_url.come/repo.git", -- full remote url
    --   ["remote2"] = "github_user/repo", -- GitHub user/repo shortcut,
    --   ["remote3"] = "github_user", -- GitHub user assume AstroNvim fork
    -- },
  },

  -- Set colorscheme to use
  colorscheme = "tokyonight-night",

  -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
  diagnostics = {
    virtual_text = true,
    underline = true,
  },

  -- LuaSnip Options
  -- luasnip = {
  --   -- Extend filetypes
  --   filetype_extend = {
  --     -- javascript = { "javascriptreact" },
  --   },
  --   -- Configure luasnip loaders (vscode, lua, and/or snipmate)
  --   vscode = {
  --     -- Add paths for including more VS Code style snippets in luasnip
  --     paths = {},
  --   },
  -- },

  -- Modify which-key registration (Use this with mappings table in the above.)
  ["which-key"] = {
    -- Add bindings which show up as group name
    register = {
      -- first key is the mode, n == normal mode
      n = {
        -- second key is the prefix, <leader> prefixes
        ["<leader>"] = {
          -- third key is the key to bring up next level and its displayed
          -- group name in which-key top level menu
          ["b"] = { name = "Buffer" },
          ["w"] = { name = "Workspace" },
          ["r"] = { name = "Rust" },
          ["l"] = { name = "lsp" },
        },
      },
    },
  },

  polish = function()
    -- vim.cmd [[
    --     set background=dark
    --     if exists("g:neovide")
    --         " Put anything you want to happen only in Neovide here
    --         let g:neovide_cursor_animation_length=0.05
    --         set guifont=Iosevka\ Term:h8
    --     endif
    --  ]]
  end,
}

return config
