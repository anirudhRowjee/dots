-- AstroCore provides a central place to modify mappings, vim options, autocommands, and more!
-- Configuration documentation can be found with `:h astrocore`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- Configure core features of AstroNvim
    features = {
      large_buf = { size = 1024 * 256, lines = 10000 }, -- set global limits for large files for disabling features like treesitter
      autopairs = true, -- enable autopairs at start
      cmp = true, -- enable completion at start
      diagnostics_mode = 3, -- diagnostic mode on start (0 = off, 1 = no signs/virtual text, 2 = no virtual text, 3 = on)
      highlighturl = true, -- highlight URLs at start
      notifications = true, -- enable notifications at start
    },
    -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
    diagnostics = {
      virtual_text = true,
      underline = true,
    },
    -- vim options can be configured here
    options = {
      opt = { -- vim.opt.<key>
        relativenumber = true, -- sets vim.opt.relativenumber
        number = true, -- sets vim.opt.number
        spell = false, -- sets vim.opt.spell
        signcolumn = "auto", -- sets vim.opt.signcolumn to auto
        wrap = true, -- sets vim.opt.wrap
        linebreak = true, -- sets vim.opt.wrap
        cursorline = false,
        laststatus = 2,
      },
      g = { -- vim.g.<key>
        -- configure global vim variables (vim.g)
        -- NOTE: `mapleader` and `maplocalleader` must be set in the AstroNvim opts or before `lazy.setup`
        -- This can be found in the `lua/lazy_setup.lua` file
      },
    },
    -- Mappings can be configured through AstroCore as well.
    -- NOTE: keycodes follow the casing in the vimdocs. For example, `<Leader>` must be capitalized
    mappings = {
      -- first key is the mode
      n = {
        -- second key is the lefthand side of the map

        -- navigate buffer tabs
        ["]b"] = { function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
        ["[b"] = { function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },

        -- mappings seen under group name "Buffer"
        ["<Leader>bd"] = {
          function()
            require("astroui.status.heirline").buffer_picker(
              function(bufnr) require("astrocore.buffer").close(bufnr) end
            )
          end,
          desc = "Close buffer from tabline",
        },

        -- Custom mappings begin here!
        ["<leader><leader>"] = { "<cmd>Telescope commands<cr>", desc = "List all commands" },
        ["<leader>wo"] = { "<cmd>WorkspacesOpen<cr>", desc = "Open a workspace" },
        ["<leader>wa"] = { "<cmd>WorkspacesAdd<cr>", desc = "Add a workspace" },
        ["<leader>rr"] = { "<cmd>Cargo run<cr>", desc = "Run the current rust project" },
        ["<leader>rt"] = { "<cmd>RustTest!<cr>", desc = "Test the current rust project" },
        ["<leader>Fb"] = { "<cmd>set guifont=Iosevka\\ Term:h12<cr>", desc = "Make the Font larger" },
        ["<leader>Fs"] = { "<cmd>set guifont=Iosevka\\ Term:h8<cr>", desc = "Test the Font smaller" },
        ["<C-\\>"] = { ":ToggleTerm<cr>", desc = "Toggle Terminal" }, -- change description but the same command
        -- quick save
        -- ["<C-s>"] = { ":w!<cr>", desc = "Save File" },  -- change description but the same command

        -- LSP Mappings with Glance and Trouble
        ["<leader>lR"] = { "<cmd>Glance references<cr>", desc = "Glance references" },
        ["<leader>ld"] = { "<cmd>Glance definitions<cr>", desc = "Glance Definitions" },
        ["<leader>lq"] = { "<cmd>TroubleToggle<cr>", desc = "Trouble Quickfix List" },
        -- tables with just a `desc` key will be registered with which-key if it's installed
        -- this is useful for naming menus
        -- ["<Leader>b"] = { desc = "Buffers" },

        -- setting a mapping to false will disable it
        -- ["<C-S>"] = false,
        ["<leader>lgr"] = { "<cmd>Glance references<cr>", desc = "Glance references" },
        ["<leader>lgd"] = { "<cmd>Glance definitions<cr>", desc = "Glance Definitions" },
        -- ["<leader>lq"] = { "<cmd>TroubleToggle<cr>", desc = "Trouble Quickfix List" },
        ["<leader>lw"] = { "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "LSP Workspace Symbols" },
        ["<leader>tt"] = { "<cmd>TroubleToggle<cr>", desc = "Toggle Trouble Quickfix List" },
        ["<Tab>"] = { "<cmd>tabnext<cr>", desc = "Next Tab" },
        ["<S-Tab>"] = { "<cmd>tabprev<cr>", desc = "Previous Tab" },

        -- LSP mappings with tests
        ["<leader>ltm"] = { "<cmd>lua require('neotest').run.run()<CR>", desc = "Test Function under Cursor" },
        ["<leader>ltf"] = { "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<CR>", desc = "Test Current File" },
        ["<leader>lta"] = { "<cmd>lua require('neotest').run.run(vim.fn.getcwd())<CR>", desc = "Test Project" },
        ["<leader>lts"] = { "<cmd>lua require('neotest').summary.toggle()<CR>", desc = "Toggle Test Summary" },
        ["<leader>ltq"] = { "<cmd>lua require('neotest').run.stop()<CR>", desc = "Stop Test" },
      },

      t = {
        ["<esc>"] = { "<C-\\><C-n>", desc = "Exit term" },
      },
      i = {
        ["jj"] = false,
      },
    },
  },
}
