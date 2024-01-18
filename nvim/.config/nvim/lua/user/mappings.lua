local mappings = {
  -- first key is the mode
  n = {
    -- second key is the lefthand side of the map
    -- mappings seen under group name "Buffer"
    ["<leader>bb"] = { "<cmd>tabnew<cr>", desc = "New tab" },
    ["<leader>bs"] = { "<cmd>w<cr>", desc = "Save File" },
    ["<leader>bc"] = { "<cmd>BufferLinePickClose<cr>", desc = "Pick to close" },
    ["<leader>bj"] = { "<cmd>BufferLinePick<cr>", desc = "Pick to jump" },
    ["<leader>bt"] = { "<cmd>BufferLineSortByTabs<cr>", desc = "Sort by tabs" },

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
    ["<leader>lgr"] = { "<cmd>Glance references<cr>", desc = "Glance references" },
    ["<leader>lgd"] = { "<cmd>Glance definitions<cr>", desc = "Glance Definitions" },
    ["<leader>lq"] = { "<cmd>TroubleToggle<cr>", desc = "Trouble Quickfix List" },
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
    -- setting a mapping to false will disable it
    -- ["<esc>"] = false,
    ["<esc>"] = { "<C-\\><C-n>", desc = "Exit term" },
  },
  i = {
    ["jj"] = false,
  },
}

return mappings
