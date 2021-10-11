_G.prettier_config = function()
  local buffer_name = vim.api.nvim_buf_get_name(0)

  local prettier_bin = function ()
    local lsp_util = require('lspconfig/util')

    local root_dir = lsp_util.root_pattern('tsconfig.json', 'package.json')
    local local_bin = lsp_util.path.join(root_dir(buffer_name), 'node_modules', '.bin', 'prettier')

    if lsp_util.path.exists(local_bin) then
        return local_bin
    else
        return 'prettier'
    end
  end

  return {
    exe = prettier_bin(),
    args = { '--stdin-filepath', vim.fn.fnameescape(buffer_name) },
    stdin = true
  }
end

require('formatter').setup {
  filetype = {
    elm = {
      function () return { exe = 'elm-format', args = { '--stdin' }, stdin = true } end
    },
    haskell = {
      function () return { exe = 'stylish-haskell', stdin = true } end
    },
    javascript = { prettier_config },
    javascriptreact = { prettier_config },
    json = { prettier_config },
    typescript = { prettier_config },
    typescriptreact = { prettier_config },
  }
}

vim.api.nvim_command [[augroup FormatGroup]]
vim.api.nvim_command [[autocmd!]]
vim.api.nvim_command [[autocmd BufWritePost *.elm,*.hs,*.js,*.json,*.ts,*.tsx FormatWrite]]
vim.api.nvim_command [[augroup END]]
