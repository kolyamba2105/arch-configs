_G.organize_imports = function ()
  local bufnr = vim.api.nvim_get_current_buf()

  local params = {
    command = '_typescript.organizeImports',
    arguments = {
      vim.api.nvim_buf_get_name(bufnr)
    }
  }

  vim.lsp.buf.execute_command(params)
end

return function (on_attach) 
  return {
    on_attach = function (client, bufnr)
      on_attach(client, bufnr)
      vim.cmd('command! Organize lua organize_imports()')
      client.resolved_capabilities.document_formatting = false
    end
  }
end
