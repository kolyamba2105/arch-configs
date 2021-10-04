_G.organize_imports = function ()
  vim.lsp.buf.execute_command {
    command = '_typescript.organizeImports',
    arguments = {
      vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
    }
  }
end

return function (on_attach)
  return {
    on_attach = function (client, bufnr)
      client.resolved_capabilities.document_formatting = false
      client.resolved_capabilities.document_range_formatting = false

      vim.cmd('command! Organize lua organize_imports()')

      on_attach(client, bufnr)
    end
  }
end
