return function (on_attach)
  return {
    on_attach = function (client, bufnr)
      on_attach(client, bufnr)
      client.resolved_capabilities.document_formatting = false
    end
  }
end
