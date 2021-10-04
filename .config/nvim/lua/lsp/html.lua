return function (on_attach)
  return {
    on_attach = function (client, bufnr)
      client.resolved_capabilities.document_formatting = false
      client.resolved_capabilities.document_range_formatting = false

      on_attach(client, bufnr)
    end
  }
end
