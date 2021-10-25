local cmp = require('cmp')

cmp.setup {
  mapping = {
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-e>'] = cmp.mapping.close(),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-y>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'path' },
    { name = 'buffer' },
  },
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
}

