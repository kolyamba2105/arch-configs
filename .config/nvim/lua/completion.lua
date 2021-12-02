local cmp = require('cmp')
local snip = require('luasnip')

cmp.setup {
  mapping = {
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-e>'] = cmp.mapping.close(),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-y>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
    ['<Tab>'] = function (fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif snip.expand_or_jumpable() then
        snip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function (fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif snip.jumpable(-1) then
        snip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'path' },
    { name = 'buffer' },
  },
  snippet = { expand = function(args) snip.lsp_expand(args.body) end },
}
