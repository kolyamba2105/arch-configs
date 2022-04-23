local cmp = require('cmp')
local snip = require('luasnip')

local config = {
  formatting = {
    format = function(entry, vim_item)
      local source_names = {
        buffer = "[Buffer]",
        luasnip = "[Snippet]",
        nvim_lsp = "[LSP]",
      }
      vim_item.menu = source_names[entry.source.name]

      return vim_item
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-Space>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-e>'] = cmp.mapping.close(),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-j>'] = cmp.mapping.select_next_item(),
    ['<C-k>'] = cmp.mapping.select_prev_item(),
    ['<C-y>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif snip.expand_or_jumpable() then
        snip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif snip.jumpable(-1) then
        snip.jump(-1)
      else
        fallback()
      end
    end,
  }),
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'path' },
    { name = 'buffer' },
  },
  snippet = { expand = function(args) snip.lsp_expand(args.body) end },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
}

cmp.setup(config)
