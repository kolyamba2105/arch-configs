return {
  default_config = {
    cmd = { 'elm-language-server' },
    filetypes = { 'elm' },
    root_dir = require('lspconfig/util').root_pattern('elm.json')
  }
}
