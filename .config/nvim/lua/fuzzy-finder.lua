require('telescope').setup {
  defaults = {
    layout_strategy = 'vertical'
  },
  pickers = {
    find_files = {
      hidden = true,
      previewer = false
    }
  }
}
