local M = {}

M.buf_set_keymap = function(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
M.buf_set_option = function(...) vim.api.nvim_buf_set_option(bufnr, ...) end

return M
